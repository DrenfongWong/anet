--
--  Copyright (C) 2012 secunet Security Networks AG
--  Copyright (C) 2012 Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2012 Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software; you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2 of the License, or (at your
--  option) any later version.  See <http://www.fsf.org/copyleft/gpl.txt>.
--
--  This program is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  As a special exception, if other files instantiate generics from this
--  unit,  or  you  link  this  unit  with  other  files  to  produce  an
--  executable   this  unit  does  not  by  itself  cause  the  resulting
--  executable to  be  covered by the  GNU General  Public License.  This
--  exception does  not  however  invalidate  any  other reasons why  the
--  executable file might be covered by the GNU Public License.
--

with Interfaces.C;

with Anet.Constants;
with Anet.Sockets.Thin;
with Anet.Byte_Swapping;
with Anet.Net_Ifaces;

package body Anet.Sockets.Inet is

   package C renames Interfaces.C;

   procedure Bind
     (Socket    :     Integer;
      Sock_Addr :     Thin.Sockaddr_In_Type;
      Iface     :     Types.Iface_Name_Type := "";
      Success   : out Boolean);
   --  Bind given socket to the specified sockaddr and set reuse address flag.
   --  If an interface name is given, the socket is bound to it. Success is set
   --  to True if the operation was successful, False if not.

   procedure Receive
     (Socket :     Integer;
      Src    : out Thin.Sockaddr_In_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Receive data from given inet socket. This procedure blocks until data
   --  has been received. Last is the index value such that Item (Last) is the
   --  last character assigned. An exception is raised if a socket error
   --  occurs or if no address information could be retrieved from the
   --  underlying protocol.

   function Create_Inet4
     (Address : IPv4_Addr_Type;
      Port    : Port_Type)
      return Thin.Sockaddr_In_Type;
   --  Create inet4 sockaddr type from given address and port.

   function Create_Inet6
     (Address : IPv6_Addr_Type;
      Port    : Port_Type)
      return Thin.Sockaddr_In_Type;
   --  Create inet6 sockaddr type from given address and port.

   -------------------------------------------------------------------------

   procedure Accept_Connection
     (Socket     :     TCPv4_Socket_Type;
      New_Socket : out TCPv4_Socket_Type)
   is
      use type C.int;

      Res  : C.int;
      Sock : Thin.Sockaddr_In_Type (Family => Family_Inet);
      Len  : aliased C.int := Sock'Size / 8;
   begin
      Res := Thin.C_Accept (S       => C.int (Socket.Sock_FD),
                            Name    => Sock'Address,
                            Namelen => Len'Access);

      if Res = C_Failure then
         raise Socket_Error with "Unable to accept connection on TCPv4 "
           & "socket - " & Get_Errno_String;
      end if;

      New_Socket.Sock_FD := Integer (Res);
   end Accept_Connection;

   -------------------------------------------------------------------------

   procedure Accept_Connection
     (Socket     :     TCPv6_Socket_Type;
      New_Socket : out TCPv6_Socket_Type)
   is
      use type C.int;

      Res  : C.int;
      Sock : Thin.Sockaddr_In_Type (Family => Family_Inet6);
      Len  : aliased C.int := Sock'Size / 8;
   begin
      Res := Thin.C_Accept (S       => C.int (Socket.Sock_FD),
                            Name    => Sock'Address,
                            Namelen => Len'Access);

      if Res = C_Failure then
         raise Socket_Error with "Unable to accept connection on TCPv6 "
           & "socket - " & Get_Errno_String;
      end if;

      New_Socket.Sock_FD := Integer (Res);
   end Accept_Connection;

   -------------------------------------------------------------------------

   procedure Bind
     (Socket    :     Integer;
      Sock_Addr :     Thin.Sockaddr_In_Type;
      Iface     :     Types.Iface_Name_Type := "";
      Success   : out Boolean)
   is
      use type C.int;

      Res : C.int;
   begin
      Thin.Set_Socket_Option
        (Socket => Socket,
         Option => Reuse_Address,
         Value  => True);

      Res := Thin.C_Bind (S       => C.int (Socket),
                          Name    => Sock_Addr'Address,
                          Namelen => Sock_Addr'Size / 8);
      Success := Res /= C_Failure;
      if not Success then
         return;
      end if;

      if Iface'Length /= 0 then
         Thin.Set_Socket_Option
           (Socket => Socket,
            Level  => Thin.Socket_Level,
            Option => Bind_To_Device,
            Value  => String (Iface));
      end if;
   end Bind;

   -------------------------------------------------------------------------

   procedure Bind
     (Socket  : in out IPv4_Socket_Type;
      Address :        IPv4_Addr_Type        := Any_Addr;
      Port    :        Port_Type;
      Iface   :        Types.Iface_Name_Type := "")
   is
      Result   : Boolean;
      Sockaddr : constant Thin.Sockaddr_In_Type
        := Create_Inet4 (Address => Address,
                         Port    => Port);
   begin
      Bind (Socket    => Socket.Sock_FD,
            Sock_Addr => Sockaddr,
            Iface     => Iface,
            Success   => Result);

      if not Result then
         raise Socket_Error with "Unable to bind socket to "
           & To_String (Address => Address) & "," & Port'Img & " - "
           & Get_Errno_String;
      end if;
   end Bind;

   -------------------------------------------------------------------------

   procedure Bind
     (Socket  : in out IPv6_Socket_Type;
      Address :        IPv6_Addr_Type        := Any_Addr_V6;
      Port    :        Port_Type;
      Iface   :        Types.Iface_Name_Type := "")
   is
      Result   : Boolean;
      Sockaddr : constant Thin.Sockaddr_In_Type
        := Create_Inet6 (Address => Address,
                         Port    => Port);
   begin
      Bind (Socket    => Socket.Sock_FD,
            Sock_Addr => Sockaddr,
            Iface     => Iface,
            Success   => Result);

      if not Result then
         raise Socket_Error with "Unable to bind socket to "
           & To_String (Address => Address) & "," & Port'Img & " - "
           & Get_Errno_String;
      end if;
   end Bind;

   -------------------------------------------------------------------------

   procedure Connect
     (Socket  : in out TCPv4_Socket_Type;
      Address :        IPv4_Addr_Type;
      Port    :        Port_Type)
   is
      use type C.int;

      Res : C.int;
      Dst : constant Thin.Sockaddr_In_Type := Create_Inet4
        (Address => Address,
         Port    => Port);
   begin
      Res := Thin.C_Connect (S       => C.int (Socket.Sock_FD),
                             Name    => Dst'Address,
                             Namelen => Dst'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable to connect socket to address "
           & To_String (Address => Address) & " (" & Port'Img & " ) - "
           & Get_Errno_String;
      end if;
   end Connect;

   -------------------------------------------------------------------------

   procedure Connect
     (Socket  : in out TCPv6_Socket_Type;
      Address :        IPv6_Addr_Type;
      Port    :        Port_Type)
   is
      use type C.int;

      Res : C.int;
      Dst : constant Thin.Sockaddr_In_Type := Create_Inet6
        (Address => Address,
         Port    => Port);
   begin
      Res := Thin.C_Connect (S       => C.int (Socket.Sock_FD),
                             Name    => Dst'Address,
                             Namelen => Dst'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable to connect socket to address "
           & To_String (Address => Address) & " (" & Port'Img & " ) - "
           & Get_Errno_String;
      end if;
   end Connect;

   -------------------------------------------------------------------------

   function Create_Inet4
     (Address : IPv4_Addr_Type;
      Port    : Port_Type)
      return Thin.Sockaddr_In_Type
   is
   begin
      return (Family     => Family_Inet,
              Sin_Family => Constants.Sys.AF_INET,
              Sin_Port   => C.unsigned_short
                (Byte_Swapping.Host_To_Network (Input => Port)),
              Sin_Addr   => Address,
              Sin_Zero   => <>);
   end Create_Inet4;

   -------------------------------------------------------------------------

   function Create_Inet6
     (Address : IPv6_Addr_Type;
      Port    : Port_Type)
      return Thin.Sockaddr_In_Type
   is
   begin
      return (Family     => Family_Inet6,
              Sin_Family => Constants.Sys.AF_INET6,
              Sin_Port   => C.unsigned_short
                (Byte_Swapping.Host_To_Network (Input => Port)),
              Sin6_Addr  => Address,
              others     => 0);
   end Create_Inet6;

   -------------------------------------------------------------------------

   procedure Init (Socket : in out UDPv4_Socket_Type)
   is
   begin
      Init (Socket => Socket,
            Family => Family_Inet,
            Mode   => Datagram_Socket);
   end Init;

   -------------------------------------------------------------------------

   procedure Init (Socket : in out TCPv4_Socket_Type)
   is
   begin
      Init (Socket => Socket,
            Family => Family_Inet,
            Mode   => Stream_Socket);
   end Init;

   -------------------------------------------------------------------------

   procedure Init (Socket : in out UDPv6_Socket_Type)
   is
   begin
      Init (Socket => Socket,
            Family => Family_Inet6,
            Mode   => Datagram_Socket);
   end Init;

   -------------------------------------------------------------------------

   procedure Init (Socket : in out TCPv6_Socket_Type)
   is
   begin
      Init (Socket => Socket,
            Family => Family_Inet6,
            Mode   => Stream_Socket);
   end Init;

   -------------------------------------------------------------------------

   procedure Join_Multicast_Group
     (Socket : UDPv4_Socket_Type;
      Group  : IPv4_Addr_Type;
      Iface  : Types.Iface_Name_Type := "")
   is
      use type C.int;
      use type C.unsigned_short;

      Mreq      : Thin.IPv4_Mreq_Type;
      Iface_Idx : Natural := 0;
      Res       : C.int;
   begin
      if Iface'Length > 0 then
         Iface_Idx := Net_Ifaces.Get_Iface_Index (Name => Iface);
      end if;

      Mreq.Imr_Multiaddr := Group;
      Mreq.Imr_Interface := C.unsigned (Iface_Idx);

      Res := Thin.C_Setsockopt
        (S       => C.int (Socket.Sock_FD),
         Level   => Constants.Sys.IPPROTO_IP,
         Optname => Constants.Sys.IP_ADD_MEMBERSHIP,
         Optval  => Mreq'Address,
         Optlen  => Mreq'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable to join multicast group "
           & To_String (Address => Group) & ": " & Get_Errno_String;
      end if;
   end Join_Multicast_Group;

   -------------------------------------------------------------------------

   procedure Join_Multicast_Group
     (Socket : UDPv6_Socket_Type;
      Group  : IPv6_Addr_Type;
      Iface  : Types.Iface_Name_Type := "")
   is
      use type C.int;
      use type C.unsigned_short;

      Mreq6     : Thin.IPv6_Mreq_Type;
      Iface_Idx : Natural := 0;
      Res       : C.int;
   begin
      if Iface'Length > 0 then
         Iface_Idx := Net_Ifaces.Get_Iface_Index (Name => Iface);
      end if;

      Mreq6.IPv6mr_Multiaddr := Group;
      Mreq6.IPv6mr_Interface := C.unsigned (Iface_Idx);

      Res := Thin.C_Setsockopt
        (S       => C.int (Socket.Sock_FD),
         Level   => Constants.IPPROTO_IPV6,
         Optname => Constants.IPV6_ADD_MEMBERSHIP,
         Optval  => Mreq6'Address,
         Optlen  => Mreq6'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable to join multicast group "
           & To_String (Address => Group) & ": " & Get_Errno_String;
      end if;
   end Join_Multicast_Group;

   -------------------------------------------------------------------------

   procedure Receive
     (Socket :     Integer;
      Src    : out Thin.Sockaddr_In_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      use type Interfaces.C.int;
      use type Ada.Streams.Stream_Element_Offset;

      Res : C.int;
      Len : aliased C.int := Src'Size / 8;
   begin
      Res := Thin.C_Recvfrom (S       => C.int (Socket),
                              Msg     => Item'Address,
                              Len     => Item'Length,
                              Flags   => 0,
                              From    => Src'Address,
                              Fromlen => Len'Access);

      if Res = C_Failure then
         raise Socket_Error with "Error receiving data: " & Get_Errno_String;
      end if;
      if Len = 0 then
         raise Socket_Error with "No address information received";
      end if;

      Last := Item'First + Ada.Streams.Stream_Element_Offset (Res - 1);
   end Receive;

   -------------------------------------------------------------------------

   procedure Receive
     (Socket :     UDPv4_Socket_Type;
      Src    : out UDPv4_Sockaddr_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      Sockaddr : Thin.Sockaddr_In_Type (Family => Family_Inet);
   begin
      Receive (Socket => Socket.Sock_FD,
               Src    => Sockaddr,
               Item   => Item,
               Last   => Last);

      Src.Addr := Sockaddr.Sin_Addr;
      Src.Port := Port_Type (Sockaddr.Sin_Port);
   end Receive;

   -------------------------------------------------------------------------

   procedure Receive
     (Socket :     UDPv6_Socket_Type;
      Src    : out UDPv6_Sockaddr_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      Sockaddr : Thin.Sockaddr_In_Type (Family => Family_Inet6);
   begin
      Receive (Socket => Socket.Sock_FD,
               Src    => Sockaddr,
               Item   => Item,
               Last   => Last);

      Src.Addr := Sockaddr.Sin6_Addr;
      Src.Port := Port_Type (Sockaddr.Sin_Port);
   end Receive;

   -------------------------------------------------------------------------

   procedure Send
     (Socket   : IPv4_Socket_Type;
      Item     : Ada.Streams.Stream_Element_Array;
      Dst_Addr : IPv4_Addr_Type;
      Dst_Port : Port_Type)
   is
      use type Interfaces.C.int;
      use type Ada.Streams.Stream_Element_Offset;

      Res        : C.int;
      Sent_Bytes : Ada.Streams.Stream_Element_Offset;
      Dst        : constant Thin.Sockaddr_In_Type := Create_Inet4
        (Address => Dst_Addr,
         Port    => Dst_Port);
   begin
      Res := Thin.C_Sendto (S     => C.int (Socket.Sock_FD),
                            Buf   => Item'Address,
                            Len   => Item'Length,
                            Flags => 0,
                            To    => Dst'Address,
                            Tolen => Dst'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Error sending data to "
           & To_String (Address => Dst_Addr) & "," & Dst_Port'Img & " - "
           & Get_Errno_String;
      end if;

      Sent_Bytes := Item'First + Ada.Streams.Stream_Element_Offset (Res - 1);
      if Sent_Bytes /= Item'Length then
         raise Socket_Error with "Incomplete send operation to "
           & To_String (Address => Dst_Addr) & "," & Dst_Port'Img & " - only"
           & Sent_Bytes'Img & " of" & Item'Length'Img & " bytes sent";
      end if;
   end Send;

   -------------------------------------------------------------------------

   procedure Send
     (Socket   : IPv6_Socket_Type;
      Item     : Ada.Streams.Stream_Element_Array;
      Dst_Addr : IPv6_Addr_Type;
      Dst_Port : Port_Type)
   is
      use type Interfaces.C.int;
      use type Ada.Streams.Stream_Element_Offset;

      Res        : C.int;
      Sent_Bytes : Ada.Streams.Stream_Element_Offset;
      Dst        : constant Thin.Sockaddr_In_Type := Create_Inet6
        (Address => Dst_Addr,
         Port    => Dst_Port);
   begin
      Res := Thin.C_Sendto (S     => C.int (Socket.Sock_FD),
                            Buf   => Item'Address,
                            Len   => Item'Length,
                            Flags => 0,
                            To    => Dst'Address,
                            Tolen => Dst'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Error sending data to "
           & To_String (Address => Dst_Addr) & "," & Dst_Port'Img & " - "
           & Get_Errno_String;
      end if;

      Sent_Bytes := Item'First + Ada.Streams.Stream_Element_Offset (Res - 1);
      if Sent_Bytes /= Item'Length then
         raise Socket_Error with "Incomplete send operation to "
           & To_String (Address => Dst_Addr) & "," & Dst_Port'Img & " - only"
           & Sent_Bytes'Img & " of" & Item'Length'Img & " bytes sent";
      end if;
   end Send;

end Anet.Sockets.Inet;
