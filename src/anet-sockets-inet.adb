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
with Anet.Sockets.Thin.Inet;
with Anet.Byte_Swapping;

package body Anet.Sockets.Inet is

   package C renames Interfaces.C;

   -------------------------------------------------------------------------

   procedure Accept_Connection
     (Socket     :     TCPv4_Socket_Type;
      New_Socket : out TCPv4_Socket_Type)
   is
      Sock_In   : Thin.Inet.Sockaddr_In_Type (Family => Family_Inet);
      Sock_Addr : System.Address;
      Sock_Len  : Integer := 0;
   begin
      New_Socket.Address := Socket.Address;
      Sock_Addr          := Sock_In'Address;
      Sock_Len           := Sock_In'Size / 8;

      Thin.Accept_Socket (Socket       => Socket.Sock_FD,
                          Sockaddr     => Sock_Addr,
                          Sockaddr_Len => Sock_Len,
                          New_Socket   => New_Socket.Sock_FD);
   end Accept_Connection;

   -------------------------------------------------------------------------

   procedure Accept_Connection
     (Socket     :     TCPv6_Socket_Type;
      New_Socket : out TCPv6_Socket_Type)
   is
      Sock_In   : Thin.Inet.Sockaddr_In_Type (Family => Family_Inet6);
      Sock_Addr : System.Address;
      Sock_Len  : Integer := 0;
   begin
      New_Socket.Address := Socket.Address;
      Sock_Addr          := Sock_In'Address;
      Sock_Len           := Sock_In'Size / 8;

      Thin.Accept_Socket (Socket       => Socket.Sock_FD,
                          Sockaddr     => Sock_Addr,
                          Sockaddr_Len => Sock_Len,
                          New_Socket   => New_Socket.Sock_FD);
   end Accept_Connection;

   -------------------------------------------------------------------------

   procedure Bind
     (Socket  : in out Inet_Socket_Type;
      Address :        Socket_Addr_Type      :=
        (Addr_V4 => Any_Addr, others => <>);
      Iface   :        Types.Iface_Name_Type := "")
   is
      Result : Boolean;
   begin
      Thin.Set_Socket_Option
        (Socket => Socket.Sock_FD,
         Option => Reuse_Address,
         Value  => True);

      Thin.Inet.Bind (Socket  => Socket.Sock_FD,
                      Address => Thin.Inet.To_Sock_Addr (Address),
                      Success => Result);

      if not Result then
         raise Socket_Error with "Unable to bind socket to "
           & To_String (Address => Address) & " - " & Get_Errno_String;
      end if;

      Socket.Address := Address;

      if Iface'Length /= 0 then
         Thin.Set_Socket_Option
           (Socket => Socket.Sock_FD,
            Level  => Thin.Socket_Level,
            Option => Bind_To_Device,
            Value  => String (Iface));
      end if;
   end Bind;

   -------------------------------------------------------------------------

   procedure Connect
     (Socket  : in out TCPv4_Socket_Type;
      Address :        IPv4_Addr_Type;
      Port    :        Port_Type)
   is
      Result : Boolean;
      Sin    : constant Thin.Inet.Sockaddr_In_Type
        := (Family     => Family_Inet,
            Sin_Family => Constants.Sys.AF_INET,
            Sin_Port   => C.unsigned_short
              (Byte_Swapping.Host_To_Network (Input => Port)),
            Sin_Addr   => Address,
            Sin_Zero   => <>);
   begin
      Thin.Inet.Connect (Socket  => Socket.Sock_FD,
                         Dst     => Sin,
                         Success => Result);

      if not Result then
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
      Result : Boolean;
      Sin    : constant Thin.Inet.Sockaddr_In_Type
        := (Family     => Family_Inet6,
            Sin_Family => Constants.Sys.AF_INET6,
            Sin_Port   => C.unsigned_short
              (Byte_Swapping.Host_To_Network (Input => Port)),
            Sin6_Addr  => Address,
            others     => 0);
   begin
      Thin.Inet.Connect (Socket  => Socket.Sock_FD,
                         Dst     => Sin,
                         Success => Result);

      if not Result then
         raise Socket_Error with "Unable to connect socket to address "
           & To_String (Address => Address) & " (" & Port'Img & " ) - "
           & Get_Errno_String;
      end if;
   end Connect;

   -------------------------------------------------------------------------

   function Create return UDPv4_Socket_Type
   is
   begin
      return Socket : UDPv4_Socket_Type do
         Create (Socket => Socket,
                 Family => Family_Inet,
                 Mode   => Datagram_Socket);
      end return;
   end Create;

   -------------------------------------------------------------------------

   function Create return TCPv4_Socket_Type
   is
   begin
      return Socket : TCPv4_Socket_Type do
         Create (Socket => Socket,
                 Family => Family_Inet,
                 Mode   => Stream_Socket);
      end return;
   end Create;

   -------------------------------------------------------------------------

   function Create return UDPv6_Socket_Type
   is
   begin
      return Socket : UDPv6_Socket_Type do
         Create (Socket => Socket,
                 Family => Family_Inet6,
                 Mode   => Datagram_Socket);
      end return;
   end Create;

   -------------------------------------------------------------------------

   function Create return TCPv6_Socket_Type
   is
   begin
      return Socket : TCPv6_Socket_Type do
         Create (Socket => Socket,
                 Family => Family_Inet6,
                 Mode   => Stream_Socket);
      end return;
   end Create;

   -------------------------------------------------------------------------

   procedure Receive
     (Socket :     UDPv4_Socket_Type;
      Src    : out UDPv4_Sockaddr_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      Sockaddr : Thin.Inet.Sockaddr_In_Type (Family => Family_Inet);
   begin
      Thin.Inet.Receive (Socket => Socket.Sock_FD,
                         Data   => Item,
                         Last   => Last,
                         Source => Sockaddr);
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
      Sockaddr : Thin.Inet.Sockaddr_In_Type (Family => Family_Inet6);
   begin
      Thin.Inet.Receive (Socket => Socket.Sock_FD,
                         Data   => Item,
                         Last   => Last,
                         Source => Sockaddr);
      Src.Addr := Sockaddr.Sin6_Addr;
      Src.Port := Port_Type (Sockaddr.Sin_Port);
   end Receive;

   -------------------------------------------------------------------------

   procedure Send
     (Socket : Inet_Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      Dst    : Socket_Addr_Type)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Result : Boolean;
      Len    : Ada.Streams.Stream_Element_Offset;
   begin
      Thin.Inet.Send (Socket  => Socket.Sock_FD,
                      Data    => Item,
                      Last    => Len,
                      Dst     => Thin.Inet.To_Sock_Addr (Address => Dst),
                      Success => Result);

      if not Result then
         raise Socket_Error with "Error sending data to "
           & To_String (Address => Dst) & " - " & Get_Errno_String;
      end if;

      if Len /= Item'Length then
         raise Socket_Error with "Incomplete send operation to "
           & To_String (Address => Dst) & ", only" & Len'Img & " of"
           & Item'Length'Img & " bytes sent";
      end if;
   end Send;

end Anet.Sockets.Inet;
