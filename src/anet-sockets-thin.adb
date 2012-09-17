--
--  Copyright (C) 2011, 2012 secunet Security Networks AG
--  Copyright (C) 2011, 2012 Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2011, 2012 Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Anet.Constants;
with Anet.Byte_Swapping;
with Anet.Sockets.Thin.Inet;

package body Anet.Sockets.Thin is

   package C renames Interfaces.C;

   Families : constant array (Family_Type) of C.int
     := (Family_Inet   => Constants.Sys.AF_INET,
         Family_Inet6  => Constants.Sys.AF_INET6,
         Family_Packet => Constants.AF_PACKET,
         Family_Unix   => Constants.AF_UNIX);
   --  Address family mapping.

   Levels : constant array (Level_Type) of C.int
     := (Socket_Level => Constants.Sys.SOL_SOCKET);
   --  Protocol level mapping.

   Options_Bool : constant array (Option_Name_Bool) of C.int
     := (Reuse_Address => Constants.Sys.SO_REUSEADDR,
         Broadcast     => Constants.Sys.SO_BROADCAST);
   --  Mapping for option names with boolean value.

   Options_Str : constant array (Option_Name_Str) of C.int
     := (Bind_To_Device => Constants.SO_BINDTODEVICE);
   --  Mapping for option names with string value.

   Modes : constant array (Mode_Type) of C.int
     := (Stream_Socket   => Constants.Sys.SOCK_STREAM,
         Datagram_Socket => Constants.Sys.SOCK_DGRAM);
   --  Socket mode mapping.

   Get_Requests : constant array (Netdev_Request_Name) of C.int
     := (If_Addr   => Constants.SIOCGIFADDR,
         If_Flags  => Constants.SIOCGIFFLAGS,
         If_Hwaddr => Constants.SIOCGIFHWADDR,
         If_Index  => Constants.SIOCGIFINDEX);
   --  Currently supported netdevice ioctl get requests.

   Set_Requests : constant array (Netdev_Request_Name) of C.int
     := (If_Flags => Constants.SIOCSIFFLAGS,
         others   => C.int (-1));
   --  Currently supported netdevice ioctl set requests.

   type Sockaddr_Type is record
      Sa_Family : C.unsigned_short;
      --  Address family
      Sa_Data   : C.char_array (1 .. 14) := (others => C.nul);
      --  Family-specific data
   end record;
   pragma Convention (C, Sockaddr_Type);
   --  Generic socket address.

   type If_Req_Type (Name : Netdev_Request_Name := If_Index) is record
      Ifr_Name : C.char_array (1 .. Constants.IFNAMSIZ) := (others => C.nul);

      case Name is
         when If_Addr   =>
            Ifr_Addr    : Sockaddr_Type;
         when If_Hwaddr =>
            Ifr_Hwaddr  : Sockaddr_Type;
         when If_Index  =>
            Ifr_Ifindex : C.int   := 0;
         when If_Flags  =>
            Ifr_Flags   : C.short := 0;
      end case;
   end record;
   pragma Unchecked_Union (If_Req_Type);
   pragma Convention (C, If_Req_Type);
   --  Interface request structure (struct ifreq).

   function C_Send
     (S     : C.int;
      Buf   : System.Address;
      Len   : C.int;
      Flags : C.int)
      return C.int;
   pragma Import (C, C_Send, "send");

   function Ioctl_Get
     (Socket     : Integer;
      Request    : Netdev_Request_Name;
      Iface_Name : Types.Iface_Name_Type)
      return If_Req_Type;
   --  Execute netdevice ioctl get request on interface with given name. The
   --  specified socket must have been created beforehand. The procedure
   --  returns the resulting interface request record to the caller.

   procedure Ioctl
     (Socket  : Integer;
      Request : C.int;
      If_Req  : not null access If_Req_Type);
   --  Execute netdevice ioctl request on interface with given name and request
   --  type. The specified socket must have been created beforehand.

   function Query_Iface
     (Iface_Name : Types.Iface_Name_Type;
      Request    : Netdev_Request_Name)
      return If_Req_Type;
   --  Query interface with given request.

   procedure Join_Multicast_Group_V4
     (Socket : Integer;
      Group  : Socket_Addr_Type;
      Iface  : Types.Iface_Name_Type := "");
   --  Join the given IPv4 multicast group on the interface specified by name.

   procedure Join_Multicast_Group_V6
     (Socket : Integer;
      Group  : Socket_Addr_Type;
      Iface  : Types.Iface_Name_Type := "");
   --  Join the given IPv6 multicast group on the interface specified by name.

   -------------------------------------------------------------------------

   procedure Accept_Socket
     (Socket       :     Integer;
      Sockaddr     :     System.Address;
      Sockaddr_Len :     Integer;
      New_Socket   : out Integer)
   is
      use type C.int;

      function C_Accept
        (S       : C.int;
         Name    : System.Address;
         Namelen : not null access C.int)
         return C.int;
      pragma Import (C, C_Accept, "accept");

      Res : C.int;
      Len : aliased C.int := C.int (Sockaddr_Len);
   begin
      Res := C_Accept (S       => C.int (Socket),
                       Name    => Sockaddr,
                       Namelen => Len'Access);

      if Res = C_Failure then
         raise Socket_Error with "Unable to accept connection on socket - "
           & Get_Errno_String;
      end if;

      New_Socket := Integer (Res);
   end Accept_Socket;

   -------------------------------------------------------------------------

   procedure Close_Socket (Socket : Integer)
   is
      use type C.int;

      function C_Close (Fd : C.int) return C.int;
      pragma Import (C, C_Close, "close");

      Res : C.int;
   begin
      Res := C_Close (C.int (Socket));

      if Res = C_Failure then
         raise Socket_Error with "Unable to close socket: " & Get_Errno_String;
      end if;
   end Close_Socket;

   -------------------------------------------------------------------------

   procedure Connect_Socket
     (Socket : Integer;
      Dst    : Socket_Addr_Type)
   is
      use type C.int;

      Res : C.int;
      Sin : constant Inet.Sockaddr_In_Type
        := Inet.To_Sock_Addr (Address => Dst);
   begin
      Res := C_Connect (S       => C.int (Socket),
                        Name    => Sin'Address,
                        Namelen => Sin'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable to connect socket to address "
           & To_String (Dst) & " - " & Get_Errno_String;
      end if;
   end Connect_Socket;

   -------------------------------------------------------------------------

   procedure Create_Socket
     (Socket : out Integer;
      Family :     Family_Type := Family_Inet;
      Mode   :     Mode_Type   := Datagram_Socket)
   is
      use type Interfaces.C.int;

      function C_Socket
        (Domain   : C.int;
         Typ      : C.int;
         Protocol : C.int)
         return C.int;
      pragma Import (C, C_Socket, "socket");

      Res   : C.int;
      Proto : C.int := 0;
   begin
      if Family = Family_Packet then
         Proto := C.int (Byte_Swapping.Host_To_Network
                         (Input => Double_Byte (Constants.ETH_P_IP)));
      end if;

      Res := C_Socket (Domain   => Families (Family),
                       Typ      => Modes (Mode),
                       Protocol => Proto);

      if Res = C_Failure then
         raise Socket_Error with "Unable to create socket (" & Family'Img & "/"
           & Mode'Img & "): " & Get_Errno_String;
      end if;

      Socket := Integer (Res);
   end Create_Socket;

   -------------------------------------------------------------------------

   function Get_Iface_Index (Name : Types.Iface_Name_Type) return Positive
   is
      Req : constant If_Req_Type := Query_Iface
        (Iface_Name => Name,
         Request    => If_Index);
   begin
      return Positive (Req.Ifr_Ifindex);
   end Get_Iface_Index;

   -------------------------------------------------------------------------

   function Get_Iface_IP (Name : Types.Iface_Name_Type) return IPv4_Addr_Type
   is
      Req : constant If_Req_Type := Query_Iface
        (Iface_Name => Name,
         Request    => If_Addr);

      --  The actual IP address is at range 3 .. 6. The first two bytes of the
      --  result are the sin_port value.

      Offset : constant := 2;
   begin
      return Result : IPv4_Addr_Type do
         for B in Result'Range loop
            Result (B) := Character'Pos
              (C.To_Ada
                 (Req.Ifr_Addr.Sa_Data
                    (C.size_t (B + Offset))));
         end loop;
      end return;
   end Get_Iface_IP;

   -------------------------------------------------------------------------

   function Get_Iface_Mac
     (Name : Types.Iface_Name_Type)
      return Hardware_Addr_Type
   is
      Req  : constant If_Req_Type := Query_Iface
        (Iface_Name => Name,
         Request    => If_Hwaddr);
   begin
      return Result : Hardware_Addr_Type (1 .. 6) do
         for B in Result'Range loop
            Result (B) := Character'Pos
              (C.To_Ada
                 (Req.Ifr_Hwaddr.Sa_Data
                    (C.size_t (B))));
         end loop;
      end return;
   end Get_Iface_Mac;

   -------------------------------------------------------------------------

   procedure Ioctl
     (Socket  : Integer;
      Request : C.int;
      If_Req  : not null access If_Req_Type)
   is
      use type C.int;

      function C_Ioctl
        (S    : C.int;
         Req  : C.int;
         Arg  : access If_Req_Type)
         return C.int;
      pragma Import (C, C_Ioctl, "ioctl");

      Ctl_Ret : C.int;
   begin
      Ctl_Ret := C_Ioctl
        (S   => C.int (Socket),
         Req => Request,
         Arg => If_Req);

      if Ctl_Ret = C_Failure then
         raise Socket_Error with "Ioctl (" & Request'Img
           & ") failed on interface '" & C.To_Ada (If_Req.Ifr_Name) & "': "
           & Get_Errno_String;
      end if;
   end Ioctl;

   -------------------------------------------------------------------------

   function Ioctl_Get
     (Socket     : Integer;
      Request    : Netdev_Request_Name;
      Iface_Name : Types.Iface_Name_Type)
      return If_Req_Type
   is
      C_Name  : constant C.char_array := C.To_C (String (Iface_Name));
      If_Req  : aliased If_Req_Type (Name => Request);
   begin
      If_Req.Ifr_Name (1 .. C_Name'Length) := C_Name;

      Ioctl (Socket  => Socket,
             Request => Get_Requests (Request),
             If_Req  => If_Req'Access);

      return If_Req;
   end Ioctl_Get;

   -------------------------------------------------------------------------

   function Is_Iface_Up (Name : Types.Iface_Name_Type) return Boolean
   is
      use type Interfaces.C.short;

      Req : constant If_Req_Type := Query_Iface
        (Iface_Name => Name,
         Request    => If_Flags);
   begin
      return (Req.Ifr_Flags mod 2) = Constants.IFF_UP;
   end Is_Iface_Up;

   -------------------------------------------------------------------------

   procedure Join_Multicast_Group
     (Socket : Integer;
      Group  : Socket_Addr_Type;
      Iface  : Types.Iface_Name_Type := "")
   is
   begin
      if Group.Family = Family_Inet then
         Join_Multicast_Group_V4 (Socket => Socket,
                                  Group  => Group,
                                  Iface  => Iface);
      elsif Group.Family = Family_Inet6 then
         Join_Multicast_Group_V6 (Socket => Socket,
                                  Group  => Group,
                                  Iface  => Iface);
      end if;
   end Join_Multicast_Group;

   -------------------------------------------------------------------------

   procedure Join_Multicast_Group_V4
     (Socket : Integer;
      Group  : Socket_Addr_Type;
      Iface  : Types.Iface_Name_Type := "")
   is
      use type Interfaces.C.int;

      type IP_Mreq_Type is record
         Imr_Multiaddr : IPv4_Addr_Type;
         Imr_Interface : C.unsigned;
      end record;
      pragma Convention (C, IP_Mreq_Type);
      --  struct ip_mreq (netinet/in.h).

      Mreq : IP_Mreq_Type
        := (Imr_Multiaddr => Group.Addr_V4,
            Imr_Interface => 0);
      Res  : C.int;
   begin
      if Iface'Length /= 0 then
         Mreq.Imr_Interface := C.unsigned (Get_Iface_Index (Name => Iface));
      end if;

      Res := C_Setsockopt
        (S       => C.int (Socket),
         Level   => Constants.Sys.IPPROTO_IP,
         Optname => Constants.Sys.IP_ADD_MEMBERSHIP,
         Optval  => Mreq'Address,
         Optlen  => Mreq'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable to join multicast group "
           & To_String (Address => Group) & ": " & Get_Errno_String;
      end if;
   end Join_Multicast_Group_V4;

   -------------------------------------------------------------------------

   procedure Join_Multicast_Group_V6
     (Socket : Integer;
      Group  : Socket_Addr_Type;
      Iface  : Types.Iface_Name_Type := "")
   is
      use type Interfaces.C.int;

      type IPv6_Mreq_Type is record
         IPv6mr_Multiaddr : IPv6_Addr_Type;
         IPv6mr_Interface : C.unsigned;
      end record;
      pragma Convention (C, IPv6_Mreq_Type);
      --  struct ipv6_mreq (netinet/in.h).

      Mreq : IPv6_Mreq_Type
        := (IPv6mr_Multiaddr => Group.Addr_V6,
            IPv6mr_Interface => 0);
      Res  : C.int;
   begin
      if Iface'Length /= 0 then
         Mreq.IPv6mr_Interface := C.unsigned (Get_Iface_Index (Name => Iface));
      end if;

      Res := C_Setsockopt
        (S       => C.int (Socket),
         Level   => Constants.IPPROTO_IPV6,
         Optname => Constants.IPV6_ADD_MEMBERSHIP,
         Optval  => Mreq'Address,
         Optlen  => Mreq'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable to join multicast group "
           & To_String (Address => Group) & ": " & Get_Errno_String;
      end if;
   end Join_Multicast_Group_V6;

   -------------------------------------------------------------------------

   procedure Listen_Socket
     (Socket  : Integer;
      Backlog : Positive := 1)
   is
      use type Interfaces.C.int;

      function C_Listen
        (Socket  : C.int;
         Backlog : C.int)
         return C.int;
      pragma Import (C, C_Listen, "listen");

      Res : C.int;
   begin
      Res := C_Listen (Socket  => C.int (Socket),
                       Backlog => C.int (Backlog));

      if Res = C_Failure then
         raise Socket_Error with "Unable to listen on socket with backlog"
           & Backlog'Img & " - " & Get_Errno_String;
      end if;
   end Listen_Socket;

   -------------------------------------------------------------------------

   function Query_Iface
     (Iface_Name : Types.Iface_Name_Type;
      Request    : Netdev_Request_Name)
      return If_Req_Type
   is
      Req  : If_Req_Type;
      Sock : Integer := -1;
   begin
      Create_Socket (Socket => Sock);

      begin
         Req := Ioctl_Get (Socket     => Sock,
                           Request    => Request,
                           Iface_Name => Iface_Name);

      exception
         when Socket_Error =>
            Close_Socket (Socket => Sock);
            raise;
      end;
      Close_Socket (Socket => Sock);

      return Req;
   end Query_Iface;

   -------------------------------------------------------------------------

   procedure Send_Socket
     (Socket :     Integer;
      Data   :     Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      use type C.int;
      use type Ada.Streams.Stream_Element_Offset;

      Res : C.int;
   begin
      Res := C_Send (S     => C.int (Socket),
                     Buf   => Data'Address,
                     Len   => Data'Length,
                     Flags => 0);

      if Res = C_Failure then
         raise Socket_Error with "Unable to send data on unix socket"
           & " - " & Get_Errno_String;
      end if;

      Last := Data'First + Ada.Streams.Stream_Element_Offset (Res - 1);
   end Send_Socket;

   -------------------------------------------------------------------------

   procedure Set_Iface_State
     (Name  : Types.Iface_Name_Type;
      State : Boolean)
   is
      Sock : Integer := -1;
   begin
      Create_Socket (Socket => Sock);

      declare
         C_Name : constant C.char_array := C.To_C (String (Name));
         Req    : aliased If_Req_Type (Name => If_Flags);
      begin
         Req.Ifr_Name (1 .. C_Name'Length) := C_Name;

         if State then
            Req.Ifr_Flags := Constants.IFF_UP;
         else
            Req.Ifr_Flags := 0;
         end if;

         Ioctl (Socket  => Sock,
                Request => Set_Requests (If_Flags),
                If_Req  => Req'Access);

      exception
         when Socket_Error =>
            Close_Socket (Socket => Sock);
            raise;
      end;
      Close_Socket (Socket => Sock);
   end Set_Iface_State;

   -------------------------------------------------------------------------

   procedure Set_Socket_Option
     (Socket : Integer;
      Level  : Level_Type := Socket_Level;
      Option : Option_Name_Bool;
      Value  : Boolean)
   is
      use type Interfaces.C.int;

      Val : C.int := C.int (Boolean'Pos (Value));
      Res : C.int;
   begin
      Res := C_Setsockopt
        (S       => C.int (Socket),
         Level   => Levels (Level),
         Optname => Options_Bool (Option),
         Optval  => Val'Address,
         Optlen  => Val'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable set socket option " & Option'Img
           & " to " & Value'Img & ": " & Get_Errno_String;
      end if;
   end Set_Socket_Option;

   -------------------------------------------------------------------------

   procedure Set_Socket_Option
     (Socket : Integer;
      Level  : Level_Type := Socket_Level;
      Option : Option_Name_Str;
      Value  : String)
   is
      use type Interfaces.C.int;

      Val : constant C.char_array := C.To_C (Value);
      Res : C.int;
   begin
      Res := C_Setsockopt
        (S       => C.int (Socket),
         Level   => Levels (Level),
         Optname => Options_Str (Option),
         Optval  => Val'Address,
         Optlen  => Val'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable set socket option " & Option'Img
           & " to '" & Value & "': " & Get_Errno_String;
      end if;
   end Set_Socket_Option;

end Anet.Sockets.Thin;
