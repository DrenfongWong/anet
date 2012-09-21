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

with Ada.Streams;

with Interfaces.C;

with Anet.Constants;
with Anet.Types;

package Anet.Sockets.Thin is

   package C renames Interfaces.C;

   type Level_Type is (Socket_Level);
   --  Protocol level type.

   type Netdev_Request_Name is
     (If_Addr,
      If_Flags,
      If_Hwaddr,
      If_Index);
   --  Supported netdevice requests.

   type Sockaddr_Type is record
      Sa_Family : C.unsigned_short;
      --  Address family
      Sa_Data   : C.char_array (1 .. 14) := (others => C.nul);
      --  Family-specific data
   end record;
   pragma Convention (C, Sockaddr_Type);
   --  Generic socket address.

   type Sockaddr_Un_Type is record
      Sin_Family : Interfaces.C.unsigned_short := Constants.AF_UNIX;
      --  Address family
      Pathname   : Interfaces.C.char_array (1 .. Constants.UNIX_PATH_MAX)
        := (others => Interfaces.C.nul);
      --  Pathname
   end record;
   pragma Convention (C, Sockaddr_Un_Type);
   --  Low-level UNIX socket address type (struct sockaddr_un).

   type If_Req_Type (Name : Netdev_Request_Name := If_Index) is record
      Ifr_Name : Interfaces.C.char_array
        (1 .. Constants.IFNAMSIZ) := (others => C.nul);

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

   Set_Requests : constant array (Netdev_Request_Name) of C.int
     := (If_Flags => Constants.SIOCSIFFLAGS,
         others   => C.int (-1));
   --  Currently supported netdevice ioctl set requests.

   procedure Create_Socket
     (Socket : out Integer;
      Family :     Family_Type := Family_Inet;
      Mode   :     Mode_Type   := Datagram_Socket);
   --  Create a new communication socket with specified family and mode.

   procedure Close_Socket (Socket : Integer);
   --  Close given socket.

   procedure Listen_Socket
     (Socket  : Integer;
      Backlog : Positive := 1);
   --  Listen for specified amount of requests on given socket.

   procedure Accept_Socket
     (Socket       :     Integer;
      Sockaddr     :     System.Address;
      Sockaddr_Len :     Integer;
      New_Socket   : out Integer);
   --  Accept connection request from listening socket and return new connected
   --  socket. The Sockaddr argument must be an address to a low-level
   --  Sockaddr_In or Sockaddr_Un object matching the socket family.
   --  Sockaddr_Len is the size of the low-level sockaddr object (in bytes).

   procedure Send_Socket
     (Socket :     Integer;
      Data   :     Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Send data on unix socket. The socket must be of type Family_Unix for
   --  this to work.

   procedure Receive_Socket
     (Socket :     Integer;
      Data   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Receive data from given socket. Last is the index value which designates
   --  the last stream element in data.

   procedure Set_Socket_Option
     (Socket : Integer;
      Level  : Level_Type := Socket_Level;
      Option : Option_Name_Bool;
      Value  : Boolean);
   --  Set socket option of given socket to specified boolean value.

   procedure Set_Socket_Option
     (Socket : Integer;
      Level  : Level_Type := Socket_Level;
      Option : Option_Name_Str;
      Value  : String);
   --  Set socket option of given socket to specified string value.

   function Query_Iface
     (Iface_Name : Types.Iface_Name_Type;
      Request    : Netdev_Request_Name)
      return If_Req_Type;
   --  Query interface with given request.

   procedure Ioctl
     (Socket  : Integer;
      Request : C.int;
      If_Req  : not null access If_Req_Type);
   --  Execute netdevice ioctl request on interface with given name and request
   --  type. The specified socket must have been created beforehand.

private

   function C_Bind
     (S       : Interfaces.C.int;
      Name    : System.Address;
      Namelen : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, C_Bind, "bind");

   function C_Connect
     (S       : Interfaces.C.int;
      Name    : System.Address;
      Namelen : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, C_Connect, "connect");

   function C_Sendto
     (S     : Interfaces.C.int;
      Buf   : System.Address;
      Len   : Interfaces.C.int;
      Flags : Interfaces.C.int;
      To    : System.Address;
      Tolen : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, C_Sendto, "sendto");

   function C_Recvfrom
     (S       : Interfaces.C.int;
      Msg     : System.Address;
      Len     : Interfaces.C.int;
      Flags   : Interfaces.C.int;
      From    : System.Address;
      Fromlen : not null access Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, C_Recvfrom, "recvfrom");

end Anet.Sockets.Thin;
