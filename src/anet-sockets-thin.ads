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

package Anet.Sockets.Thin is

   type Level_Type is (Socket_Level);
   --  Protocol level type.

   type Netdev_Request_Name is
     (If_Addr,
      If_Flags,
      If_Hwaddr,
      If_Index);
   --  Supported netdevice requests.

   procedure Create_Socket
     (Socket : out Integer;
      Family :     Family_Type := Family_Inet;
      Mode   :     Mode_Type   := Datagram_Socket);
   --  Create a new communication socket with specified family and mode.

   procedure Close_Socket (Socket : Integer);
   --  Close given socket.

   procedure Connect_Socket
     (Socket : Integer;
      Dst    : Socket_Addr_Type);
   --  Connect given socket to specified destination address.

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

   procedure Join_Multicast_Group
     (Socket : Integer;
      Group  : Socket_Addr_Type;
      Iface  : Iface_Name_Type := "");
   --  Join the given multicast group on the interface specified by name. If no
   --  interface name is provided, the kernel selects the interface.

   function Get_Iface_Index (Name : Iface_Name_Type) return Positive;
   --  Get interface index of interface given by name.

   function Get_Iface_Mac (Name : Iface_Name_Type) return Hardware_Addr_Type;
   --  Get hardware address of interface given by name.

   function Get_Iface_IP (Name : Iface_Name_Type) return IPv4_Addr_Type;
   --  Get IP address of interface given by name. If given interface has no
   --  assigned IP an exception is raised.

   function Is_Iface_Up (Name : Iface_Name_Type) return Boolean;
   --  Check if interface given by name is up. True is returned if the
   --  interface is up.

   procedure Set_Iface_State
     (Name  : Iface_Name_Type;
      State : Boolean);
   --  Set state of interface given by name. If state is True the interface is
   --  brought up.

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
