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

   type Sockaddr_In_Type (Family : Family_Inet_Type := Family_Inet) is record
      Sin_Family : Interfaces.C.unsigned_short;
      --  Address family
      Sin_Port   : Interfaces.C.unsigned_short;
      --  Port in network byte order

      case Family is
         when Family_Inet =>
            Sin_Addr : IPv4_Addr_Type      := (others => 0);
            --  IPv4 address
            Sin_Zero : Byte_Array (1 .. 8) := (others => 0);
            --  Padding
         when Family_Inet6 =>
            Sin_Flowinfo : Interfaces.C.unsigned;
            --  IPv6 flow information
            Sin6_Addr    : IPv6_Addr_Type := (others => 0);
            --  IPv6 address
            Sin_Scope_ID : Interfaces.C.unsigned;
            --  Scope ID
      end case;
   end record;
   pragma Unchecked_Union (Sockaddr_In_Type);
   pragma Convention (C, Sockaddr_In_Type);
   --  Low-level internet socket address type (struct sockaddr_in, struct
   --  sockaddr_in6).

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

   procedure Bind_Socket
     (Socket  : Integer;
      Address : IP_Addr_Type;
      Port    : Port_Type);
   --  Bind given socket to specified IP address and port.

   procedure Bind_Socket
     (Socket : Integer;
      Iface  : String);
   --  Bind given packet socket (Family_Packet) to specified interface.

   procedure Bind_Unix_Socket
     (Socket : Integer;
      Path   : String);
   --  Bind given unix socket (Family_Unix) to specified path.

   procedure Connect_Socket
     (Socket : Integer;
      Path   : String);
   --  Connect given unix socket (Family_Unix) to specified path.

   procedure Listen_Socket
     (Socket  : Integer;
      Backlog : Positive := 1);
   --  Listen for specified amount of requests on given socket.

   procedure Accept_Socket
     (Socket     :     Integer;
      New_Socket : out Integer);
   --  Accept connection request from listening socket and return new connected
   --  socket.

   procedure Receive_Socket
     (Socket   :     Integer;
      Data     : out Ada.Streams.Stream_Element_Array;
      Last     : out Ada.Streams.Stream_Element_Offset;
      Source   : out Socket_Addr_Type);
   --  Receive data from given socket. Last is the index value which designates
   --  the last stream element in data. The source IP and port specify the
   --  sender socket from which the data was received.

   procedure Receive_Socket
     (Socket      :     Integer;
      Data        : out Ada.Streams.Stream_Element_Array;
      Last        : out Ada.Streams.Stream_Element_Offset;
      Src_HW_Addr : out Hardware_Addr_Type);
   --  Receive data from given packet socket (Family_Packet). Last is the index
   --  value which designates the last stream element in data. The source
   --  hardware address specifies the MAC of the packet sender.

   procedure Receive_Socket
     (Socket :     Integer;
      Data   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Receive data from given unix domain socket (Family_Unix). Last is the
   --  index value which designates the last stream element in data.

   procedure Send_Socket
     (Socket   :     Integer;
      Data     :     Ada.Streams.Stream_Element_Array;
      Last     : out Ada.Streams.Stream_Element_Offset;
      Dst_IP   :     IP_Addr_Type;
      Dst_Port :     Port_Type);
   --  Send data to another socket specified by destination IP and port. Last
   --  is the index value which designates the last sent stream element.

   procedure Send_Socket
     (Socket :     Integer;
      Data   :     Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      To     :     Hardware_Addr_Type;
      Iface  :     String);
   --  Send data on packet socket to given hardware address over interface
   --  specified by name. The socket must be of type Family_Packet for this to
   --  work.

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
      Group  : IP_Addr_Type;
      Iface  : String := "");
   --  Join the given multicast group on the interface specified by name. If no
   --  interface name is provided, the kernel selects the interface.

   procedure Get_Socket_Info
     (Sock_Addr :     Sockaddr_In_Type;
      Source    : out Socket_Addr_Type);
   --  Get IP address and port from given low-level inet sock address.

   function Get_Iface_Index (Name : String) return Positive;
   --  Get interface index of interface given by name.

   function Get_Iface_Mac (Name : String) return Hardware_Addr_Type;
   --  Get hardware address of interface given by name.

   function Get_Iface_IP (Name : String) return IPv4_Addr_Type;
   --  Get IP address of interface given by name. If given interface has no
   --  assigned IP an exception is raised.

   function Is_Iface_Up (Name : String) return Boolean;
   --  Check if interface given by name is up. True is returned if the
   --  interface is up.

   procedure Set_Iface_State
     (Name  : String;
      State : Boolean);
   --  Set state of interface given by name. If state is True the interface is
   --  brought up.

end Anet.Sockets.Thin;
