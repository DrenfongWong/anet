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

with Ada.Streams;

private with Ada.Finalization;

with Interfaces.C;

with System;

package Anet.Sockets is

   type Family_Type is (Family_Inet, Family_Inet6, Family_Packet, Family_Unix);
   --  Address families (IPv4, IPv6, raw).

   subtype Family_Inet_Type is Family_Type range Family_Inet .. Family_Inet6;
   --  Internet protocol address families.

   type Mode_Type is (Stream_Socket, Datagram_Socket);
   --  Supported socket modes.

   type IP_Addr_Type (Family : Family_Inet_Type := Family_Inet) is private;
   --  IP address type. This type is used to represent IP adresses, IPv4 or
   --  IPv6 depending on family.

   function To_IP_Addr (Str : String) return IP_Addr_Type;
   --  Return IP address for given string.

   function To_String (Address : IP_Addr_Type) return String;
   --  Return string representation of an IP address.

   Any_Addr_V4 : constant IP_Addr_Type;
   --  IPv4 any inet address (0.0.0.0).

   Loopback_Addr_V4 : constant IP_Addr_Type;
   --  IPv4 localhost address (127.0.0.1).

   Broadcast_Addr_V4 : constant IP_Addr_Type;
   --  IPv4 broadcast address (255.255.255.255).

   Loopback_Addr_V6 : constant IP_Addr_Type;
   --  IPv6 loopback address (::1).

   All_DHCP_Relay_Agents_and_Servers : constant IP_Addr_Type;
   --  All DHCP relay agents and servers multicast group (FF02::1:2).

   type Count_Type is mod System.Max_Binary_Modulus;

   type Sender_Info_Type is record
      IP_Addr : IP_Addr_Type;
      Port    : Port_Type                   := 0;
      HW_Addr : Hardware_Addr_Type (1 .. 6) := (others => 0);
   end record;
   --  Sender information. This record stores information about a sender of
   --  data.

   type Rcv_Item_Callback is not null access procedure
     (Item : Ada.Streams.Stream_Element_Array;
      Src  : Sender_Info_Type);
   --  Data reception callback procedure. The Item argument contains the
   --  received data, the Src argument identifies the sender of the data.

   type Socket_Type is tagged limited private;
   --  Communication socket.

   procedure Create
     (Socket : out Socket_Type;
      Family :     Family_Type := Family_Inet;
      Mode   :     Mode_Type   := Datagram_Socket);
   --  Create a new socket with given family (IPv4, IPv6, packet) and mode
   --  (UDP, TCP).

   procedure Bind
     (Socket  : in out Socket_Type;
      Address :        IP_Addr_Type := Any_Addr_V4;
      Port    :        Port_Type;
      Iface   :        String       := "");
   --  Open given socket and bind it to specified IP address and port. If an
   --  interface name is given the socket is bound to it.

   procedure Bind
     (Socket : in out Socket_Type;
      Iface  :        String);
   --  Bind given packet socket (Family_Packet) to specified interface.

   procedure Bind_Unix
     (Socket : in out Socket_Type;
      Path   :        String);
   --  Bind given unix socket (Family_Unix) to specified path.

   procedure Connect
     (Socket : in out Socket_Type;
      Path   :        String);
   --  Connect given unix socket (Family_Unix) to specified path.

   procedure Accept_Unix
     (Socket     :     Socket_Type;
      New_Socket : out Socket_Type);
   --  Accept first connection request from listening socket and return new
   --  connected socket.

   function Get_Rcv_Msg_Count (Socket : Socket_Type) return Count_Type;
   --  Returns the number of received and processed DHCP messages.

   procedure Send
     (Socket   : Socket_Type;
      Item     : Ada.Streams.Stream_Element_Array;
      Dst_IP   : IP_Addr_Type;
      Dst_Port : Port_Type);
   --  Send given data to the specified destination via the given socket.

   procedure Send
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      To     : Hardware_Addr_Type;
      Iface  : String);
   --  Send data on packet socket to given hardware address over interface
   --  specified by name. The socket must be of type Family_Packet for this to
   --  work.

   procedure Send
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array);
   --  Send data on unix socket to connected or bound path. The socket must be
   --  of type Family_Unix for this to work.

   procedure Receive
     (Socket :     Socket_Type;
      Src    : out Sender_Info_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Receive data from given socket. This procedure blocks until data has
   --  been received. Last is the index value such that Item (Last) is the last
   --  character assigned. An exception is raised if a socket error occurs. The
   --  Src argument specifies the sender from which the data was received.

   procedure Listen_Unix
     (Socket  : Socket_Type;
      Backlog : Positive := 1);
   --  Listen for specified amount of requests on given socket.

   procedure Listen
     (Socket   : in out Socket_Type;
      Callback :        Rcv_Item_Callback);
   --  Start listening for data on given socket. The given callback is
   --  asynchronously executed upon data reception. Call stop procedure to
   --  properly shutdown the listener.

   procedure Stop (Socket : in out Socket_Type);
   --  Stop listening for data.

   type Option_Name_Bool is
     (Broadcast,
      Reuse_Address);
   --  Supported boolean socket options.

   type Option_Name_Str is (Bind_To_Device);
   --  Supported string based socket options.

   procedure Set_Socket_Option
     (Socket : Socket_Type;
      Option : Option_Name_Bool;
      Value  : Boolean);
   --  Set socket option of given socket to specified boolean value.

   procedure Set_Socket_Option
     (Socket : Socket_Type;
      Option : Option_Name_Str;
      Value  : String);
   --  Set socket option of given socket to specified string value.

   procedure Join_Multicast_Group
     (Socket : Socket_Type;
      Group  : IP_Addr_Type;
      Iface  : String := "");
   --  Join the given multicast group on the interface specified by name. If no
   --  interface name is provided, the kernel selects the interface.

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

   Socket_Error : exception;

private

   type IP_Addr_Type (Family : Family_Inet_Type := Family_Inet) is record
      case Family is
         when Family_Inet  => Addr_V4 : IPv4_Addr_Type := (others => 0);
         when Family_Inet6 => Addr_V6 : IPv6_Addr_Type := (others => 0);
      end case;
   end record;

   Any_Addr_V4 : constant IP_Addr_Type
     := (Family  => Family_Inet,
         Addr_V4 => (others => 0));

   Loopback_Addr_V4 : constant IP_Addr_Type
     := (Family  => Family_Inet,
         Addr_V4 => (127, 0, 0, 1));

   Loopback_Addr_V6 : constant IP_Addr_Type
     := (Family  => Family_Inet6,
         Addr_V6 => (16     => 1,
                     others => 0));

   Broadcast_Addr_V4 : constant IP_Addr_Type
     := (Family  => Family_Inet,
         Addr_V4 => (255, 255, 255, 255));

   All_DHCP_Relay_Agents_and_Servers : constant IP_Addr_Type
     := (Family  => Family_Inet6,
         Addr_V6 => (255, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2));

   protected type Trigger_Type is

      procedure Activate;
      --  Activate trigger.

      procedure Shutdown;
      --  Signal shutdown to all tasks waiting on the Stop entry.

      entry Stop;
      --  Entry used for listener ATC.

      procedure Signal_Termination;
      --  Signal termination to all tasks waiting on the Wait_For_Termination
      --  entry.

      entry Wait_For_Termination;
      --  Wait until termination is signaled.

   private
      Shutdown_Requested : Boolean := False;
      Is_Terminated      : Boolean := True;
   end Trigger_Type;
   --  This trigger is used to terminate the receiver task by means of ATC.

   task type Receiver_Task (Parent : not null access Socket_Type) is

      entry Listen (Cb : Rcv_Item_Callback);
      --  Start listening for data on parent's socket. The callback procedure
      --  is called upon reception of new data.

   end Receiver_Task;

   type Socket_Type is new Ada.Finalization.Limited_Controlled with record
      Sock_FD : Integer := -1;
      Family  : Family_Type;

      --  Data listener

      Item_Count : Count_Type := 0;
      pragma Atomic (Item_Count);

      Trigger : Trigger_Type;
      R_Task  : Receiver_Task (Parent => Socket_Type'Access);
   end record;

   overriding
   procedure Finalize (Socket : in out Socket_Type);
   --  Close socket.

   function C_Setsockopt
     (S       : Interfaces.C.int;
      Level   : Interfaces.C.int;
      Optname : Interfaces.C.int;
      Optval  : System.Address;
      Optlen  : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, C_Setsockopt, "setsockopt");
   --  Set given socket option on specified protocol level.

end Anet.Sockets;
