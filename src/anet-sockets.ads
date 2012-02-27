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
with Ada.Strings.Unbounded;

private with Ada.Finalization;

with Interfaces.C;

with System;

package Anet.Sockets is

   Max_Iface_Name_Len : constant := 16;
   --  Maximum length of an interface name (IFNAMSIZ).

   subtype Iface_Name_Range is Positive range 1 .. Max_Iface_Name_Len - 1;
   --  Range of interface name (1 .. IFNAMSIZ - 1).

   type Iface_Name_Type is array (Iface_Name_Range range <>) of Character;
   --  Interface name type.

   function Is_Valid_Iface (Name : String) return Boolean;
   --  Returns true if the given name is a valid interface name.

   Max_Unix_Path_Len  : constant := 108;
   --  Maximum length of an unix path (UNIX_PATH_MAX).

   subtype Unix_Path_Range is Positive range 1 .. Max_Unix_Path_Len - 1;
   --  Range of unix paths (1 .. UNIX_PATH_MAX - 1).

   type Unix_Path_Type is array (Unix_Path_Range range <>) of Character;
   --  Unix path type.

   function Is_Valid_Unix (Path : String) return Boolean;
   --  Returns true if the given path is a valid unix path.

   type Family_Type is (Family_Inet, Family_Inet6, Family_Packet, Family_Unix);
   --  Address families (IPv4, IPv6, raw).

   subtype Family_Inet_Type is Family_Type range Family_Inet .. Family_Inet6;
   --  Internet protocol address families.

   type Mode_Type is (Stream_Socket, Datagram_Socket);
   --  Supported socket modes.

   type Socket_Addr_Type (Family : Family_Type := Family_Inet) is record
      case Family is
         when Family_Inet  =>
            Addr_V4 : IPv4_Addr_Type := (others => 0);
            Port_V4 : Port_Type      := 0;
         when Family_Inet6 =>
            Addr_V6 : IPv6_Addr_Type := (others => 0);
            Port_V6 : Port_Type      := 0;
         when Family_Packet =>
            HW_Addr : Hardware_Addr_Type (1 .. 6) := (others => 0);
         when Family_Unix =>
            Path    : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;
   --  Socket address information. This record is used to specify
   --  family-specific address information such as the IP address and port of a
   --  sender of data.

   No_Addr : constant Socket_Addr_Type;

   function To_String (Address : Socket_Addr_Type) return String;
   --  Return string representation for given socket address type.

   type Socket_Type is tagged limited private;
   --  Communication socket.

   procedure Create
     (Socket : out Socket_Type;
      Family :     Family_Type;
      Mode   :     Mode_Type);
   --  Create a new socket with given family (IPv4, IPv6, packet, Unix) and
   --  mode (UDP, TCP).

   procedure Bind
     (Socket  : in out Socket_Type;
      Address :        Socket_Addr_Type := (Addr_V4 => Any_Addr, others => <>);
      Iface   :        Iface_Name_Type  := "");
   --  Open given socket and bind it to specified IP address and port. If an
   --  interface name is given the socket is bound to it.

   procedure Bind_Packet
     (Socket : in out Socket_Type;
      Iface  :        Iface_Name_Type);
   --  Bind given packet socket (Family_Packet) to specified interface.

   procedure Bind_Unix
     (Socket : in out Socket_Type;
      Path   :        Unix_Path_Type);
   --  Bind given unix socket (Family_Unix) to specified path.

   procedure Close (Socket : in out Socket_Type);
   --  Close given socket.

   procedure Connect
     (Socket : in out Socket_Type;
      Dst    :        Socket_Addr_Type);
   --  Connect given socket to the address specified by Dst.

   procedure Accept_Connection
     (Socket     :     Socket_Type;
      New_Socket : out Socket_Type);
   --  Accept first connection request from listening socket and return new
   --  connected socket.

   procedure Send
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      Dst    : Socket_Addr_Type);
   --  Send given data to the specified destination via the given socket.

   procedure Send
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      To     : Hardware_Addr_Type;
      Iface  : Iface_Name_Type);
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
      Src    : out Socket_Addr_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Receive data from given socket. This procedure blocks until data has
   --  been received. Last is the index value such that Item (Last) is the last
   --  character assigned. An exception is raised if a socket error occurs. The
   --  Src argument specifies the sender from which the data was received.

   procedure Listen
     (Socket  : Socket_Type;
      Backlog : Positive := 1);
   --  Listen for specified amount of requests on given socket.

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

   Socket_Error : exception;

private

   No_Addr : constant Socket_Addr_Type := (Family => Family_Inet,
                                           others => <>);

   type Socket_Type is new Ada.Finalization.Limited_Controlled with record
      Sock_FD : Integer := -1;
      Address : Socket_Addr_Type;
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
