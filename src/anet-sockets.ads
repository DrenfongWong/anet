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

private with Ada.Finalization;

with Interfaces.C;

with Anet.Constants;

package Anet.Sockets is

   type Family_Type is
     (Family_Inet,
      Family_Inet6,
      Family_Netlink,
      Family_Packet,
      Family_Unix);
   --  Address families.

   type Mode_Type is
     (Datagram_Socket,
      Raw_Socket,
      Stream_Socket);
   --  Supported socket modes.

   type Level_Type is (Socket_Level);
   --  Protocol level type.

   type Socket_Type is abstract tagged limited private;
   --  Communication socket.

   procedure Close (Socket : in out Socket_Type);
   --  Close given socket.

   procedure Send
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array);
   --  Send data on socket to connected endpoint.

   procedure Receive
     (Socket :     Socket_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Receive data from given socket. This procedure blocks until data has
   --  been received. Last is the index value such that Item (Last) is the last
   --  character assigned. An exception is raised if a socket error occurs.

   procedure Listen
     (Socket  : Socket_Type;
      Backlog : Positive := 1);
   --  Listen for specified amount of requests on given socket.

   type Dgram_Socket_Type is limited interface;
   --  Datagram socket.

   type Stream_Socket_Type is limited interface;
   --  Stream socket.

   procedure Send
     (Socket : Stream_Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array) is abstract;
   --  Send data on socket to connected endpoint.

   procedure Receive
     (Socket :     Stream_Socket_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is abstract;
   --  Receive data from given socket. This procedure blocks until data has
   --  been received. Last is the index value such that Item (Last) is the last
   --  character assigned. An exception is raised if a socket error occurs.

   procedure Listen
     (Socket  : Stream_Socket_Type;
      Backlog : Positive := 1) is abstract;
   --  Listen for specified amount of requests on given socket.

   procedure Accept_Connection
     (Socket     :     Stream_Socket_Type;
      New_Socket : out Stream_Socket_Type) is abstract;
   --  Accept first connection request from listening socket and return new
   --  connected socket.

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

   Socket_Error : exception;

private

   Families : constant array (Family_Type) of Interfaces.C.int
     := (Family_Inet    => Constants.Sys.AF_INET,
         Family_Inet6   => Constants.Sys.AF_INET6,
         Family_Netlink => Constants.AF_NETLINK,
         Family_Packet  => Constants.AF_PACKET,
         Family_Unix    => Constants.AF_UNIX);
   --  Address family mapping.

   Modes : constant array (Mode_Type) of Interfaces.C.int
     := (Datagram_Socket => Constants.Sys.SOCK_DGRAM,
         Raw_Socket      => Constants.SOCK_RAW,
         Stream_Socket   => Constants.Sys.SOCK_STREAM);
   --  Socket mode mapping.

   Levels : constant array (Level_Type) of Interfaces.C.int
     := (Socket_Level => Constants.Sys.SOL_SOCKET);
   --  Protocol level mapping.

   Options_Bool : constant array (Option_Name_Bool) of Interfaces.C.int
     := (Reuse_Address => Constants.Sys.SO_REUSEADDR,
         Broadcast     => Constants.Sys.SO_BROADCAST);
   --  Mapping for option names with boolean value.

   Options_Str : constant array (Option_Name_Str) of Interfaces.C.int
     := (Bind_To_Device => Constants.SO_BINDTODEVICE);
   --  Mapping for option names with string value.

   use type Interfaces.C.int;

   type Socket_Type is new Ada.Finalization.Limited_Controlled with record
      Sock_FD : Interfaces.C.int := -1;
   end record;

   overriding
   procedure Finalize (Socket : in out Socket_Type);
   --  Close socket.

   procedure Init
     (Socket   : in out Socket_Type;
      Family   :        Family_Type;
      Mode     :        Mode_Type;
      Protocol :        Natural := 0);
   --  Initialize given socket with specified family, mode and protocol.

   procedure Check_Complete_Send
     (Item      : Ada.Streams.Stream_Element_Array;
      Result    : Interfaces.C.int;
      Error_Msg : String);
   --  Verify that a Send operation was able to transmit all bytes of given
   --  buffer by calculating the actual number of bytes sent from the buffer
   --  range and the send(2)/sendto(2) result specified. The procedure raises
   --  an exception starting with the given error message if the check fails.

end Anet.Sockets;
