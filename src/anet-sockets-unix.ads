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

with Ada.Strings.Unbounded;

with Anet.Types;

package Anet.Sockets.Unix is

   type Unix_Socket_Type is abstract new Socket_Type with private;
   --  UNIX domain socket.

   overriding
   procedure Close (Socket : in out Unix_Socket_Type);
   --  Close given UNIX domain socket.

   procedure Bind
     (Socket : in out Unix_Socket_Type;
      Path   :        Types.Unix_Path_Type);
   --  Bind given UNIX domain socket to path.

   procedure Connect
     (Socket : in out Unix_Socket_Type;
      Path   :        Types.Unix_Path_Type);
   --  Connect given UNIX domain socket to path.

   type UDP_Socket_Type is new Unix_Socket_Type
     and Dgram_Socket_Type with private;
   --  UNIX domain socket in datagram mode.

   function Init return UDP_Socket_Type;
   --  Initialize given UNIX/UDP socket.

   procedure Receive
     (Socket :     UDP_Socket_Type;
      Src    : out Types.Unix_Path_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Receive data from given Unix/UDP socket. This procedure blocks until
   --  data has been received. Last is the index value such that Item (Last) is
   --  the last character assigned. An exception is raised if a socket error
   --  occurs. The source argument is set to the socket path from which the
   --  data was received.

   type TCP_Socket_Type is new Unix_Socket_Type
     and Stream_Socket_Type with private;
   --  UNIX domain socket in stream mode.

   function Init return TCP_Socket_Type;
   --  Initialize given UNIX/TCP socket.

   overriding
   procedure Accept_Connection
     (Socket     :     TCP_Socket_Type;
      New_Socket : out TCP_Socket_Type);
   --  Accept first connection request from listening socket and return new
   --  connected socket.

private

   type Unix_Socket_Type is abstract new Socket_Type with record
      Path : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type UDP_Socket_Type is new Unix_Socket_Type
     and Dgram_Socket_Type with null record;

   type TCP_Socket_Type is new Unix_Socket_Type
     and Stream_Socket_Type with null record;

end Anet.Sockets.Unix;
