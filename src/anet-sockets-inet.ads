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

package Anet.Sockets.Inet is

   type Inet_Socket_Type is abstract new Socket_Type with private;
   --  Internet socket.

   procedure Bind
     (Socket  : in out Inet_Socket_Type;
      Address :        Socket_Addr_Type := (Addr_V4 => Any_Addr, others => <>);
      Iface   :        Iface_Name_Type  := "");
   --  Bind given Internet socket to the specified IP address and port. If an
   --  interface name is given, the socket is bound to it.

   procedure Send
     (Socket : Inet_Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      Dst    : Socket_Addr_Type);
   --  Send given data to the specified destination via the given socket.

   type UDPv4_Socket_Type is new Inet_Socket_Type
     and Dgram_Socket_Type with private;
   --  IPv4/UDP socket.

   function Create return UDPv4_Socket_Type;
   --  Create new IPv4/UDP socket.

   type TCPv4_Socket_Type is new Inet_Socket_Type
     and Stream_Socket_Type with private;
   --  IPv4/TCP socket.

   function Create return TCPv4_Socket_Type;
   --  Create new IPv4/TCP socket.

   type UDPv6_Socket_Type is new Inet_Socket_Type
     and Dgram_Socket_Type with private;
   --  IPv6/UDP socket.

   function Create return UDPv6_Socket_Type;
   --  Create new IPv6/UDP socket.

   type TCPv6_Socket_Type is new Inet_Socket_Type
     and Stream_Socket_Type with private;
   --  IPv6/TCP socket.

   function Create return TCPv6_Socket_Type;
   --  Create new IPv6/TCP socket.

private

   type Inet_Socket_Type is abstract new Socket_Type with null record;

   type UDPv4_Socket_Type is new Inet_Socket_Type
     and Dgram_Socket_Type with null record;

   type TCPv4_Socket_Type is new Inet_Socket_Type
     and Stream_Socket_Type with null record;

   type UDPv6_Socket_Type is new Inet_Socket_Type
     and Dgram_Socket_Type with null record;

   type TCPv6_Socket_Type is new Inet_Socket_Type
     and Stream_Socket_Type with null record;

end Anet.Sockets.Inet;