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

with Ahven.Framework;

package Socket_Tests is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Testcase);
   --  Initialize testcase.

   procedure Send_V4_Stream;
   --  Test sending over IPv4 stream socket.

   procedure Send_V4_Datagram;
   --  Test sending over IPv4 datagram socket.

   procedure Send_V6_Stream;
   --  Test sending over IPv6 stream socket.

   procedure Send_V6_Datagram;
   --  Test sending over IPv6 datagram socket.

   procedure Send_Multicast_V4;
   --  Test sending over IPv4 multicast.

   procedure Send_Multicast_V6;
   --  Test sending over IPv6 multicast.

   procedure Send_Unix_Stream;
   --  Test sending over a UNIX stream socket.

   procedure Send_Unix_Datagram;
   --  Test sending over a datagram Unix socket.

   procedure Send_Netlink_Raw;
   --  Test sending over raw Netlink socket.

   procedure Unix_Delete_Socket;
   --  Test socket path cleanup of UNIX domain socket.

   procedure Listen_Callbacks;
   --  Test data reception callback handling.

   procedure Error_Callbacks;
   --  Test error callback handling.

end Socket_Tests;
