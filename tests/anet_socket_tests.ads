--
--  Copyright (C) 2011 secunet Security Networks AG
--  Copyright (C) 2011 Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2011 Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Anet_Socket_Tests is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Testcase);
   --  Initialize testcase.

   procedure Send_V4;
   --  Test sending over IPv4 socket.

   procedure Send_V6;
   --  Test sending over IPv6 socket.

   procedure Send_Multicast_V6;
   --  Test sending over IPv6 multicast.

   procedure Send_Unix;
   --  Test sending over a streaming Unix socket.

   procedure Send_Unix_Datagram;
   --  Test sending over a datagram Unix socket.

   procedure Receive_V4;
   --  Test data reception with IPv4 socket.

   procedure Receive_V6;
   --  Test data reception with IPv6 socket.

   procedure Receive_Multicast_V6;
   --  Test IPv6 multicast data reception.

   procedure Receive_Unix;
   --  Test data reception with UNIX socket.

   procedure Listen_Callbacks;
   --  Test data reception callback handling.

   procedure Error_Callbacks;
   --  Test error callback handling.

   procedure Get_Loopback_Interface_Index;
   --  Test get interface index function on loopback device.

   procedure Get_Loopback_Interface_Mac;
   --  Test get interface MAC function on loopback device.

   procedure Get_Loopback_Interface_IP;
   --  Test get interface IP function on loopback device.

end Anet_Socket_Tests;
