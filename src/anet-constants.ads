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

pragma Warnings (Off);
with System.OS_Constants;
pragma Warnings (On);

package Anet.Constants is

   package Sys renames System.OS_Constants;

   --------------
   -- Families --
   --------------

   AF_UNIX             : constant := 1;        --  Unix domain family
   AF_PACKET           : constant := 17;       --  Packet family

   ---------------------
   -- Protocol levels --
   ---------------------

   IPPROTO_IPV6        : constant := 41;       --  IPv6

   -----------------------
   -- Socket operations --
   -----------------------

   SO_BINDTODEVICE     : constant := 25;       --  Bind to interface device
   SO_ATTACH_FILTER    : constant := 26;       --  Socket filtering
   IPV6_ADD_MEMBERSHIP : constant := 20;       --  Join multicast group (IPv6)

   -----------------------------------
   -- Socket configuration controls --
   -----------------------------------

   SIOCGIFADDR         : constant := 16#8915#; --  Get address
   SIOCGIFFLAGS        : constant := 16#8913#; --  Get flags
   SIOCSIFFLAGS        : constant := 16#8914#; --  Set flags
   SIOCGIFHWADDR       : constant := 16#8927#; --  Get hardware address
   SIOCGIFINDEX        : constant := 16#8933#; --  Name -> if_index mapping

   ---------------------
   -- Interface flags --
   ---------------------

   IFF_UP              : constant := 1;        --  Interface is up

   ---------------------------
   -- Ethernet protocol IDs --
   ---------------------------

   ETH_P_IP            : constant := 16#0800#; --  Internet Protocol packet

   ---------------
   -- Unix path --
   ---------------

   UNIX_PATH_MAX       : constant := 108;

   ---------------------
   -- Interface names --
   ---------------------

   IFNAMSIZ            : constant := 16;

end Anet.Constants;
