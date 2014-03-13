--
--  Copyright (C) 2011-2014 Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2011-2014 Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Anet.Sockets.Thin.Inet is

   subtype Family_Inet_Type is Family_Type range Family_Inet .. Family_Inet6;
   --  Internet protocol address families.

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
   --  Low-level Internet socket address type (struct sockaddr_in, struct
   --  sockaddr_in6).

   Sockaddr_In_Size  : constant := 16;
   Sockaddr_In6_Size : constant := 28;

end Anet.Sockets.Thin.Inet;
