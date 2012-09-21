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

package body Anet.Sockets.Thin.Inet is

   -------------------------------------------------------------------------

   procedure Join_Multicast_Group
     (Socket    :     Integer;
      Group     :     Sockaddr_In_Type;
      Iface_Idx :     Natural := 0;
      Success   : out Boolean)
   is
      use type C.int;
      use type C.unsigned_short;

      Mreq      : IPv4_Mreq_Type;
      Mreq6     : IPv6_Mreq_Type;
      Mreq_Addr : System.Address;
      Mreq_Size : C.int;
      Opt       : C.int;
      Level     : C.int;
      Res       : C.int;
   begin
      if Group.Sin_Family = Constants.Sys.AF_INET then
         Mreq.Imr_Multiaddr := Group.Sin_Addr;
         Mreq.Imr_Interface := C.unsigned (Iface_Idx);
         Mreq_Addr          := Mreq'Address;
         Mreq_Size          := Mreq'Size / 8;
         Opt                := Constants.Sys.IP_ADD_MEMBERSHIP;
         Level              := Constants.Sys.IPPROTO_IP;
      else
         Mreq6.IPv6mr_Multiaddr := Group.Sin6_Addr;
         Mreq6.IPv6mr_Interface := C.unsigned (Iface_Idx);
         Mreq_Addr              := Mreq6'Address;
         Mreq_Size              := Mreq6'Size / 8;
         Opt                    := Constants.IPV6_ADD_MEMBERSHIP;
         Level                  := Constants.IPPROTO_IPV6;
      end if;

      Res := C_Setsockopt
        (S       => C.int (Socket),
         Level   => Level,
         Optname => Opt,
         Optval  => Mreq_Addr,
         Optlen  => Mreq_Size);

      Success := Res /= C_Failure;
   end Join_Multicast_Group;

end Anet.Sockets.Thin.Inet;
