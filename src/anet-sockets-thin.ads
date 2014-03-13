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

with System;

package Anet.Sockets.Thin is

   type Sockaddr_Type is record
      Sa_Family : Interfaces.C.unsigned_short;
      --  Address family
      Sa_Data   : Interfaces.C.char_array (1 .. 14)
        := (others => Interfaces.C.nul);
      --  Family-specific data
   end record;
   pragma Convention (C, Sockaddr_Type);
   --  Generic socket address.

   type Sockaddr_Un_Type is record
      Sin_Family : Interfaces.C.unsigned_short := Constants.AF_UNIX;
      --  Address family
      Pathname   : Interfaces.C.char_array (1 .. Constants.UNIX_PATH_MAX)
        := (others => Interfaces.C.nul);
      --  Pathname
   end record;
   pragma Convention (C, Sockaddr_Un_Type);
   --  Low-level UNIX socket address type (struct sockaddr_un).

   type IPv4_Mreq_Type is record
      Imr_Multiaddr : IPv4_Addr_Type;
      Imr_Interface : IPv4_Addr_Type;
   end record;
   pragma Convention (C, IPv4_Mreq_Type);
   --  struct ip_mreq (netinet/in.h).

   type IPv6_Mreq_Type is record
      IPv6mr_Multiaddr : IPv6_Addr_Type;
      IPv6mr_Interface : Interfaces.C.unsigned;
   end record;
   pragma Convention (C, IPv6_Mreq_Type);
   --  struct ipv6_mreq (netinet/in.h).

   type Netdev_Request_Name is
     (If_Addr,
      If_Flags,
      If_Hwaddr,
      If_Index);
   --  Supported netdevice requests.

   type If_Req_Type (Name : Netdev_Request_Name := If_Index) is record
      Ifr_Name : Interfaces.C.char_array
        (1 .. Constants.IFNAMSIZ) := (others => Interfaces.C.nul);

      case Name is
         when If_Addr   =>
            Ifr_Addr    : Sockaddr_Type;
         when If_Hwaddr =>
            Ifr_Hwaddr  : Sockaddr_Type;
         when If_Index  =>
            Ifr_Ifindex : Interfaces.C.int   := 0;
         when If_Flags  =>
            Ifr_Flags   : Interfaces.C.short := 0;
      end case;
   end record;
   pragma Unchecked_Union (If_Req_Type);
   pragma Convention (C, If_Req_Type);
   --  Interface request structure (struct ifreq).

   Get_Requests : constant array (Netdev_Request_Name) of Interfaces.C.int
     := (If_Addr   => Constants.SIOCGIFADDR,
         If_Flags  => Constants.SIOCGIFFLAGS,
         If_Hwaddr => Constants.SIOCGIFHWADDR,
         If_Index  => Constants.SIOCGIFINDEX);
   --  Currently supported netdevice ioctl get requests.

   Set_Requests : constant array (Netdev_Request_Name) of Interfaces.C.int
     := (If_Flags => Constants.SIOCSIFFLAGS,
         others   => Interfaces.C.int (-1));
   --  Currently supported netdevice ioctl set requests.

   -------------
   -- Imports --
   -------------

   function C_Socket
     (Domain   : Interfaces.C.int;
      Typ      : Interfaces.C.int;
      Protocol : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, C_Socket, "socket");

   function C_Bind
     (S       : Interfaces.C.int;
      Name    : System.Address;
      Namelen : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, C_Bind, "bind");

   function C_Connect
     (S       : Interfaces.C.int;
      Name    : System.Address;
      Namelen : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, C_Connect, "connect");

   function C_Recv
     (S     : Interfaces.C.int;
      Msg   : System.Address;
      Len   : Interfaces.C.int;
      Flags : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, C_Recv, "recv");

   function C_Recvfrom
     (S       : Interfaces.C.int;
      Msg     : System.Address;
      Len     : Interfaces.C.int;
      Flags   : Interfaces.C.int;
      From    : System.Address;
      Fromlen : not null access Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, C_Recvfrom, "recvfrom");

   function C_Send
     (S     : Interfaces.C.int;
      Buf   : System.Address;
      Len   : Interfaces.C.int;
      Flags : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, C_Send, "send");

   function C_Sendto
     (S     : Interfaces.C.int;
      Buf   : System.Address;
      Len   : Interfaces.C.int;
      Flags : Interfaces.C.int;
      To    : System.Address;
      Tolen : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, C_Sendto, "sendto");

   function C_Setsockopt
     (S       : Interfaces.C.int;
      Level   : Interfaces.C.int;
      Optname : Interfaces.C.int;
      Optval  : System.Address;
      Optlen  : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, C_Setsockopt, "setsockopt");

   function C_Accept
     (S       : Interfaces.C.int;
      Name    : System.Address;
      Namelen : not null access Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, C_Accept, "accept");

   function C_Listen
     (Socket  : Interfaces.C.int;
      Backlog : Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, C_Listen, "listen");

   function C_Ioctl
     (S   : Interfaces.C.int;
      Req : Interfaces.C.int;
      Arg : access If_Req_Type)
      return Interfaces.C.int;
   pragma Import (C, C_Ioctl, "ioctl");

   function C_Close (Fd : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, C_Close, "close");

end Anet.Sockets.Thin;
