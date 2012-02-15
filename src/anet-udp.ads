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

with Ada.Streams;

package Anet.UDP is

   UDP_Header_Length : constant := 8;
   --  UDP header size in bytes.

   function Create_Header
     (Payload  : Ada.Streams.Stream_Element_Array;
      Src_IP   : IPv4_Addr_Type;
      Dst_IP   : IPv4_Addr_Type;
      Src_Port : Port_Type;
      Dst_Port : Port_Type)
      return Ada.Streams.Stream_Element_Array;
   --  Returns an UDP header for given source and destination port and payload.
   --  The source, destination IPv4 addresses are used for UDP checksuming.

   procedure Validate_Checksum
     (Packet : Ada.Streams.Stream_Element_Array;
      Src_IP : IPv4_Addr_Type;
      Dst_IP : IPv4_Addr_Type);
   --  Validate the checksum of the given UDP packet. Note: No length checks
   --  on the packet data are performed by this procedure. These checks must be
   --  done prior to calling this procedure. If the checksum is not valid, an
   --  Invalid_UDP_Packet exception is raised.

   Invalid_UDP_Packet : exception;

end Anet.UDP;
