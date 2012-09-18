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

with Anet.Constants;
with Anet.Byte_Swapping;

package body Anet.Sockets.Thin.Packet is

   type Sockaddr_LL_Type is record
      Sa_Family   : C.unsigned_short            := Constants.AF_PACKET;
      --  Address family (always AF_PACKET)
      Sa_Protocol : C.unsigned_short            := 0;
      --  Physical layer protocol
      Sa_Ifindex  : C.int                       := 0;
      --  Interface number
      Sa_Hatype   : C.unsigned_short            := 0;
      --  Header type
      Sa_Pkttype  : C.unsigned_char             := 0;
      --  Packet type
      Sa_Halen    : C.unsigned_char             := 0;
      --  Length of address
      Sa_Addr     : Hardware_Addr_Type (1 .. 8) := (others => 0);
      --  Physical layer address
   end record;
   pragma Convention (C, Sockaddr_LL_Type);
   --  Device independent physical layer address

   -------------------------------------------------------------------------

   procedure Bind
     (Socket    :     Integer;
      Iface_Idx :     Positive;
      Success   : out Boolean)
   is
      use type C.int;

      Res   : C.int;
      Value : Sockaddr_LL_Type;
   begin
      Value.Sa_Protocol := C.unsigned_short
        (Byte_Swapping.Host_To_Network
           (Input => Double_Byte (Constants.ETH_P_IP)));
      Value.Sa_Ifindex  := C.int (Iface_Idx);

      Res := C_Bind (S       => C.int (Socket),
                     Name    => Value'Address,
                     Namelen => Value'Size / 8);

      Success := Res /= C_Failure;
   end Bind;

   -------------------------------------------------------------------------

   procedure Receive
     (Socket      :     Integer;
      Data        : out Ada.Streams.Stream_Element_Array;
      Last        : out Ada.Streams.Stream_Element_Offset;
      Src_HW_Addr : out Hardware_Addr_Type)
   is
      use type Interfaces.C.int;
      use type Ada.Streams.Stream_Element_Offset;

      Res   : C.int;
      Saddr : Sockaddr_LL_Type;
      Len   : aliased C.int := Saddr'Size / 8;
   begin
      Res := C_Recvfrom (S       => C.int (Socket),
                         Msg     => Data'Address,
                         Len     => Data'Length,
                         Flags   => 0,
                         From    => Saddr'Address,
                         Fromlen => Len'Access);

      if Res = C_Failure then
         raise Socket_Error with "Error receiving packet data: "
           & Get_Errno_String;
      end if;

      Src_HW_Addr := Saddr.Sa_Addr (Saddr.Sa_Addr'First .. Src_HW_Addr'Length);
      Last        := Data'First + Ada.Streams.Stream_Element_Offset (Res - 1);
   end Receive;

   -------------------------------------------------------------------------

   procedure Send
     (Socket    :     Integer;
      Data      :     Ada.Streams.Stream_Element_Array;
      Last      : out Ada.Streams.Stream_Element_Offset;
      To        :     Hardware_Addr_Type;
      Iface_Idx :     Positive;
      Success   : out Boolean)
   is
      use type C.int;
      use type Ada.Streams.Stream_Element_Offset;

      Res     : C.int;
      LL_Dest : Sockaddr_LL_Type;
   begin
      LL_Dest.Sa_Ifindex  := C.int (Iface_Idx);
      LL_Dest.Sa_Halen    := To'Length;
      LL_Dest.Sa_Protocol := C.unsigned_short
        (Byte_Swapping.Host_To_Network
           (Input => Double_Byte (Constants.ETH_P_IP)));

      LL_Dest.Sa_Addr (1 .. To'Length) := To;

      Res := C_Sendto (S     => C.int (Socket),
                       Buf   => Data'Address,
                       Len   => Data'Length,
                       Flags => 0,
                       To    => LL_Dest'Address,
                       Tolen => LL_Dest'Size / 8);

      Success := Res /= C_Failure;
      Last    := Data'First + Ada.Streams.Stream_Element_Offset (Res - 1);
   end Send;

end Anet.Sockets.Thin.Packet;
