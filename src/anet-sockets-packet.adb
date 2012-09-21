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

with Interfaces.C;

with Anet.Net_Ifaces;
with Anet.Sockets.Thin;
with Anet.Constants;
with Anet.Byte_Swapping;

package body Anet.Sockets.Packet is

   package C renames Interfaces.C;

   -------------------------------------------------------------------------

   procedure Bind
     (Socket : in out Packet_Socket_Type;
      Iface  :        Types.Iface_Name_Type)
   is
      use type C.int;

      Res   : C.int;
      Value : Thin.Sockaddr_Ll_Type;
   begin
      Value.Sa_Protocol := C.unsigned_short
        (Byte_Swapping.Host_To_Network
           (Input => Double_Byte (Constants.ETH_P_IP)));
      Value.Sa_Ifindex  := C.int (Net_Ifaces.Get_Iface_Index (Name => Iface));

      Res := Thin.C_Bind (S       => C.int (Socket.Sock_FD),
                          Name    => Value'Address,
                          Namelen => Value'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable to bind packet socket to interface "
           & String (Iface) & " - " & Get_Errno_String;
      end if;
   end Bind;

   -------------------------------------------------------------------------

   procedure Init (Socket : in out UDP_Socket_Type)
   is
   begin
      Init (Socket   => Socket,
            Family   => Family_Packet,
            Mode     => Datagram_Socket,
            Protocol => Integer (Byte_Swapping.Host_To_Network
              (Input => Double_Byte (Constants.ETH_P_IP))));
   end Init;

   -------------------------------------------------------------------------

   procedure Receive
     (Socket :     Packet_Socket_Type;
      Src    : out Hardware_Addr_Type;
      Data   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      use type Interfaces.C.int;
      use type Ada.Streams.Stream_Element_Offset;

      Res   : C.int;
      Saddr : Thin.Sockaddr_Ll_Type;
      Len   : aliased C.int := Saddr'Size / 8;
   begin
      Res := Thin.C_Recvfrom (S       => C.int (Socket.Sock_FD),
                              Msg     => Data'Address,
                              Len     => Data'Length,
                              Flags   => 0,
                              From    => Saddr'Address,
                              Fromlen => Len'Access);

      if Res = C_Failure then
         raise Socket_Error with "Error receiving packet data: "
           & Get_Errno_String;
      end if;

      Src  := Saddr.Sa_Addr (Saddr.Sa_Addr'First .. Src'Length);
      Last := Data'First + Ada.Streams.Stream_Element_Offset (Res - 1);
   end Receive;

   -------------------------------------------------------------------------

   procedure Send
     (Socket : Packet_Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      To     : Hardware_Addr_Type;
      Iface  : Types.Iface_Name_Type)
   is
      use type C.int;
      use type Ada.Streams.Stream_Element_Offset;

      Res        : C.int;
      Ll_Dest    : Thin.Sockaddr_Ll_Type;
      Sent_Bytes : Ada.Streams.Stream_Element_Offset;
   begin
      Ll_Dest.Sa_Ifindex  := C.int (Net_Ifaces.Get_Iface_Index
                                    (Name => Iface));
      Ll_Dest.Sa_Halen    := To'Length;
      Ll_Dest.Sa_Protocol := C.unsigned_short
        (Byte_Swapping.Host_To_Network
           (Input => Double_Byte (Constants.ETH_P_IP)));

      Ll_Dest.Sa_Addr (1 .. To'Length) := To;

      Res := Thin.C_Sendto (S     => C.int (Socket.Sock_FD),
                            Buf   => Item'Address,
                            Len   => Item'Length,
                            Flags => 0,
                            To    => Ll_Dest'Address,
                            Tolen => Ll_Dest'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable to send packet data on interface "
           & String (Iface) & " to " & To_String (Address => To)
           & " - " & Get_Errno_String;
      end if;

      Sent_Bytes := Item'First + Ada.Streams.Stream_Element_Offset (Res - 1);
      if Sent_Bytes /= Item'Length then
         raise Socket_Error with "Incomplete packet send operation to "
           & To_String (Address => To) & ", only" & Sent_Bytes'Img & " of"
           & Item'Length'Img & " bytes sent";
      end if;
   end Send;

end Anet.Sockets.Packet;
