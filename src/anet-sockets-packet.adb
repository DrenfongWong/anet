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

with Anet.Sockets.Thin.Packet;

package body Anet.Sockets.Packet is

   -------------------------------------------------------------------------

   procedure Bind
     (Socket : in out Packet_Socket_Type;
      Iface  :        Iface_Name_Type)
   is
   begin
      Thin.Packet.Bind (Socket => Socket.Sock_FD,
                        Iface  => Iface);
      Socket.Address.HW_Addr := Get_Iface_Mac (Name => Iface);
   end Bind;

   -------------------------------------------------------------------------

   function Create return UDP_Socket_Type
   is
   begin
      return Socket : UDP_Socket_Type do
         Create (Socket => Socket,
                 Family => Family_Packet,
                 Mode   => Datagram_Socket);
      end return;
   end Create;

   -------------------------------------------------------------------------

   function Create return TCP_Socket_Type
   is
   begin
      return Socket : TCP_Socket_Type do
         Create (Socket => Socket,
                 Family => Family_Packet,
                 Mode   => Stream_Socket);
      end return;
   end Create;

   -------------------------------------------------------------------------

   procedure Send
     (Socket : Packet_Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      To     : Hardware_Addr_Type;
      Iface  : Iface_Name_Type)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Len : Ada.Streams.Stream_Element_Offset;
   begin
      Thin.Packet.Send
        (Socket => Socket.Sock_FD,
         Data   => Item,
         Last   => Len,
         To     => To,
         Iface  => Iface);

      if Len /= Item'Length then
         raise Socket_Error with "Incomplete packet send operation to "
           & To_String (Address => To) & ", only" & Len'Img & " of"
           & Item'Length'Img & " bytes sent";
      end if;
   end Send;

end Anet.Sockets.Packet;
