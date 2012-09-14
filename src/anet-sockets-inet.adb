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

with Anet.Sockets.Thin.Inet;

package body Anet.Sockets.Inet is

   -------------------------------------------------------------------------

   procedure Bind
     (Socket  : in out Inet_Socket_Type;
      Address :        Socket_Addr_Type := (Addr_V4 => Any_Addr, others => <>);
      Iface   :        Iface_Name_Type  := "")
   is
   begin
      Thin.Set_Socket_Option
        (Socket => Socket.Sock_FD,
         Option => Reuse_Address,
         Value  => True);

      Thin.Inet.Bind (Socket  => Socket.Sock_FD,
                      Address => Address);
      Socket.Address := Address;

      if Iface'Length /= 0 then
         Thin.Set_Socket_Option
           (Socket => Socket.Sock_FD,
            Level  => Thin.Socket_Level,
            Option => Bind_To_Device,
            Value  => String (Iface));
      end if;
   end Bind;

   -------------------------------------------------------------------------

   function Create return UDPv4_Socket_Type
   is
   begin
      return Socket : UDPv4_Socket_Type do
         Create (Socket => Socket,
                 Family => Family_Inet,
                 Mode   => Datagram_Socket);
      end return;
   end Create;

   -------------------------------------------------------------------------

   function Create return TCPv4_Socket_Type
   is
   begin
      return Socket : TCPv4_Socket_Type do
         Create (Socket => Socket,
                 Family => Family_Inet,
                 Mode   => Stream_Socket);
      end return;
   end Create;

   -------------------------------------------------------------------------

   function Create return UDPv6_Socket_Type
   is
   begin
      return Socket : UDPv6_Socket_Type do
         Create (Socket => Socket,
                 Family => Family_Inet6,
                 Mode   => Datagram_Socket);
      end return;
   end Create;

   -------------------------------------------------------------------------

   function Create return TCPv6_Socket_Type
   is
   begin
      return Socket : TCPv6_Socket_Type do
         Create (Socket => Socket,
                 Family => Family_Inet6,
                 Mode   => Stream_Socket);
      end return;
   end Create;

   -------------------------------------------------------------------------

   procedure Send
     (Socket : Inet_Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      Dst    : Socket_Addr_Type)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Len : Ada.Streams.Stream_Element_Offset;
   begin
      Thin.Inet.Send (Socket => Socket.Sock_FD,
                      Data   => Item,
                      Last   => Len,
                      Dst    => Dst);

      if Len /= Item'Length then
         raise Socket_Error with "Incomplete send operation to "
           & To_String (Address => Dst) & ", only" & Len'Img & " of"
           & Item'Length'Img & " bytes sent";
      end if;
   end Send;

end Anet.Sockets.Inet;
