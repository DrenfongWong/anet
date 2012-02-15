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
with Ada.Strings.Unbounded;

package Anet.Sockets.Mock is

   use Ada.Strings.Unbounded;

   type Test_Socket_Type is new Socket_Type with private;

   overriding
   procedure Send
     (Socket   : Test_Socket_Type;
      Item     : Ada.Streams.Stream_Element_Array;
      Dst_IP   : Sockets.IP_Addr_Type;
      Dst_Port : Port_Type);

   overriding
   procedure Send
     (Socket : Test_Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      To     : Hardware_Addr_Type;
      Iface  : String);

   function Get_Last_Iface return String;

   procedure Clear;

   Send_Count    : Natural                           := 0;
   Last_Dst_IP   : Sockets.IP_Addr_Type              := Sockets.Any_Addr_V4;
   Last_Dst_HW   : Hardware_Addr_Type (1 .. 6)       := (others => 0);
   Last_Dst_Port : Port_Type                         := 0;
   Last_Item     : Ada.Streams.Stream_Element_Array (1 .. 2048);
   Last_Idx      : Ada.Streams.Stream_Element_Offset := Last_Item'First;
   Last_Iface    : Unbounded_String;

private

   type Test_Socket_Type is new Socket_Type with null record;

end Anet.Sockets.Mock;
