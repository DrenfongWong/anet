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

package body Anet.Sockets.Mock is

   -------------------------------------------------------------------------

   procedure Clear
   is
   begin
      Send_Count    := 0;
      Last_Dst_IP   := Any_Addr_V4;
      Last_Dst_Port := 0;
      Last_Item     := (others => 0);
      Last_Idx      := Last_Item'First;
   end Clear;

   -------------------------------------------------------------------------

   function Get_Last_Iface return String
   is
   begin
      return To_String (Last_Iface);
   end Get_Last_Iface;

   -------------------------------------------------------------------------

   procedure Send
     (Socket   : Test_Socket_Type;
      Item     : Ada.Streams.Stream_Element_Array;
      Dst_IP   : IP_Addr_Type;
      Dst_Port : Port_Type)
   is
      pragma Unreferenced (Socket);
   begin
      Last_Item (Last_Item'First .. Item'Length) := Item;

      Send_Count    := Send_Count + 1;
      Last_Dst_IP   := Dst_IP;
      Last_Dst_Port := Dst_Port;
      Last_Idx      := Item'Length;
   end Send;

   -------------------------------------------------------------------------

   procedure Send
     (Socket : Test_Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      To     : Hardware_Addr_Type;
      Iface  : String)
   is
      use type Ada.Streams.Stream_Element_Offset;

      pragma Unreferenced (Socket);
   begin
      Last_Dst_IP := (Family  => Family_Inet,
                      Addr_V4 => (1 => Byte (Item (17)),
                                  2 => Byte (Item (18)),
                                  3 => Byte (Item (19)),
                                  4 => Byte (Item (20))));

      Last_Item (Last_Item'First .. Item'Length) := Item;

      Send_Count  := Send_Count + 1;
      Last_Idx    := Item'Length;
      Last_Dst_HW := To;
      Last_Iface  := To_Unbounded_String (Iface);
   end Send;

end Anet.Sockets.Mock;
