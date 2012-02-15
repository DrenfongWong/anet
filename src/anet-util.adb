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

with Interfaces;

package body Anet.Util is

   use Ada.Streams;

   -------------------------------------------------------------------------

   function Calculate_One_Complement
     (Data : Ada.Streams.Stream_Element_Array)
      return Double_Byte
   is
      function Make_Even
        (Data : Stream_Element_Array)
         return Stream_Element_Array;
      --  Pad data array with a nil element if it has an uneven number of
      --  elements.

      function Make_Even
        (Data : Stream_Element_Array)
         return Stream_Element_Array
      is
      begin
         if Data'Length mod 2 = 0 then
            return Data;
         end if;

         return D : Stream_Element_Array (Data'First .. Data'Last + 1) do
            D (Data'Range) := Data;
            D (D'Last)     := 0;
         end return;
      end Make_Even;

      use Interfaces;

      Sum  : Unsigned_32                   := 0;
      Even : constant Stream_Element_Array := Make_Even (Data => Data);
      Idx  : Stream_Element_Offset         := Even'First;
   begin
      loop
         Sum := Sum + Unsigned_32 (Shift_Left
           (Value  => Unsigned_16 (Even (Idx)),
            Amount => 8));
         Sum := Sum + Unsigned_32 (Even (Idx + 1));

         Idx := Idx + 2;
         exit when Idx > Even'Last;
      end loop;

      loop
         declare
            Carries : constant Unsigned_32
              := Shift_Right (Value  => Sum,
                              Amount => 16);
         begin
            exit when Carries < 1;

            Sum := (Sum and 16#ffff#) + Carries;
         end;
      end loop;

      Sum := (not Sum) mod 2 ** 16;

      return Double_Byte (Sum);
   end Calculate_One_Complement;

end Anet.Util;
