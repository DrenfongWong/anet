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

with Anet.Util;

package body Anet_Util_Tests is

   use Ada.Streams;
   use Ahven;
   use Anet.Util;

   -------------------------------------------------------------------------

   procedure Calculate_One_Complement
   is
      use type Anet.Double_Byte;

      Data1  : constant Stream_Element_Array
        := (16#12#, 16#ff#, 16#ab#, 16#24#, 16#45#, 16#92#, 16#12#, 16#12#);
      Data2  : constant Stream_Element_Array (1 .. 6)  := (others => 0);
      Data3  : constant Stream_Element_Array (1 .. 12) := (others => 16#ff#);
      IP_Hdr : constant Stream_Element_Array
        := (16#45#, 16#00#, 16#01#, 16#4b#, 16#78#, 16#12#, 16#00#, 16#00#,
            16#40#, 16#11#, 16#00#, 16#00#, 16#c0#, 16#a8#, 16#ef#, 16#01#,
            16#c0#, 16#a8#, 16#ef#, 16#77#);
   begin
      Assert (Condition => Calculate_One_Complement (Data => Data1) = 59959,
              Message   => "One's complement mismatch1");
      Assert (Condition => Calculate_One_Complement (Data => Data2) = 65535,
              Message   => "One's complement mismatch2");
      Assert (Condition => Calculate_One_Complement (Data => Data3) = 0,
              Message   => "One's complement mismatch3");
      Assert (Condition => Calculate_One_Complement (Data => IP_Hdr) = 41413,
              Message   => "One's complement mismatch4");
   end Calculate_One_Complement;

   -------------------------------------------------------------------------

   procedure Calculate_One_Complement_Uneven
   is
      use type Anet.Double_Byte;

      Data : constant Stream_Element_Array
        := (16#12#, 16#ff#, 16#ab#, 16#45#, 16#92#, 16#12#, 16#fa#);
   begin
      Assert (Condition => Calculate_One_Complement (Data => Data) = 46503,
              Message   => "One's complement mismatch");
   end Calculate_One_Complement_Uneven;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Tests for Util package");
      T.Add_Test_Routine
        (Routine => Calculate_One_Complement'Access,
         Name    => "Calculate one's complement");
      T.Add_Test_Routine
        (Routine => Calculate_One_Complement_Uneven'Access,
         Name    => "Calculate one's complement (uneven)");
   end Initialize;

end Anet_Util_Tests;
