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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Interfaces.C.Strings;

with GNAT.OS_Lib;

package body Anet is

   -------------------------------------------------------------------------

   function Get_Errno_String return String
   is
      package C renames Interfaces.C;

      use type Interfaces.C.Strings.chars_ptr;

      function C_Strerror (Errnum : C.int) return C.Strings.chars_ptr;
      pragma Import (C, C_Strerror, "strerror");

      C_Msg : C.Strings.chars_ptr;
   begin
      C_Msg := C_Strerror (C.int (GNAT.OS_Lib.Errno));

      if C_Msg = C.Strings.Null_Ptr then
         return "";
      end if;

      return C.Strings.Value (Item => C_Msg);
   end Get_Errno_String;

   -------------------------------------------------------------------------

   function To_Byte (Str : Hex_Byte_Str) return Byte
   is
      Result : constant Byte := Byte'Value ("16#" & Str & "#");
   begin
      return Result;
   end To_Byte;

   -------------------------------------------------------------------------

   function To_Bytes (Str : String) return Byte_Array
   is
   begin
      return Bytes : Byte_Array (Str'Range) do
         for C in Str'Range loop
            Bytes (C) := Character'Pos (Str (C));
         end loop;
      end return;
   end To_Bytes;

   -------------------------------------------------------------------------

   function To_Hex (B : Byte) return String
   is
      Hex_To_Char : constant String (1 .. 16) := "0123456789ABCDEF";
   begin
      return Result : String (1 .. 2) do
         Result (1) := Hex_To_Char (Natural (B / 16) + 1);
         Result (2) := Hex_To_Char (Natural (B mod 16) + 1);
      end return;
   end To_Hex;

   -------------------------------------------------------------------------

   function To_Hex (Data : Ada.Streams.Stream_Element_Array) return String
   is
      use Ada.Strings.Unbounded;

      S : Unbounded_String;
   begin
      for B in Data'Range loop
         S := S & To_Hex (B => Byte (Data (B)));
      end loop;

      return To_String (S);
   end To_Hex;

   -------------------------------------------------------------------------

   function To_IPv4_Addr (Str : String) return IPv4_Addr_Type
   is
   begin
      if Str = "0.0.0.0" then
         return Any_Addr;
      end if;

      if Str = "255.255.255.255" then
         return Bcast_Addr;
      end if;

      if Str = "" then
         raise Constraint_Error with "Unable to convert empty string to IP";
      end if;

      if Ada.Strings.Fixed.Count (Source  => Str,
                                  Pattern => ".") /= 3
      then
         raise Constraint_Error with
           "Valid address string must contain 3 dots";
      end if;

      declare
         Result   : IPv4_Addr_Type;
         Left_Idx : Natural := Str'First;
         Dot_Idx  : Natural := Str'First;
      begin
         for I in IPv4_Addr_Type'Range loop
            Dot_Idx := Ada.Strings.Fixed.Index
              (From    => Left_Idx,
               Source  => Str,
               Pattern => ".");

            --  Check if we reached the end of the string.

            if Dot_Idx = 0 then
               Dot_Idx := Str'Last + 1;
            end if;

            Result (I) := Byte'Value (Str (Left_Idx .. Dot_Idx - 1));

            Left_Idx := Dot_Idx + 1;
            Dot_Idx  := Left_Idx;
         end loop;

         return Result;

      exception
         when others =>
            raise Constraint_Error with "Invalid octet: "
              & Str (Left_Idx .. Dot_Idx);
      end;
   end To_IPv4_Addr;

   -------------------------------------------------------------------------

   function To_IPv6_Addr (Str : String) return IPv6_Addr_Type
   is
   begin
      if Str = "" then
         raise Constraint_Error with "Unable to convert empty string to IP";
      end if;

      if Ada.Strings.Fixed.Count (Source  => Str,
                                  Pattern => ":") /= 7
      then
         raise Constraint_Error with
           "Valid IPv6 address string must contain 7 colons";
      end if;

      declare
         Result   : IPv6_Addr_Type;
         Left_Idx : Natural := Str'First;
         Dot_Idx  : Natural := Str'First;
      begin
         for I in IPv6_Addr_Type'First .. IPv6_Addr_Type'Length / 2 loop
            Dot_Idx := Ada.Strings.Fixed.Index
              (From    => Left_Idx,
               Source  => Str,
               Pattern => ":");

            --  Check if we reached the end of the string.

            if Dot_Idx = 0 then
               Dot_Idx := Str'Last + 1;
            end if;

            Result (I * 2 - 1) := To_Byte (Str (Left_Idx .. Left_Idx + 1));
            Result (I * 2)     := To_Byte
              (Str (Left_Idx + 2 .. Left_Idx + 3));

            Left_Idx := Dot_Idx + 1;
            Dot_Idx  := Left_Idx;
         end loop;

         return Result;

      exception
         when others =>
            raise Constraint_Error with "Invalid octet: "
              & Str (Left_Idx .. Dot_Idx);
      end;
   end To_IPv6_Addr;

   -------------------------------------------------------------------------

   function To_String (Bytes : Byte_Array) return String
   is
      Result : String (Bytes'Range);
      Char   : Character;
   begin
      for Index in Result'Range loop
         Char := Character'Val (Bytes (Index));
         if Char = ASCII.NUL then
            return Result (Result'First .. Index - 1);
         end if;

         Result (Index) := Char;
      end loop;

      return Result;
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Address : IPv4_Addr_Type) return String
   is
      Buffer : String (1 .. 16);
      Length : Natural := 1;

      procedure Append (B : Byte);
      --  Append decimal image of B to buffer.

      procedure Append (B : Byte)
      is
         Img : constant String  := B'Img;
         Len : constant Natural := Img'Length - 1;
      begin
         Buffer (Length .. Length + Len - 1) := Img (2 .. Img'Last);
         Length := Length + Len;
      end Append;
   begin
      for I in Address'Range loop
         Append (B => Address (I));

         if I /= Address'Last then
            Buffer (Length) := '.';
            Length := Length + 1;
         end if;
      end loop;

      return Buffer (1 .. Length - 1);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Address : IPv6_Addr_Type) return String
   is
      Buffer : String (1 .. 39);
      Length : Natural := 1;

      procedure Append (B : Byte);
      --  Append hexadecimal image of B to buffer.

      procedure Append (B : Byte)
      is
      begin
         Buffer (Length .. Length + 1) := To_Hex (B => B);
         Length := Length + 2;
      end Append;
   begin
      for I in Address'Range loop
         Append (B => Address (I));

         if Length mod 5 = 0 and then I /= Address'Last then
            Buffer (Length) := ':';
            Length          := Length + 1;
         end if;
      end loop;

      return Buffer (1 .. Length - 1);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Address : Hardware_Addr_Type) return String
   is
      Buffer : String (1 .. 3 * Address'Length - 1);
      Length : Natural := 1;

      procedure Append (B : Byte);
      --  Append hexadecimal image of B to buffer.

      procedure Append (B : Byte)
      is
      begin
         Buffer (Length .. Length + 1) := To_Hex (B => B);
         Length := Length + 2;
      end Append;
   begin
      for I in Address'Range loop
         Append (B => Address (I));

         if I /= Address'Last then
            Buffer (Length) := ':';
            Length          := Length + 1;
         end if;
      end loop;

      return Buffer (1 .. Length - 1);
   end To_String;

end Anet;
