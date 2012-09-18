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

with Ada.Direct_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Anet;

with Anet.OS;

package body Test_Utils is

   package D_IO is new Ada.Direct_IO (Element_Type => Character);

   Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
   Last   : Ada.Streams.Stream_Element_Offset;

   -------------------------------------------------------------------------

   procedure Dump
     (Data : Ada.Streams.Stream_Element_Array;
      Src  : Anet.Sockets.Inet.UDPv4_Sockaddr_Type)
   is
      pragma Unreferenced (Src);
   begin
      Buffer (Buffer'First .. Data'Length) := Data;
      Last                                 := Data'Length;
   end Dump;

   -------------------------------------------------------------------------

   procedure Dump
     (Data : Ada.Streams.Stream_Element_Array;
      Src  : Anet.Sockets.Inet.UDPv6_Sockaddr_Type)
   is
      pragma Unreferenced (Src);
   begin
      Dump (Data => Data,
            Src  => Anet.Sockets.Inet.UDPv4_Sockaddr_Type'(others => <>));
   end Dump;

   -------------------------------------------------------------------------

   function Equal_Files
     (Filename1 : String;
      Filename2 : String)
      return Boolean
   is
      use type D_IO.Count;

      procedure Open_File
        (Filename :     String;
         File     : out D_IO.File_Type);
      --  Open file specified by filename.

      procedure Open_File
        (Filename :     String;
         File     : out D_IO.File_Type)
      is
      begin
         D_IO.Open (File => File,
                    Mode => D_IO.In_File,
                    Name => Filename,
                    Form => "shared=no");

      exception
         when others =>
            raise Open_File_Error with
              "Unable to open file '" & Filename & "'";
      end Open_File;

      File1, File2 : D_IO.File_Type;
      Char1, Char2 : Character;
      Result       : Boolean := True;
   begin
      Open_File (Filename => Filename1,
                 File     => File1);
      Open_File (Filename => Filename2,
                 File     => File2);

      if D_IO.Size (File1) /= D_IO.Size (File2) then
         D_IO.Close (File => File1);
         D_IO.Close (File => File2);
         return False;
      end if;

      while not D_IO.End_Of_File (File => File1) loop

         --  Read one byte from both files.

         D_IO.Read (File => File1,
                    Item => Char1);
         D_IO.Read (File => File2,
                    Item => Char2);

         if Char1 /= Char2 then
            Result := False;
         end if;
      end loop;

      D_IO.Close (File => File1);
      D_IO.Close (File => File2);

      return Result;
   end Equal_Files;

   -------------------------------------------------------------------------

   function Get_Dump return Ada.Streams.Stream_Element_Array
   is
   begin
      return Buffer (Buffer'First .. Last);
   end Get_Dump;

   -------------------------------------------------------------------------

   procedure Raise_Error
     (Data : Ada.Streams.Stream_Element_Array;
      Src  : Anet.Sockets.Inet.UDPv4_Sockaddr_Type)
   is
   begin
      raise Constraint_Error with "DO NOT PANIC: Explicit raise";
   end Raise_Error;

   -------------------------------------------------------------------------

   procedure Send_Data
     (Dst      : Anet.Sockets.Socket_Addr_Type := Test_Addr_V4;
      Mode     : String := "UDP-DATAGRAM";
      Filename : String)
   is
      use Anet;
      use Ada.Strings.Unbounded;
      use type Anet.Sockets.Family_Type;

      Port_Str : Unbounded_String;
      IP_Str   : Unbounded_String;
   begin
      if Dst.Family = Sockets.Family_Inet then
         IP_Str   := To_Unbounded_String (To_String (Address => Dst.Addr_V4));
         Port_Str := To_Unbounded_String
           (Source => Ada.Strings.Fixed.Trim (Source => Dst.Port_V4'Img,
                                              Side   => Ada.Strings.Left));
      elsif Dst.Family = Sockets.Family_Inet6 then
         IP_Str   := To_Unbounded_String
           ("[" & To_String (Address => Dst.Addr_V6) & "]");
         Port_Str := To_Unbounded_String
           (Source => Ada.Strings.Fixed.Trim (Source => Dst.Port_V6'Img,
                                              Side   => Ada.Strings.Left));
      else
         raise Constraint_Error with "Invalid family type " & Dst.Family'Img;
      end if;

      OS.Execute (Command => "socat " & Filename & " " & Mode & ":"
                  & To_String (IP_Str) & ":" & To_String (Port_Str));
   end Send_Data;

end Test_Utils;
