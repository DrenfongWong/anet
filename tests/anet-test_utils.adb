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

with Ada.Direct_IO;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Exceptions;

with Anet;

with Anet.OS;

package body Anet.Test_Utils is

   package D_IO is new Ada.Direct_IO (Element_Type => Character);

   -------------------------------------------------------------------------

   procedure Dump
     (Data     : Ada.Streams.Stream_Element_Array;
      Filename : String)
   is
      use Ada.Streams.Stream_IO;

      File : File_Type;
   begin
      Create (File => File,
              Mode => Out_File,
              Name => Filename);
      Write (File => File,
             Item => Data);
      Close (File => File);

   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Error dumping data:");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (X => E));
   end Dump;

   -------------------------------------------------------------------------

   procedure Dump
     (Data : Ada.Streams.Stream_Element_Array;
      Src  : Anet.Sockets.Sender_Info_Type)
   is
      pragma Unreferenced (Src);
   begin
      Dump (Data     => Data,
            Filename => Dump_File);
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
              "Unable to open file '" & Filename1 & "'";
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

   procedure Send_Data
     (Dst_IP   : Anet.Sockets.IP_Addr_Type := Anet.Sockets.Loopback_Addr_V4;
      Port     : Anet.Port_Type            := Listen_Port;
      Filename : String)
   is
      IP_Str : constant String := Anet.Sockets.To_String (Address => Dst_IP);
      Cmd    : constant String := "cat " & Filename
        & " | nc6 -q 0 -u " & IP_Str & Port'Img;
   begin
      OS.Execute (Command => Cmd);
   end Send_Data;

end Anet.Test_Utils;
