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

package Anet.OS is

   procedure Execute (Command : String);
   --  Execute given command with /bin/sh.

   function Read_File
     (Filename : String)
      return Ada.Streams.Stream_Element_Array;
   --  Read file given by filename and return binary file content.

   procedure Delete_File
     (Filename       : String;
      Ignore_Missing : Boolean := True);
   --  Delete a file given by filename string. Ignore missing specifies if
   --  trying to delete a nonexistent file should raise an IO error exception.

   Command_Failed : exception;
   IO_Error       : exception;

end Anet.OS;
