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

with Interfaces.C;

with Anet.OS;

package body Anet.Sockets.Thin.Unix is

   package C renames Interfaces.C;

   -------------------------------------------------------------------------

   procedure Bind
     (Socket : Integer;
      Path   : Types.Unix_Path_Type)
   is
      use type C.int;

      Res    : C.int;
      C_Path : constant C.char_array := C.To_C (String (Path));
      Value  : Sockaddr_Un_Type;
   begin
      OS.Delete_File (Filename => String (Path));

      Value.Pathname (1 .. C_Path'Length) := C_Path;

      Res := C_Bind (S       => C.int (Socket),
                     Name    => Value'Address,
                     Namelen => Value'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable to bind unix socket to path "
           & String (Path) & " - " & Get_Errno_String;
      end if;
   end Bind;

   -------------------------------------------------------------------------

   procedure Connect
     (Socket : Integer;
      Path   : Types.Unix_Path_Type)
   is
      use type C.int;

      Res    : C.int;
      C_Path : constant C.char_array := C.To_C (String (Path));
      Value  : Sockaddr_Un_Type;
   begin
      Value.Pathname (1 .. C_Path'Length) := C_Path;

      Res := C_Connect (S       => C.int (Socket),
                        Name    => Value'Address,
                        Namelen => Value'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable to connect unix socket to path "
           & String (Path) & " - " & Get_Errno_String;
      end if;
   end Connect;

   -------------------------------------------------------------------------

   procedure Receive
     (Socket :     Integer;
      Data   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      use type Interfaces.C.int;
      use type Ada.Streams.Stream_Element_Offset;

      function C_Recv
        (S       : C.int;
         Msg     : System.Address;
         Len     : C.int;
         Flags   : C.int)
         return C.int;
      pragma Import (C, C_Recv, "recv");

      Res : C.int;
   begin
      Res := C_Recv (S     => C.int (Socket),
                     Msg   => Data'Address,
                     Len   => Data'Length,
                     Flags => 0);

      if Res = C_Failure then
         raise Socket_Error with "Error receiving packet data: "
           & Get_Errno_String;
      end if;

      Last := Data'First + Ada.Streams.Stream_Element_Offset (Res - 1);
   end Receive;

end Anet.Sockets.Thin.Unix;
