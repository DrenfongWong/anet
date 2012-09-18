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

package body Anet.Sockets.Thin.Inet is

   package C renames Interfaces.C;

   -------------------------------------------------------------------------

   procedure Bind
     (Socket  :     Integer;
      Address :     Sockaddr_In_Type;
      Success : out Boolean)
   is
      use type C.int;

      Res : C.int;
   begin
      Res := C_Bind (S       => C.int (Socket),
                     Name    => Address'Address,
                     Namelen => Address'Size / 8);
      Success := Res /= C_Failure;
   end Bind;

   -------------------------------------------------------------------------

   procedure Connect
     (Socket  :     Integer;
      Dst     :     Sockaddr_In_Type;
      Success : out Boolean)
   is
      use type C.int;

      Res : C.int;
   begin
      Res := C_Connect (S       => C.int (Socket),
                        Name    => Dst'Address,
                        Namelen => Dst'Size / 8);
      Success := Res /= C_Failure;
   end Connect;

   -------------------------------------------------------------------------

   procedure Receive
     (Socket :     Integer;
      Data   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      Source : out Sockaddr_In_Type)
   is
      use type Interfaces.C.int;
      use type Ada.Streams.Stream_Element_Offset;

      Res : C.int;
      Len : aliased C.int := Source'Size / 8;
   begin
      Res := C_Recvfrom (S       => C.int (Socket),
                         Msg     => Data'Address,
                         Len     => Data'Length,
                         Flags   => 0,
                         From    => Source'Address,
                         Fromlen => Len'Access);

      if Res = C_Failure then
         raise Socket_Error with "Error receiving data: " & Get_Errno_String;
      end if;

      if Len = 0 then
         raise Socket_Error with "No address information received";
      end if;

      Last := Data'First + Ada.Streams.Stream_Element_Offset (Res - 1);
   end Receive;

   -------------------------------------------------------------------------

   procedure Send
     (Socket  :     Integer;
      Data    :     Ada.Streams.Stream_Element_Array;
      Last    : out Ada.Streams.Stream_Element_Offset;
      Dst     :     Sockaddr_In_Type;
      Success : out Boolean)
   is
      use type Interfaces.C.int;
      use type Ada.Streams.Stream_Element_Offset;

      Res : C.int;
   begin
      Res := C_Sendto (S     => C.int (Socket),
                       Buf   => Data'Address,
                       Len   => Data'Length,
                       Flags => 0,
                       To    => Dst'Address,
                       Tolen => Dst'Size / 8);
      Success := Res /= C_Failure;
      Last    := Data'First + Ada.Streams.Stream_Element_Offset (Res - 1);
   end Send;

end Anet.Sockets.Thin.Inet;
