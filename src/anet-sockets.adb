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

with Interfaces.C;

with Anet.Sockets.Thin;

package body Anet.Sockets is

   package C renames Interfaces.C;

   -------------------------------------------------------------------------

   procedure Close (Socket : in out Socket_Type)
   is
      use type C.int;

      Res : C.int;
   begin
      if Socket.Sock_FD /= -1 then
         Res := Thin.C_Close (C.int (Socket.Sock_FD));
         if Res = C_Failure then
            raise Socket_Error with "Unable to close socket: "
              & Get_Errno_String;
         end if;
         Socket.Sock_FD := -1;
      end if;
   end Close;

   -------------------------------------------------------------------------

   procedure Finalize (Socket : in out Socket_Type)
   is
   begin
      Socket_Type'Class (Socket).Close;
   end Finalize;

   -------------------------------------------------------------------------

   procedure Init
     (Socket : in out Socket_Type;
      Family :        Family_Type;
      Mode   :        Mode_Type)
   is
   begin
      Thin.Create_Socket (Socket => Socket.Sock_FD,
                          Family => Family,
                          Mode   => Mode);
   end Init;

   -------------------------------------------------------------------------

   procedure Listen
     (Socket  : Socket_Type;
      Backlog : Positive := 1)
   is
      use type C.int;

      Res : C.int;
   begin
      Res := Thin.C_Listen (Socket  => C.int (Socket.Sock_FD),
                            Backlog => C.int (Backlog));

      if Res = C_Failure then
         raise Socket_Error with "Unable to listen on socket with backlog"
           & Backlog'Img & " - " & Get_Errno_String;
      end if;
   end Listen;

   -------------------------------------------------------------------------

   procedure Receive
     (Socket :     Socket_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      Thin.Receive_Socket (Socket => Socket.Sock_FD,
                           Data   => Item,
                           Last   => Last);
   end Receive;

   -------------------------------------------------------------------------

   procedure Send
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Len : Ada.Streams.Stream_Element_Offset;
   begin
      Thin.Send_Socket
        (Socket => Socket.Sock_FD,
         Data   => Item,
         Last   => Len);

      if Len /= Item'Length then
         raise Socket_Error with "Incomplete send operation on socket"
         & ", only" & Len'Img & " of" & Item'Length'Img & " bytes sent";
      end if;
   end Send;

   -------------------------------------------------------------------------

   procedure Set_Socket_Option
     (Socket : Socket_Type;
      Option : Option_Name_Bool;
      Value  : Boolean)
   is
   begin
      Thin.Set_Socket_Option
        (Socket => Socket.Sock_FD,
         Option => Option,
         Value  => Value);
   end Set_Socket_Option;

   -------------------------------------------------------------------------

   procedure Set_Socket_Option
     (Socket : Socket_Type;
      Option : Option_Name_Str;
      Value  : String)
   is
   begin
      Thin.Set_Socket_Option
        (Socket => Socket.Sock_FD,
         Option => Option,
         Value  => Value);
   end Set_Socket_Option;

end Anet.Sockets;
