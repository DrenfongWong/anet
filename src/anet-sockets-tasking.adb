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

package body Anet.Sockets.Tasking is

   procedure Empty_Cb
     (Item : Ada.Streams.Stream_Element_Array;
      Src  : Sender_Info_Type) is null;
   --  This placeholder callback is needed for initialization of data reception
   --  callbacks.

   -------------------------------------------------------------------------

   function Get_Rcv_Msg_Count (Receiver : Receiver_Type) return Count_Type
   is
   begin
      return Receiver.Item_Count;
   end Get_Rcv_Msg_Count;

   -------------------------------------------------------------------------

   procedure Listen
     (Receiver : in out Receiver_Type;
      Callback :        Rcv_Item_Callback)
   is
   begin
      Receiver.Trigger.Activate;
      Receiver.R_Task.Listen (Cb => Callback);
   end Listen;

   -------------------------------------------------------------------------

   procedure Stop (Receiver : in out Receiver_Type)
   is
   begin
      Receiver.Trigger.Shutdown;
      Receiver.Trigger.Wait_For_Termination;
   end Stop;

   -------------------------------------------------------------------------

   task body Receiver_Task
   is
      Callback : Rcv_Item_Callback := Empty_Cb'Access;
   begin
      select
         accept Listen (Cb : Rcv_Item_Callback)
         do
            Callback := Cb;
         end Listen;

      or
         terminate;
      end select;

      select
         Parent.Trigger.Stop;
      then abort
         Reception_Loop :
         loop
            declare
               Sender : Sender_Info_Type;
               Buffer : Ada.Streams.Stream_Element_Array (1 .. 2048);
               Last   : Ada.Streams.Stream_Element_Offset;
            begin
               Parent.S.all.Receive (Src  => Sender,
                                     Item => Buffer,
                                     Last => Last);

               Callback (Item => Buffer (Buffer'First .. Last),
                         Src  => Sender);
               Parent.Item_Count := Parent.Item_Count + 1;

            exception
               when Sockets.Socket_Error => exit Reception_Loop;
               when others               => null;
            end;
         end loop Reception_Loop;
      end select;
      Parent.Trigger.Signal_Termination;
   end Receiver_Task;

   -------------------------------------------------------------------------

   protected body Trigger_Type
   is

      ----------------------------------------------------------------------

      procedure Activate
      is
      begin
         Is_Terminated := False;
      end Activate;

      ----------------------------------------------------------------------

      procedure Shutdown
      is
      begin
         Shutdown_Requested := True;
      end Shutdown;

      ----------------------------------------------------------------------

      procedure Signal_Termination
      is
      begin
         Is_Terminated := True;
      end Signal_Termination;

      ----------------------------------------------------------------------

      entry Stop when Shutdown_Requested
      is
      begin
         null;
      end Stop;

      ----------------------------------------------------------------------

      entry Wait_For_Termination when Is_Terminated
      is
      begin
         null;
      end Wait_For_Termination;

   end Trigger_Type;

end Anet.Sockets.Tasking;
