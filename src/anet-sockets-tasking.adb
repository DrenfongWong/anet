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
      Src  : Socket_Addr_Type) is null;
   --  This placeholder callback is needed for initialization of data reception
   --  callbacks.

   procedure No_Op_Cb
     (E         :        Ada.Exceptions.Exception_Occurrence;
      Stop_Flag : in out Boolean) is null;
   --  This placeholder callback is needed for initialization of error handling
   --  callbacks.

   -------------------------------------------------------------------------

   function Get_Rcv_Msg_Count
     (Receiver : Dgram_Receiver_Type)
      return Count_Type
   is
   begin
      return Receiver.Item_Count;
   end Get_Rcv_Msg_Count;

   -------------------------------------------------------------------------

   function Is_Listening (Receiver : Dgram_Receiver_Type) return Boolean
   is
   begin
      return Receiver.Trigger.Is_Listening;
   end Is_Listening;

   -------------------------------------------------------------------------

   procedure Listen
     (Receiver : in out Dgram_Receiver_Type;
      Callback :        Rcv_Item_Callback)
   is
   begin
      Receiver.Trigger.Activate;
      Receiver.R_Task.Listen (Cb => Callback);
   end Listen;

   -------------------------------------------------------------------------

   procedure Register_Error_Handler
     (Receiver : in out Dgram_Receiver_Type;
      Callback :        Error_Handler_Callback)
   is
   begin
      Receiver.R_Task.Set_Error_Handler (Cb => Callback);
   end Register_Error_Handler;

   -------------------------------------------------------------------------

   procedure Stop (Receiver : in out Dgram_Receiver_Type)
   is
   begin
      Receiver.Trigger.Shutdown;
      Receiver.Trigger.Wait_For_Termination;
   end Stop;

   -------------------------------------------------------------------------

   task body Receiver_Task
   is
      Data_Callback  : Rcv_Item_Callback      := Empty_Cb'Access;
      Error_Callback : Error_Handler_Callback := No_Op_Cb'Access;
      Stop           : Boolean                := False;
   begin
      Setup_Loop :
      loop
         select
            accept Listen (Cb : Rcv_Item_Callback)
            do
               Data_Callback := Cb;
            end Listen;

            exit Setup_Loop;

         or
            accept Set_Error_Handler (Cb : Error_Handler_Callback)
            do
               Error_Callback := Cb;
            end Set_Error_Handler;

         or

            terminate;
         end select;
      end loop Setup_Loop;

      select
         Parent.Trigger.Stop;
      then abort
         Reception_Loop :
         loop
            declare
               Sender : Socket_Addr_Type (Family => Parent.S.Address.Family);
               Buffer : Ada.Streams.Stream_Element_Array (1 .. 2048);
               Last   : Ada.Streams.Stream_Element_Offset;
            begin
               Parent.S.all.Receive (Src  => Sender,
                                     Item => Buffer,
                                     Last => Last);
               Data_Callback (Item => Buffer (Buffer'First .. Last),
                              Src  => Sender);
               Parent.Item_Count := Parent.Item_Count + 1;

            exception
               when Ex : others =>
                  Error_Callback (E         => Ex,
                                  Stop_Flag => Stop);
                  if Stop then
                     exit Reception_Loop;
                  end if;
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

      function Is_Listening return Boolean
      is
      begin
         return not Is_Terminated;
      end Is_Listening;

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
