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

package Anet.Sockets.Tasking is

   type Count_Type is mod System.Max_Binary_Modulus;

   type Rcv_Item_Callback is not null access procedure
     (Item : Ada.Streams.Stream_Element_Array;
      Src  : Sender_Info_Type);
   --  Data reception callback procedure. The Item argument contains the
   --  received data, the Src argument identifies the sender of the data.

   type Receiver_Type (S : not null access Socket_Type) is limited private;
   --  Listens for incoming data on the given socket and processes it by
   --  calling the registered listen callback.

   function Get_Rcv_Msg_Count (Receiver : Receiver_Type) return Count_Type;
   --  Returns the number of received and processed messages.

   procedure Listen
     (Receiver : in out Receiver_Type;
      Callback :        Rcv_Item_Callback);
   --  Start listening for data on given socket. The given callback is
   --  asynchronously executed upon data reception. Call stop procedure to
   --  properly shutdown the listener.

   procedure Stop (Receiver : in out Receiver_Type);
   --  Stop listening for data.

private

   protected type Trigger_Type is

      procedure Activate;
      --  Activate trigger.

      procedure Shutdown;
      --  Signal shutdown to all tasks waiting on the Stop entry.

      entry Stop;
      --  Entry used for listener ATC.

      procedure Signal_Termination;
      --  Signal termination to all tasks waiting on the Wait_For_Termination
      --  entry.

      entry Wait_For_Termination;
      --  Wait until termination is signaled.

   private
      Shutdown_Requested : Boolean := False;
      Is_Terminated      : Boolean := True;
   end Trigger_Type;
   --  This trigger is used to terminate the receiver task by means of ATC.

   task type Receiver_Task (Parent : not null access Receiver_Type) is

      entry Listen (Cb : Rcv_Item_Callback);
      --  Start listening for data on parent's socket. The callback procedure
      --  is called upon reception of new data.

   end Receiver_Task;

   type Receiver_Type (S : not null access Socket_Type) is limited record
      Item_Count : Count_Type := 0;
      pragma Atomic (Item_Count);

      Trigger : Trigger_Type;
      R_Task  : Receiver_Task (Parent => Receiver_Type'Access);
   end record;

end Anet.Sockets.Tasking;
