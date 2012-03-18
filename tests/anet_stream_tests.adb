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

with Ada.Streams;
with Ada.Strings.Unbounded;

with Anet.Streams;
with Anet.Sockets;

package body Anet_Stream_Tests is

   use Ada.Strings.Unbounded;
   use Ahven;
   use Anet;

   type Test_Record is record
      A : Natural;
      B : String (1 .. 4);
      C : Float;
      D : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   --  Test record.

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Tests for in-memory streams");
      T.Add_Test_Routine
        (Routine => Write_Read_Records'Access,
         Name    => "Write/read record types");
      T.Add_Test_Routine
        (Routine => Send_Records'Access,
         Name    => "Send records over socket");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Send_Records
   is
      use Anet.Sockets;

      Path   : constant String := "obj/mysock";
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;

      Server, Client : Sockets.Socket_Type;

      There : Test_Record;
      Here  : constant Test_Record
        := (A => 12333,
            B => "foob",
            C => 13.5,
            D => To_Unbounded_String ("some string here"));

      task Receiver is
         entry Ready;
         entry Done;
      end Receiver;

      task body Receiver is
         Sender : Socket_Addr_Type (Family => Family_Unix);
         S1     : aliased Streams.Memory_Stream_Type;
      begin
         Server.Create (Family => Family_Unix,
                        Mode   => Datagram_Socket);
         Server.Bind_Unix (Path => Unix_Path_Type (Path));
         accept Ready;

         Server.Receive (Src  => Sender,
                         Item => Buffer,
                         Last => Last);
         S1.Set_Buffer (Buffer (Buffer'First .. Last));
         Test_Record'Read (S1'Access, There);

         accept Done;
      end Receiver;

      S2 : aliased Streams.Memory_Stream_Type;
   begin
      Receiver.Ready;

      Client.Create (Family => Family_Unix,
                     Mode   => Datagram_Socket);
      Client.Connect
        (Dst => (Family => Family_Unix,
                 Path   => To_Unbounded_String (Path)));

      Test_Record'Write (S2'Access, Here);
      Client.Send (Item => S2.Get_Buffer);

      select
         delay 3.0;
      then abort
         Receiver.Done;
      end select;

      Assert (Condition => Here = There,
              Message   => "Records mismatch");

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         raise;
   end Send_Records;

   -------------------------------------------------------------------------

   procedure Write_Read_Records
   is
      S1 : aliased Streams.Memory_Stream_Type;
      S2 : aliased Streams.Memory_Stream_Type;
      R1 : Test_Record;
      R2 : Test_Record;
      T  : constant Test_Record := (A => 123,
                                    B => "abcd",
                                    C => 3.5,
                                    D => To_Unbounded_String ("some string"));
   begin
      Test_Record'Write (S1'Access, T);

      Test_Record'Read (S1'Access, R1);
      Assert (Condition => T = R1,
              Message   => "T /= R1");

      S2.Set_Buffer (Buffer => S1.Get_Buffer);
      Test_Record'Read (S2'Access, R2);
      Assert (Condition => T = R2,
              Message   => "T /= R2");
   end Write_Read_Records;

end Anet_Stream_Tests;
