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

with Ada.Streams;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Directories;

with Anet.OS;
with Anet.Sockets.Tasking;
with Anet.Test_Utils;

package body Anet_Socket_Tests is

   use Ahven;
   use Anet;
   use Anet.Sockets;

   procedure Error_Handler
     (E         :        Ada.Exceptions.Exception_Occurrence;
      Stop_Flag : in out Boolean);
   --  Receiver error handler callback for testing purposes. It ignores the
   --  exception and tells the receiver to terminate by setting the stop flag.

   task type Command_Task (Command : access constant String) is
      entry Wait;
   end Command_Task;
   --  Command task. Executes given command and Waits for rendezvous.

   -------------------------------------------------------------------------

   task body Command_Task
   is
   begin
      OS.Execute (Command => Command.all);
      accept Wait;
   end Command_Task;

   -------------------------------------------------------------------------

   procedure Error_Callbacks
   is
      Sock : aliased Socket_Type;
   begin
      Sock.Create (Family => Family_Inet,
                   Mode   => Datagram_Socket);
      Sock.Bind (Address => Test_Utils.Test_Addr_V4);

      declare
         R : Tasking.Receiver_Type (S => Sock'Access);
      begin
         Tasking.Listen (Receiver => R,
                         Callback => Test_Utils.Raise_Error'Access);

         Test_Utils.Send_Data (Filename => "data/chunk1.dat");

         --  By default all errors should be ignored

         Assert (Condition => Tasking.Is_Listening (Receiver => R),
                 Message   => "Receiver not listening");

         Tasking.Stop (Receiver => R);

      exception
         when others =>
            Tasking.Stop (Receiver => R);
            raise;
      end;

      declare
         R : Tasking.Receiver_Type (S => Sock'Access);
      begin
         Tasking.Register_Error_Handler (Receiver => R,
                                         Callback => Error_Handler'Access);
         Tasking.Listen (Receiver => R,
                         Callback => Test_Utils.Raise_Error'Access);

         Test_Utils.Send_Data (Filename => "data/chunk1.dat");

         Assert (Condition => not Tasking.Is_Listening (Receiver => R),
                 Message   => "Receiver still listening");

      exception
         when others =>
            Tasking.Stop (Receiver => R);
            raise;
      end;

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         OS.Delete_File (Filename => Test_Utils. Dump_File);
         raise;
   end Error_Callbacks;

   -------------------------------------------------------------------------

   procedure Error_Handler
     (E         :        Ada.Exceptions.Exception_Occurrence;
      Stop_Flag : in out Boolean)
   is
      pragma Unreferenced (E);
   begin
      Stop_Flag := True;
   end Error_Handler;

   -------------------------------------------------------------------------

   procedure Get_Loopback_Interface_Index
   is
   begin
      declare
         Index : Positive;
         pragma Unreferenced (Index);
      begin
         Index := Get_Iface_Index (Name => "nonexistent");
         Fail (Message => "Expected socket error (nonexistent)");

      exception
         when Socket_Error => null;
      end;

      Assert (Condition => Get_Iface_Index (Name => "lo") = 1,
              Message   => "Loopback index not 1");
   end Get_Loopback_Interface_Index;

   -------------------------------------------------------------------------

   procedure Get_Loopback_Interface_IP
   is
   begin
      Assert (Condition => Get_Iface_IP (Name => "lo") = Loopback_Addr_V4,
              Message   => "Loopback IP not 127.0.0.1");
   end Get_Loopback_Interface_IP;

   -------------------------------------------------------------------------

   procedure Get_Loopback_Interface_Mac
   is
      Ref_Mac : constant Hardware_Addr_Type (1 .. 6) := (others => 0);
   begin
      Assert (Condition => Get_Iface_Mac (Name => "lo") = Ref_Mac,
              Message   => "Loopback Mac not zero");

      declare
         Mac : Hardware_Addr_Type (1 .. 6);
         pragma Unreferenced (Mac);
      begin
         Mac := Get_Iface_Mac (Name => "nonexistent");
         Fail (Message => "Expected socket error (nonexistent)");

      exception
         when Socket_Error => null;
      end;
   end Get_Loopback_Interface_Mac;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Tests for Sockets package");
      T.Add_Test_Routine
        (Routine => Send_V4_Datagram'Access,
         Name    => "Send data (IPv4, datagram)");
      T.Add_Test_Routine
        (Routine => Send_V6'Access,
         Name    => "Send data (IPv6)");
      T.Add_Test_Routine
        (Routine => Send_Multicast_V6'Access,
         Name    => "Send data (IPv6 multicast)");
      T.Add_Test_Routine
        (Routine => Send_Unix_Stream'Access,
         Name    => "Send data (Unix, stream)");
      T.Add_Test_Routine
        (Routine => Send_Unix_Datagram'Access,
         Name    => "Send data (Unix, datagram)");
      T.Add_Test_Routine
        (Routine => Receive_V4_Stream'Access,
         Name    => "Receive data (IPv4, stream)");
      T.Add_Test_Routine
        (Routine => Receive_V4_Datagram'Access,
         Name    => "Receive data (IPv4, datagram)");
      T.Add_Test_Routine
        (Routine => Receive_V6'Access,
         Name    => "Receive data (IPv6)");
      T.Add_Test_Routine
        (Routine => Receive_Multicast_V6'Access,
         Name    => "Receive data (IPv6 multicast)");
      T.Add_Test_Routine
        (Routine => Receive_Unix_Stream'Access,
         Name    => "Receive data (Unix, stream)");
      T.Add_Test_Routine
        (Routine => Receive_Unix_Datagram'Access,
         Name    => "Receive data (Unix, datagram)");
      T.Add_Test_Routine
        (Routine => Unix_Delete_Socket'Access,
         Name    => "Unix socket removal");
      T.Add_Test_Routine
        (Routine => Listen_Callbacks'Access,
         Name    => "Data reception callback handling");
      T.Add_Test_Routine
        (Routine => Error_Callbacks'Access,
         Name    => "Error callback handling");
      T.Add_Test_Routine
        (Routine => Get_Loopback_Interface_Index'Access,
         Name    => "Get iface index for loopback");
      T.Add_Test_Routine
        (Routine => Get_Loopback_Interface_Mac'Access,
         Name    => "Get iface hw addr for loopback");
      T.Add_Test_Routine
        (Routine => Get_Loopback_Interface_IP'Access,
         Name    => "Get iface IP addr for loopback");
      T.Add_Test_Routine
        (Routine => Socket_Addr_To_String'Access,
         Name    => "Socket address to string conversion");
      T.Add_Test_Routine
        (Routine => Valid_Iface_Names'Access,
         Name    => "Interface name validation");
      T.Add_Test_Routine
        (Routine => Valid_Unix_Paths'Access,
         Name    => "Unix path validation");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Listen_Callbacks
   is
      use type Anet.Sockets.Tasking.Count_Type;

      C    : Tasking.Count_Type := 0;
      Sock : aliased Socket_Type;
      R    : Tasking.Receiver_Type (S => Sock'Access);
   begin
      Sock.Create (Family => Family_Inet,
                   Mode   => Datagram_Socket);
      Sock.Bind (Address => Test_Utils.Test_Addr_V4);
      Tasking.Listen (Receiver => R,
                      Callback => Test_Utils.Dump'Access);

      Assert (Condition => Tasking.Is_Listening (Receiver => R),
              Message   => "Receiver not listening");

      Test_Utils.Send_Data (Filename => "data/chunk1.dat");

      for I in 1 .. 30 loop
         C := Tasking.Get_Rcv_Msg_Count (Receiver => R);
         exit when C > 0;
         delay 0.1;
      end loop;

      Tasking.Stop (Receiver => R);

      Assert (Condition => not Tasking.Is_Listening (Receiver => R),
              Message   => "Receiver still listening");

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         Tasking.Stop (Receiver => R);
         OS.Delete_File (Filename => Test_Utils. Dump_File);
         raise;
   end Listen_Callbacks;

   -------------------------------------------------------------------------

   procedure Receive_Multicast_V6
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Socket_Type;
      Grp    : constant IPv6_Addr_Type := To_IPv6_Addr
        (Str => "ff01:0000:0000:0000:0000:0000:0001:0002");
      Addr   : constant Socket_Addr_Type
        := (Family  => Family_Inet6,
            Addr_V6 => Grp,
            Port_V6 => Test_Utils.Listen_Port);

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sender : Socket_Addr_Type (Family => Family_Inet6);
      begin
         Sock.Create (Family => Family_Inet6,
                      Mode   => Datagram_Socket);
         Sock.Bind (Address => Addr);
         Sock.Join_Multicast_Group (Group => Addr);

         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);
         Test_Utils.Dump (Data => Buffer (Buffer'First .. Last),
                          Src  => Sender);
         accept Wait;
      end Receiver;
   begin
      Test_Utils.Send_Data
        (Dst      => Addr,
         Filename => "data/chunk1.dat");

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         OS.Delete_File (Filename => Test_Utils. Dump_File);
         raise;
   end Receive_Multicast_V6;

   -------------------------------------------------------------------------

   procedure Receive_Unix_Datagram
   is
      Path   : constant String := "obj/mysock2";
      Cmd    : constant String := "socat -u EXEC:'cat data/chunk1.dat"
        & "' UNIX-SENDTO:" & Path;

      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Socket_Type;

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sender : Socket_Addr_Type (Family => Family_Unix);
      begin
         Sock.Create (Family => Family_Unix,
                      Mode   => Datagram_Socket);
         Sock.Bind_Unix (Path => Unix_Path_Type (Path));
         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);
         Test_Utils.Dump (Data => Buffer (Buffer'First .. Last),
                          Src  => Sender);

         accept Wait;
      end Receiver;
   begin
      OS.Execute (Command => Cmd);

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         OS.Delete_File (Filename => Test_Utils.Dump_File);
         raise;
   end Receive_Unix_Datagram;

   -------------------------------------------------------------------------

   procedure Receive_Unix_Stream
   is
      Path   : constant Unix_Path_Type := "obj/mysock2";
      Cmd    : constant String         := "socat -u EXEC:'cat data/chunk1.dat"
        & "' UNIX-CLIENT:" & String (Path);

      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Socket_Type;

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sender : Socket_Addr_Type (Family => Family_Unix);
         S2     : Socket_Type;
      begin
         Sock.Create (Family => Family_Unix,
                      Mode   => Stream_Socket);
         Sock.Bind_Unix (Path => Path);
         Sock.Listen;
         Sock.Accept_Connection (New_Socket => S2);
         S2.Receive (Src  => Sender,
                     Item => Buffer,
                     Last => Last);
         Test_Utils.Dump (Data => Buffer (Buffer'First .. Last),
                          Src  => Sender);

         accept Wait;
      end Receiver;
   begin
      OS.Execute (Command => Cmd);

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         OS.Delete_File (Filename => Test_Utils.Dump_File);
         raise;
   end Receive_Unix_Stream;

   -------------------------------------------------------------------------

   procedure Receive_V6
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Socket_Type;

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sender : Socket_Addr_Type (Family => Family_Inet6);
      begin
         Sock.Create (Family => Family_Inet6,
                      Mode   => Datagram_Socket);
         Sock.Bind (Address => Test_Utils.Test_Addr_V6);

         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);
         Test_Utils.Dump (Data => Buffer (Buffer'First .. Last),
                          Src  => Sender);
         accept Wait;
      end Receiver;
   begin
      Test_Utils.Send_Data (Dst      => Test_Utils.Test_Addr_V6,
                            Filename => "data/chunk1.dat");

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         OS.Delete_File (Filename => Test_Utils. Dump_File);
         raise;
   end Receive_V6;

   -------------------------------------------------------------------------

   procedure Receive_V4_Datagram
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Socket_Type;

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sender : Socket_Addr_Type;
      begin
         Sock.Create (Family => Family_Inet,
                      Mode   => Datagram_Socket);
         Sock.Bind (Address => Test_Utils.Test_Addr_V4);
         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);
         Test_Utils.Dump (Data => Buffer (Buffer'First .. Last),
                          Src  => Sender);
         accept Wait;
      end Receiver;
   begin
      Test_Utils.Send_Data (Filename => "data/chunk1.dat");

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         OS.Delete_File (Filename => Test_Utils. Dump_File);
         raise;
   end Receive_V4_Datagram;

   -------------------------------------------------------------------------

   procedure Receive_V4_Stream
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Socket_Type;
      Sender : Socket_Addr_Type;

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         S2 : Socket_Type;
      begin
         Sock.Create (Family => Family_Inet,
                      Mode   => Stream_Socket);
         Sock.Set_Socket_Option (Option => Reuse_Address,
                                 Value  => True);
         Sock.Bind (Address => Test_Utils.Test_Addr_V4);
         Sock.Listen;
         Sock.Accept_Connection (New_Socket => S2);
         S2.Receive (Src  => Sender,
                     Item => Buffer,
                     Last => Last);
         Test_Utils.Dump (Data => Buffer (Buffer'First .. Last),
                          Src  => Sender);
         accept Wait;
      end Receiver;
   begin
      Test_Utils.Send_Data (Filename => "data/chunk1.dat",
                            Mode     => "TCP");

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");
      Assert (Condition => Sender = No_Addr,
              Message   => "Unexpected sender");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         OS.Delete_File (Filename => Test_Utils. Dump_File);
         raise;
   end Receive_V4_Stream;

   -------------------------------------------------------------------------

   procedure Send_Multicast_V6
   is
      use type Anet.Sockets.Tasking.Count_Type;

      Data : constant Ada.Streams.Stream_Element_Array
        := OS.Read_File (Filename => "data/chunk1.dat");
      C    : Tasking.Count_Type := 0;
      Sock : aliased Socket_Type;
      R    : Tasking.Receiver_Type (S => Sock'Access);
      Grp  : constant IPv6_Addr_Type
        := To_IPv6_Addr (Str => "ff01:0000:0000:0000:0000:0000:0001:0002");
      Addr : constant Socket_Addr_Type
        := (Family  => Family_Inet6,
            Addr_V6 => Grp,
            Port_V6 => Test_Utils.Listen_Port);
   begin
      Sock.Create (Family => Family_Inet6,
                   Mode   => Datagram_Socket);
      Sock.Bind (Address => Addr);
      Sock.Join_Multicast_Group (Group => Addr);

      Tasking.Listen (Receiver => R,
                      Callback => Test_Utils.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item => Data,
                 Dst  => Addr);

      for I in 1 .. 30 loop
         C := Tasking.Get_Rcv_Msg_Count (Receiver => R);
         exit when C > 0;
         delay 0.1;
      end loop;

      Tasking.Stop (Receiver => R);

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         Tasking.Stop (Receiver => R);
         OS.Delete_File (Filename => Test_Utils.Dump_File);
         raise;
   end Send_Multicast_V6;

   -------------------------------------------------------------------------

   procedure Send_Unix_Datagram
   is
      Data : constant Ada.Streams.Stream_Element_Array
        := OS.Read_File (Filename => "data/chunk1.dat");

      Path : constant String         := "/tmp/mysock";
      Cmd  : aliased constant String := "socat UNIX-RECV:" & String (Path)
        & " " & Test_Utils.Dump_File;
      Sock : Socket_Type;

      Receiver : Command_Task (Command => Cmd'Access);
   begin
      Sock.Create (Family => Family_Unix,
                   Mode   => Datagram_Socket);

      --  Give receiver/socat enough time to create socket

      delay 0.1;

      Sock.Connect
        (Dst => (Family => Family_Unix,
                 Path   => Ada.Strings.Unbounded.To_Unbounded_String (Path)));
      Sock.Send (Item => Data);

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         OS.Delete_File (Filename => Test_Utils.Dump_File);
         raise;
   end Send_Unix_Datagram;

   -------------------------------------------------------------------------

   procedure Send_Unix_Stream
   is
      Data : constant Ada.Streams.Stream_Element_Array
        := OS.Read_File (Filename => "data/chunk1.dat");

      Path : constant String         := "/tmp/mysock";
      Cmd  : aliased constant String := "socat UNIX-LISTEN:" & Path & " "
        & Test_Utils.Dump_File;
      Sock : Socket_Type;

      Receiver : Command_Task (Command => Cmd'Access);
   begin
      Sock.Create (Family => Family_Unix,
                   Mode   => Stream_Socket);

      --  Give receiver/socat enough time to create socket

      delay 0.1;

      Sock.Connect
        (Dst => (Family => Family_Unix,
                 Path   => Ada.Strings.Unbounded.To_Unbounded_String (Path)));
      Sock.Send (Item => Data);

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         OS.Delete_File (Filename => Test_Utils.Dump_File);
         raise;
   end Send_Unix_Stream;

   -------------------------------------------------------------------------

   procedure Send_V6
   is
      use type Anet.Sockets.Tasking.Count_Type;

      Data : constant Ada.Streams.Stream_Element_Array
        := OS.Read_File (Filename => "data/chunk1.dat");
      C    : Tasking.Count_Type := 0;
      Sock : aliased Socket_Type;
      R    : Tasking.Receiver_Type (S => Sock'Access);
   begin
      Sock.Create (Family => Family_Inet6,
                   Mode   => Datagram_Socket);
      Sock.Bind (Address => Test_Utils.Test_Addr_V6);

      Tasking.Listen (Receiver => R,
                      Callback => Test_Utils.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item => Data,
                 Dst  => Test_Utils.Test_Addr_V6);

      for I in 1 .. 30 loop
         C := Tasking.Get_Rcv_Msg_Count (Receiver => R);
         exit when C > 0;
         delay 0.1;
      end loop;

      Tasking.Stop (Receiver => R);

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         Tasking.Stop (Receiver => R);
         OS.Delete_File (Filename => Test_Utils.Dump_File);
         raise;
   end Send_V6;

   -------------------------------------------------------------------------

   procedure Send_V4_Datagram
   is
      use type Anet.Sockets.Tasking.Count_Type;

      Data : constant Ada.Streams.Stream_Element_Array
        := OS.Read_File (Filename => "data/chunk1.dat");
      C    : Tasking.Count_Type := 0;
      Sock : aliased Socket_Type;
      R    : Tasking.Receiver_Type (S => Sock'Access);
   begin
      Sock.Create (Family => Family_Inet,
                   Mode   => Datagram_Socket);
      Sock.Bind (Address => Test_Utils.Test_Addr_V4);

      Tasking.Listen (Receiver => R,
                      Callback => Test_Utils.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item => Data,
                 Dst  => Test_Utils.Test_Addr_V4);

      for I in 1 .. 30 loop
         C := Tasking.Get_Rcv_Msg_Count (Receiver => R);
         exit when C > 0;
         delay 0.1;
      end loop;

      Tasking.Stop (Receiver => R);

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         Tasking.Stop (Receiver => R);
         OS.Delete_File (Filename => Test_Utils.Dump_File);
         raise;
   end Send_V4_Datagram;

   -------------------------------------------------------------------------

   procedure Socket_Addr_To_String
   is
      use Ada.Strings.Unbounded;

      package TU renames Anet.Test_Utils;

      Port_Str       : constant String := Test_Utils.Listen_Port'Img;
      Test_V4_Str    : constant String := "127.0.0.1 (" & Port_Str & " )";
      Test_V6_Str    : constant String
        := "0000:0000:0000:0000:0000:0000:0000:0001 (" & Port_Str & " )";
      Test_L2_Str    : constant String := "FF:FF:FF:FF:FF:FF";
      Test_Addr_L2   : constant Socket_Addr_Type
        := (Family  => Family_Packet,
            HW_Addr => (16#ff#, 16#ff#, 16#ff#, 16#ff#, 16#ff#, 16#ff#));
      Test_Unix_Str  : constant Unbounded_String
        := To_Unbounded_String ("/tmp/foosocket");
      Test_Addr_Unix : constant Socket_Addr_Type
        := (Family => Family_Unix,
            Path   => Test_Unix_Str);
   begin
      Assert
        (Condition => To_String (Address => TU.Test_Addr_V4) = Test_V4_Str,
         Message   => "IPv4 string mismatch");
      Assert
        (Condition => To_String (Address => TU.Test_Addr_V6) = Test_V6_Str,
         Message   => "IPv6 string mismatch");
      Assert
        (Condition => To_String (Address => Test_Addr_L2) = Test_L2_Str,
         Message   => "Packet address string mismatch");
      Assert
        (Condition => To_String (Address => Test_Addr_Unix) = Test_Unix_Str,
         Message   => "UNIX path mismatch");
   end Socket_Addr_To_String;

   -------------------------------------------------------------------------

   procedure Unix_Delete_Socket
   is
      Path : constant String := "obj/my_socket";
   begin
      declare
         Sock : Socket_Type;
      begin
         Sock.Create (Family => Family_Unix,
                      Mode   => Datagram_Socket);
         Sock.Bind_Unix (Path => Unix_Path_Type (Path));
         Assert (Condition => Ada.Directories.Exists (Name => Path),
                 Message   => "Path not found");
      end;
      Assert (Condition => not Ada.Directories.Exists (Name => Path),
              Message   => "Socket path still there");
   end Unix_Delete_Socket;

   -------------------------------------------------------------------------

   procedure Valid_Iface_Names
   is
      Too_Long : constant String :=
        (1 .. Sockets.Max_Iface_Name_Len + 1 => 'a');
   begin
      Assert (Condition => Is_Valid_Iface (Name => "lo"),
              Message   => "Invalid interface name 'lo'");
      Assert (Condition => not Is_Valid_Iface (Name => ""),
              Message   => "Valid empty interface name");
      Assert (Condition => not Is_Valid_Iface (Name => Too_Long),
              Message   => "Valid interface name '" & Too_Long & "'");
   end Valid_Iface_Names;

   -------------------------------------------------------------------------

   procedure Valid_Unix_Paths
   is
      Too_Long : constant String :=
        (1 .. Sockets.Max_Unix_Path_Len + 1 => 'a');
   begin
      Assert (Condition => Is_Valid_Unix (Path => "/tmp/foopath"),
              Message   => "Invalid path '/tmp/foopath'");
      Assert (Condition => not Is_Valid_Unix (Path => ""),
              Message   => "Valid empty path");
      Assert (Condition => not Is_Valid_Unix (Path => Too_Long),
              Message   => "Valid Path '" & Too_Long & "'");
   end Valid_Unix_Paths;

end Anet_Socket_Tests;
