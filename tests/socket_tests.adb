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
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Directories;

with Anet.OS;
with Anet.Types;
with Anet.Sockets.Unix;
with Anet.Sockets.Inet;
with Anet.Sockets.Tasking;
with Anet.Util;

with Test_Utils;

pragma Elaborate_All (Anet.OS);

package body Socket_Tests is

   use Ahven;
   use Anet;
   use Anet.Sockets;
   use type Ada.Streams.Stream_Element_Array;

   Ref_Chunk : constant Ada.Streams.Stream_Element_Array
     := OS.Read_File (Filename => "data/chunk1.dat");

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
      Sock : aliased Inet.UDPv4_Socket_Type := Inet.Create;
   begin
      Sock.Bind (Address => Test_Utils.Test_Addr_V4);

      declare
         R : Tasking.Dgram_Receiver_Type
           (S => Dgram_Socket_Type (Sock)'Access);
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
         R : Tasking.Dgram_Receiver_Type
           (S => Dgram_Socket_Type (Sock)'Access);
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

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Tests for Sockets package");
      T.Add_Test_Routine
        (Routine => Send_V4_Stream'Access,
         Name    => "Send data (IPv4, stream)");
      T.Add_Test_Routine
        (Routine => Send_V4_Datagram'Access,
         Name    => "Send data (IPv4, datagram)");
      T.Add_Test_Routine
        (Routine => Send_V6_Stream'Access,
         Name    => "Send data (IPv6, stream)");
      T.Add_Test_Routine
        (Routine => Send_V6_Datagram'Access,
         Name    => "Send data (IPv6, datagram)");
      T.Add_Test_Routine
        (Routine => Send_Multicast_V4'Access,
         Name    => "Send data (IPv4, multicast)");
      T.Add_Test_Routine
        (Routine => Send_Multicast_V6'Access,
         Name    => "Send data (IPv6, multicast)");
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
        (Routine => Receive_V6_Stream'Access,
         Name    => "Receive data (IPv6, stream)");
      T.Add_Test_Routine
        (Routine => Receive_V6_Datagram'Access,
         Name    => "Receive data (IPv6, datagram)");
      T.Add_Test_Routine
        (Routine => Receive_Multicast_V4'Access,
         Name    => "Receive data (IPv4, multicast)");
      T.Add_Test_Routine
        (Routine => Receive_Multicast_V6'Access,
         Name    => "Receive data (IPv6, multicast)");
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
        (Routine => Socket_Addr_To_String'Access,
         Name    => "Socket address to string conversion");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Listen_Callbacks
   is
      use type Anet.Sockets.Tasking.Count_Type;

      C    : Tasking.Count_Type             := 0;
      Sock : aliased Inet.UDPv4_Socket_Type := Inet.Create;
      R    : Tasking.Dgram_Receiver_Type
        (S => Dgram_Socket_Type (Sock)'Access);
   begin
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
      Assert (Condition => Test_Utils.Get_Dump = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         Tasking.Stop (Receiver => R);
         raise;
   end Listen_Callbacks;

   -------------------------------------------------------------------------

   procedure Receive_Multicast_V4
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Inet.UDPv4_Socket_Type  := Inet.Create;
      Grp    : constant IPv4_Addr_Type := To_IPv4_Addr (Str => "224.0.0.117");
      Addr   : constant Socket_Addr_Type
        := (Family  => Family_Inet,
            Addr_V4 => Grp,
            Port_V4 => Test_Utils.Listen_Port);

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sender : Inet.UDPv4_Sockaddr_Type;
      begin
         Sock.Bind (Address => Addr);
         Sock.Join_Multicast_Group (Group => Addr);
         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);

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

      Assert (Condition => Buffer (Buffer'First .. Last) = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         raise;
   end Receive_Multicast_V4;

   -------------------------------------------------------------------------

   procedure Receive_Multicast_V6
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Inet.UDPv6_Socket_Type  := Inet.Create;
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
         Sock.Bind (Address => Addr);
         Sock.Join_Multicast_Group (Group => Addr);
         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);

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

      Assert (Condition => Buffer (Buffer'First .. Last) = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         raise;
   end Receive_Multicast_V6;

   -------------------------------------------------------------------------

   procedure Receive_Unix_Datagram
   is
      Path   : constant String := "/tmp/mysock-"
        & Util.Random_String (Len => 8);
      Cmd    : constant String := "socat -u EXEC:'cat data/chunk1.dat"
        & "' UNIX-SENDTO:" & Path;

      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Unix.UDP_Socket_Type := Unix.Create;

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sender : Socket_Addr_Type (Family => Family_Unix);
      begin
         Sock.Bind (Path => Types.Unix_Path_Type (Path));
         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);

         accept Wait;
      end Receiver;
   begin
      OS.Execute (Command => Cmd);

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Buffer (Buffer'First .. Last) = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         raise;
   end Receive_Unix_Datagram;

   -------------------------------------------------------------------------

   procedure Receive_Unix_Stream
   is
      Path   : constant String := "/tmp/mysock-"
        & Util.Random_String (Len => 8);
      Cmd    : constant String := "socat -u EXEC:'cat data/chunk1.dat"
        & "' UNIX-CLIENT:" & Path;

      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Unix.TCP_Socket_Type := Unix.Create;

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sender : Socket_Addr_Type (Family => Family_Unix);
         S2     : Unix.TCP_Socket_Type;
      begin
         Sock.Bind (Path => Types.Unix_Path_Type (Path));
         Sock.Listen;
         Sock.Accept_Connection (New_Socket => S2);
         S2.Receive (Src  => Sender,
                     Item => Buffer,
                     Last => Last);

         accept Wait;
      end Receiver;
   begin
      OS.Execute (Command => Cmd);

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Buffer (Buffer'First .. Last) = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         raise;
   end Receive_Unix_Stream;

   -------------------------------------------------------------------------

   procedure Receive_V4_Datagram
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Inet.UDPv4_Socket_Type := Inet.Create;

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sender : Inet.UDPv4_Sockaddr_Type;
      begin
         Sock.Bind (Address => Test_Utils.Test_Addr_V4);
         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);

         accept Wait;
      end Receiver;
   begin
      Test_Utils.Send_Data (Filename => "data/chunk1.dat");

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Buffer (Buffer'First .. Last) = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         raise;
   end Receive_V4_Datagram;

   -------------------------------------------------------------------------

   procedure Receive_V4_Stream
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Inet.TCPv4_Socket_Type := Inet.Create;
      Sender : Socket_Addr_Type;

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         S2 : Inet.TCPv4_Socket_Type;
      begin
         Sock.Set_Socket_Option (Option => Reuse_Address,
                                 Value  => True);
         Sock.Bind (Address => Test_Utils.Test_Addr_V4);
         Sock.Listen;
         Sock.Accept_Connection (New_Socket => S2);
         S2.Receive (Src  => Sender,
                     Item => Buffer,
                     Last => Last);

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

      Assert (Condition => Buffer (Buffer'First .. Last) = Ref_Chunk,
              Message   => "Result mismatch");
      Assert (Condition => Sender = No_Addr,
              Message   => "Unexpected sender");

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         raise;
   end Receive_V4_Stream;

   -------------------------------------------------------------------------

   procedure Receive_V6_Datagram
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Inet.UDPv6_Socket_Type := Inet.Create;

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sender : Socket_Addr_Type (Family => Family_Inet6);
      begin
         Sock.Bind (Address => Test_Utils.Test_Addr_V6);
         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);

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

      Assert (Condition => Buffer (Buffer'First .. Last) = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         raise;
   end Receive_V6_Datagram;

   -------------------------------------------------------------------------

   procedure Receive_V6_Stream
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Inet.TCPv6_Socket_Type := Inet.Create;
      Sender : Socket_Addr_Type;

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         S2 : Inet.TCPv6_Socket_Type;
      begin
         Sock.Set_Socket_Option (Option => Reuse_Address,
                                 Value  => True);
         Sock.Bind (Address => Test_Utils.Test_Addr_V6);
         Sock.Listen;
         Sock.Accept_Connection (New_Socket => S2);
         S2.Receive (Src  => Sender,
                     Item => Buffer,
                     Last => Last);

         accept Wait;
      end Receiver;
   begin
      Test_Utils.Send_Data (Dst      => Test_Utils.Test_Addr_V6,
                            Filename => "data/chunk1.dat",
                            Mode     => "TCP");

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Buffer (Buffer'First .. Last) = Ref_Chunk,
              Message   => "Result mismatch");
      Assert (Condition => Sender = No_Addr,
              Message   => "Unexpected sender");

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         raise;
   end Receive_V6_Stream;

   -------------------------------------------------------------------------

   procedure Send_Multicast_V4
   is
      use type Anet.Sockets.Tasking.Count_Type;

      C    : Tasking.Count_Type             := 0;
      Sock : aliased Inet.UDPv4_Socket_Type := Inet.Create;
      R    : Tasking.Dgram_Receiver_Type
        (S => Dgram_Socket_Type (Sock)'Access);
      Grp  : constant IPv4_Addr_Type
        := To_IPv4_Addr (Str => "224.0.0.117");
      Addr : constant Socket_Addr_Type
        := (Family  => Family_Inet,
            Addr_V4 => Grp,
            Port_V4 => Test_Utils.Listen_Port);
   begin
      Sock.Bind (Address => Addr);
      Sock.Join_Multicast_Group (Group => Addr);

      Tasking.Listen (Receiver => R,
                      Callback => Test_Utils.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item => Ref_Chunk,
                 Dst  => Addr);

      for I in 1 .. 30 loop
         C := Tasking.Get_Rcv_Msg_Count (Receiver => R);
         exit when C > 0;
         delay 0.1;
      end loop;

      Tasking.Stop (Receiver => R);

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);
      Assert (Condition => Test_Utils.Get_Dump = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         Tasking.Stop (Receiver => R);
         raise;
   end Send_Multicast_V4;

   -------------------------------------------------------------------------

   procedure Send_Multicast_V6
   is
      use type Anet.Sockets.Tasking.Count_Type;

      C    : Tasking.Count_Type             := 0;
      Sock : aliased Inet.UDPv6_Socket_Type := Inet.Create;
      R    : Tasking.Dgram_Receiver_Type
        (S => Dgram_Socket_Type (Sock)'Access);
      Grp  : constant IPv6_Addr_Type
        := To_IPv6_Addr (Str => "ff01:0000:0000:0000:0000:0000:0001:0002");
      Addr : constant Socket_Addr_Type
        := (Family  => Family_Inet6,
            Addr_V6 => Grp,
            Port_V6 => Test_Utils.Listen_Port);
   begin
      Sock.Bind (Address => Addr);
      Sock.Join_Multicast_Group (Group => Addr);

      Tasking.Listen (Receiver => R,
                      Callback => Test_Utils.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item => Ref_Chunk,
                 Dst  => Addr);

      for I in 1 .. 30 loop
         C := Tasking.Get_Rcv_Msg_Count (Receiver => R);
         exit when C > 0;
         delay 0.1;
      end loop;

      Tasking.Stop (Receiver => R);

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);
      Assert (Condition => Test_Utils.Get_Dump = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         Tasking.Stop (Receiver => R);
         raise;
   end Send_Multicast_V6;

   -------------------------------------------------------------------------

   procedure Send_Unix_Datagram
   is
      Path : constant String         := "/tmp/mysock-"
        & Util.Random_String (Len => 8);
      Dump : constant String         := Path & ".dump";
      Cmd  : aliased constant String := "socat UNIX-RECV:" & Path & " " & Dump;
      Sock : Unix.UDP_Socket_Type    := Unix.Create;

      Receiver : Command_Task (Command => Cmd'Access);
   begin
      Util.Wait_For_File (Path     => Path,
                          Timespan => 2.0);

      Sock.Connect (Path => Types.Unix_Path_Type (Path));
      Sock.Send (Item => Ref_Chunk);

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Dump),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Dump);

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         OS.Delete_File (Filename => Dump);
         raise;
   end Send_Unix_Datagram;

   -------------------------------------------------------------------------

   procedure Send_Unix_Stream
   is
      Path : constant String         := "/tmp/mysock-"
        & Util.Random_String (Len => 8);
      Dump : constant String         := Path & ".dump";
      Cmd  : aliased constant String := "socat UNIX-LISTEN:" & Path & " "
        & Dump;
      Sock : Unix.TCP_Socket_Type    := Unix.Create;

      Receiver : Command_Task (Command => Cmd'Access);
   begin
      Util.Wait_For_File (Path     => Path,
                          Timespan => 2.0);

      Sock.Connect (Path => Types.Unix_Path_Type (Path));
      Sock.Send (Item => Ref_Chunk);

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Dump),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Dump);

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         OS.Delete_File (Filename => Dump);
         raise;
   end Send_Unix_Stream;

   -------------------------------------------------------------------------

   procedure Send_V4_Datagram
   is
      use type Anet.Sockets.Tasking.Count_Type;

      C    : Tasking.Count_Type             := 0;
      Sock : aliased Inet.UDPv4_Socket_Type := Inet.Create;
      R    : Tasking.Dgram_Receiver_Type
        (S => Dgram_Socket_Type (Sock)'Access);
   begin
      Sock.Bind (Address => Test_Utils.Test_Addr_V4);

      Tasking.Listen (Receiver => R,
                      Callback => Test_Utils.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item => Ref_Chunk,
                 Dst  => Test_Utils.Test_Addr_V4);

      for I in 1 .. 30 loop
         C := Tasking.Get_Rcv_Msg_Count (Receiver => R);
         exit when C > 0;
         delay 0.1;
      end loop;

      Tasking.Stop (Receiver => R);

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);
      Assert (Condition => Test_Utils.Get_Dump = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         Tasking.Stop (Receiver => R);
         raise;
   end Send_V4_Datagram;

   -------------------------------------------------------------------------

   procedure Send_V4_Stream
   is
      Dump : constant String         := "/tmp/dump-"
        & Util.Random_String (Len => 8);
      Cmd  : aliased constant String := "socat TCP-LISTEN:"
        & Ada.Strings.Fixed.Trim (Source => Test_Utils.Listen_Port'Img,
                                  Side   => Ada.Strings.Left)
        & ",reuseaddr " & Dump;
      Sock : Inet.TCPv4_Socket_Type  := Inet.Create;

      Receiver : Command_Task (Command => Cmd'Access);
   begin

      --  Give receiver/socat enough time to create socket

      delay 0.1;

      Sock.Connect (Address => Loopback_Addr_V4,
                    Port    => Test_Utils.Listen_Port);
      Sock.Send (Item => Ref_Chunk);

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Dump),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Dump);

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         OS.Delete_File (Filename => Dump);
         raise;
   end Send_V4_Stream;

   -------------------------------------------------------------------------

   procedure Send_V6_Datagram
   is
      use type Anet.Sockets.Tasking.Count_Type;

      C    : Tasking.Count_Type             := 0;
      Sock : aliased Inet.UDPv6_Socket_Type := Inet.Create;
      R    : Tasking.Dgram_Receiver_Type
        (S => Dgram_Socket_Type (Sock)'Access);
   begin
      Sock.Bind (Address => Test_Utils.Test_Addr_V6);

      Tasking.Listen (Receiver => R,
                      Callback => Test_Utils.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item => Ref_Chunk,
                 Dst  => Test_Utils.Test_Addr_V6);

      for I in 1 .. 30 loop
         C := Tasking.Get_Rcv_Msg_Count (Receiver => R);
         exit when C > 0;
         delay 0.1;
      end loop;

      Tasking.Stop (Receiver => R);

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);
      Assert (Condition => Test_Utils.Get_Dump = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         Tasking.Stop (Receiver => R);
         raise;
   end Send_V6_Datagram;

   -------------------------------------------------------------------------

   procedure Send_V6_Stream
   is
      Dump : constant String         := "/tmp/dump-"
        & Util.Random_String (Len => 8);
      Cmd  : aliased constant String := "socat TCP6-LISTEN:"
        & Ada.Strings.Fixed.Trim (Source => Test_Utils.Listen_Port'Img,
                                  Side   => Ada.Strings.Left)
        & ",reuseaddr " & Dump;
      Sock : Inet.TCPv6_Socket_Type  := Inet.Create;

      Receiver : Command_Task (Command => Cmd'Access);
   begin

      --  Give receiver/socat enough time to create socket

      delay 0.1;

      Sock.Connect (Address => Loopback_Addr_V6,
                    Port    => Test_Utils.Listen_Port);
      Sock.Send (Item => Ref_Chunk);

      select
         delay 3.0;
      then abort
         Receiver.Wait;
      end select;

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Dump),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Dump);

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         OS.Delete_File (Filename => Dump);
         raise;
   end Send_V6_Stream;

   -------------------------------------------------------------------------

   procedure Socket_Addr_To_String
   is
      use Ada.Strings.Unbounded;

      package TU renames Test_Utils;

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
      Path : constant String := "./my_socket";
   begin
      declare
         Sock : Unix.UDP_Socket_Type := Unix.Create;
      begin
         Sock.Bind (Path => Types.Unix_Path_Type (Path));
         Assert (Condition => Ada.Directories.Exists (Name => Path),
                 Message   => "Path not found");
      end;
      Assert (Condition => not Ada.Directories.Exists (Name => Path),
              Message   => "Socket path still there");
   end Unix_Delete_Socket;

end Socket_Tests;
