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
with Ada.Directories;

with Anet.OS;
with Anet.Types;
with Anet.Sockets.Unix;
with Anet.Sockets.Inet;
with Anet.Sockets.Netlink;
with Anet.Receivers.Datagram;
with Anet.Receivers.Stream;
with Anet.Util;

with Test_Utils;

pragma Elaborate_All (Anet.OS);
pragma Elaborate_All (Anet.Receivers.Datagram);
pragma Elaborate_All (Anet.Receivers.Stream);

package body Socket_Tests is

   use Ahven;
   use Anet;
   use Anet.Sockets;
   use type Ada.Streams.Stream_Element_Array;

   package UDPv4_Receiver is new Receivers.Datagram
     (Buffer_Size  => 1024,
      Socket_Type  => Inet.UDPv4_Socket_Type,
      Address_Type => Inet.UDPv4_Sockaddr_Type,
      Receive      => Inet.Receive);

   package UDPv6_Receiver is new Receivers.Datagram
     (Buffer_Size  => 1024,
      Socket_Type  => Inet.UDPv6_Socket_Type,
      Address_Type => Inet.UDPv6_Sockaddr_Type,
      Receive      => Inet.Receive);

   package TCPv4_Receiver is new Receivers.Stream
     (Buffer_Size  => 1024,
      Socket_Type  => Inet.TCPv4_Socket_Type);

   package TCPv6_Receiver is new Receivers.Stream
     (Buffer_Size  => 1024,
      Socket_Type  => Inet.TCPv6_Socket_Type);

   package Unix_TCP_Receiver is new Receivers.Stream
     (Buffer_Size  => 1024,
      Socket_Type  => Unix.TCP_Socket_Type);

   package Netlink_Receiver is new Receivers.Datagram
     (Buffer_Size  => 1024,
      Socket_Type  => Netlink.Raw_Socket_Type,
      Address_Type => Netlink.Netlink_Addr_Type,
      Receive      => Netlink.Receive);

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
      Sock : aliased Inet.UDPv4_Socket_Type;
   begin
      Sock.Init;
      Sock.Bind (Address => Loopback_Addr_V4,
                 Port    => Test_Utils.Listen_Port);

      declare
         Rcvr : UDPv4_Receiver.Receiver_Type (S => Sock'Access);
      begin
         Rcvr.Listen (Callback => Test_Utils.Raise_Error'Access);

         Test_Utils.Send_Data_V4 (Filename => "data/chunk1.dat");

         --  By default all errors should be ignored

         Assert (Condition => Rcvr.Is_Listening,
                 Message   => "Receiver not listening");

         Rcvr.Stop;

      exception
         when others =>
            Rcvr.Stop;
            raise;
      end;

      declare
         Rcvr : UDPv4_Receiver.Receiver_Type (S => Sock'Access);
      begin
         Rcvr.Register_Error_Handler (Callback => Error_Handler'Access);
         Rcvr.Listen (Callback => Test_Utils.Raise_Error'Access);

         Test_Utils.Send_Data_V4 (Filename => "data/chunk1.dat");

         Assert (Condition => not Rcvr.Is_Listening,
                 Message   => "Receiver still listening");

      exception
         when others =>
            Rcvr.Stop;
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
        (Routine => Send_Netlink_Raw'Access,
         Name    => "Send data (Netlink, raw)");
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
   end Initialize;

   -------------------------------------------------------------------------

   procedure Listen_Callbacks
   is
      use type Receivers.Count_Type;

      C    : Receivers.Count_Type := 0;
      Sock : aliased Inet.UDPv4_Socket_Type;
      Rcvr : UDPv4_Receiver.Receiver_Type (S => Sock'Access);
   begin
      Sock.Init;
      Sock.Bind (Address => Loopback_Addr_V4,
                 Port    => Test_Utils.Listen_Port);
      Rcvr.Listen (Callback => Test_Utils.Dump'Access);

      Assert (Condition => Rcvr.Is_Listening,
              Message   => "Receiver not listening");

      Test_Utils.Send_Data_V4 (Filename => "data/chunk1.dat");

      for I in 1 .. 30 loop
         C := Rcvr.Get_Rcv_Msg_Count;
         exit when C > 0;
         delay 0.1;
      end loop;

      Rcvr.Stop;

      Assert (Condition => not Rcvr.Is_Listening,
              Message   => "Receiver still listening");

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);
      Assert (Condition => Test_Utils.Get_Dump = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         Rcvr.Stop;
         raise;
   end Listen_Callbacks;

   -------------------------------------------------------------------------

   procedure Receive_Multicast_V4
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Grp    : constant IPv4_Addr_Type := To_IPv4_Addr (Str => "224.0.0.117");

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sock   : Inet.UDPv4_Socket_Type;
         Sender : Inet.UDPv4_Sockaddr_Type;
      begin
         Sock.Init;
         Sock.Bind (Address => Grp,
                    Port    => Test_Utils.Listen_Port);
         Sock.Join_Multicast_Group (Group => Grp);
         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);

         accept Wait;
      end Receiver;
   begin
      Test_Utils.Send_Data_V4
        (Dst_Addr => Grp,
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
      Grp    : constant IPv6_Addr_Type := To_IPv6_Addr
        (Str => "ff01:0000:0000:0000:0000:0000:0001:0002");

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sock   : Inet.UDPv6_Socket_Type;
         Sender : Inet.UDPv6_Sockaddr_Type;
      begin
         Sock.Init;
         Sock.Bind (Address => Grp,
                    Port    => Test_Utils.Listen_Port);
         Sock.Join_Multicast_Group (Group => Grp);
         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);

         accept Wait;
      end Receiver;
   begin
      Test_Utils.Send_Data_V6
        (Dst_Addr => Grp,
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
      Sender : Types.Unix_Path_Type (1 .. Path'Length);

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sock : Unix.UDP_Socket_Type;
      begin
         Sock.Init;
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
      Assert (Condition => String (Sender) = Path,
              Message   => "Sender mismatch");

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

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sock : Unix.TCP_Socket_Type;
         S2   : Unix.TCP_Socket_Type;
      begin
         Sock.Init;
         Sock.Bind (Path => Types.Unix_Path_Type (Path));
         Sock.Listen;
         Sock.Accept_Connection (New_Socket => S2);
         S2.Receive (Item => Buffer,
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

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sock   : Inet.UDPv4_Socket_Type;
         Sender : Inet.UDPv4_Sockaddr_Type;
      begin
         Sock.Init;
         Sock.Bind (Address => Loopback_Addr_V4,
                    Port    => Test_Utils.Listen_Port);
         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);

         accept Wait;
      end Receiver;
   begin
      Test_Utils.Send_Data_V4 (Filename => "data/chunk1.dat");

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

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sock : Inet.TCPv4_Socket_Type;
         S2   : Inet.TCPv4_Socket_Type;
      begin
         Sock.Init;
         Sock.Bind (Address => Loopback_Addr_V4,
                    Port    => Test_Utils.Listen_Port);
         Sock.Listen;
         Sock.Accept_Connection (New_Socket => S2);
         S2.Receive (Item => Buffer,
                     Last => Last);

         accept Wait;
      end Receiver;
   begin
      Test_Utils.Send_Data_V4 (Filename => "data/chunk1.dat",
                               Mode     => "TCP");

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
   end Receive_V4_Stream;

   -------------------------------------------------------------------------

   procedure Receive_V6_Datagram
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sock   : Inet.UDPv6_Socket_Type;
         Sender : Inet.UDPv6_Sockaddr_Type;
      begin
         Sock.Init;
         Sock.Bind (Address => Loopback_Addr_V6,
                    Port    => Test_Utils.Listen_Port);
         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);

         accept Wait;
      end Receiver;
   begin
      Test_Utils.Send_Data_V6 (Filename => "data/chunk1.dat");

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

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sock : Inet.TCPv6_Socket_Type;
         S2   : Inet.TCPv6_Socket_Type;
      begin
         Sock.Init;
         Sock.Bind (Address => Loopback_Addr_V6,
                    Port    => Test_Utils.Listen_Port);
         Sock.Listen;
         Sock.Accept_Connection (New_Socket => S2);
         S2.Receive (Item => Buffer,
                     Last => Last);

         accept Wait;
      end Receiver;
   begin
      Test_Utils.Send_Data_V6
        (Filename => "data/chunk1.dat",
         Mode     => "TCP");

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
   end Receive_V6_Stream;

   -------------------------------------------------------------------------

   procedure Send_Multicast_V4
   is
      use type Receivers.Count_Type;

      C    : Receivers.Count_Type := 0;
      Sock : aliased Inet.UDPv4_Socket_Type;
      Rcvr : UDPv4_Receiver.Receiver_Type (S => Sock'Access);
      Grp  : constant IPv4_Addr_Type
        := To_IPv4_Addr (Str => "224.0.0.117");
   begin
      Sock.Init;
      Sock.Bind (Address => Grp,
                 Port    => Test_Utils.Listen_Port);
      Sock.Join_Multicast_Group (Group => Grp);

      Rcvr.Listen (Callback => Test_Utils.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item     => Ref_Chunk,
                 Dst_Addr => Grp,
                 Dst_Port => Test_Utils.Listen_Port);

      for I in 1 .. 30 loop
         C := Rcvr.Get_Rcv_Msg_Count;
         exit when C > 0;
         delay 0.1;
      end loop;

      Rcvr.Stop;

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);
      Assert (Condition => Test_Utils.Get_Dump = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         Rcvr.Stop;
         raise;
   end Send_Multicast_V4;

   -------------------------------------------------------------------------

   procedure Send_Multicast_V6
   is
      use type Receivers.Count_Type;

      C    : Receivers.Count_Type := 0;
      Sock : aliased Inet.UDPv6_Socket_Type;
      Rcvr : UDPv6_Receiver.Receiver_Type (S => Sock'Access);
      Grp  : constant IPv6_Addr_Type
        := To_IPv6_Addr (Str => "ff01:0000:0000:0000:0000:0000:0001:0002");
   begin
      Sock.Init;
      Sock.Bind (Address => Grp,
                 Port    => Test_Utils.Listen_Port);
      Sock.Join_Multicast_Group (Group => Grp);

      Rcvr.Listen (Callback => Test_Utils.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item     => Ref_Chunk,
                 Dst_Addr => Grp,
                 Dst_Port => Test_Utils.Listen_Port);

      for I in 1 .. 30 loop
         C := Rcvr.Get_Rcv_Msg_Count;
         exit when C > 0;
         delay 0.1;
      end loop;

      Rcvr.Stop;

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);
      Assert (Condition => Test_Utils.Get_Dump = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         Rcvr.Stop;
         raise;
   end Send_Multicast_V6;

   -------------------------------------------------------------------------

   procedure Send_Netlink_Raw
   is
      use type Receivers.Count_Type;

      C    : Receivers.Count_Type := 0;
      Sock : aliased Netlink.Raw_Socket_Type;
      Rcvr : Netlink_Receiver.Receiver_Type (S => Sock'Access);
      Pid  : constant Netlink.Netlink_Addr_Type := 23499;
   begin
      Sock.Init (Protocol => Netlink.Proto_Netlink_Xfrm);
      Sock.Bind (Address => Pid);

      Rcvr.Listen (Callback => Test_Utils.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item => Ref_Chunk,
                 To   => Pid);

      for I in 1 .. 30 loop
         C := Rcvr.Get_Rcv_Msg_Count;
         exit when C > 0;
         delay 0.1;
      end loop;

      Rcvr.Stop;

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);
      Assert (Condition => Test_Utils.Get_Dump = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         Rcvr.Stop;
         raise;
   end Send_Netlink_Raw;

   -------------------------------------------------------------------------

   procedure Send_Unix_Datagram
   is
      Path : constant String         := "/tmp/mysock-"
        & Util.Random_String (Len => 8);
      Dump : constant String         := Path & ".dump";
      Cmd  : aliased constant String := "socat UNIX-RECV:" & Path & " " & Dump;
      Sock : Unix.UDP_Socket_Type;

      Receiver : Command_Task (Command => Cmd'Access);
   begin
      Sock.Init;
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
      use type Receivers.Count_Type;

      Path         : constant String      := "/tmp/mysock-"
        & Util.Random_String (Len => 8);
      C            : Receivers.Count_Type := 0;
      S_Srv, S_Cli : aliased Unix.TCP_Socket_Type;
      Rcvr         : Unix_TCP_Receiver.Receiver_Type (S => S_Srv'Access);
   begin
      S_Srv.Init;
      S_Srv.Bind (Path => Types.Unix_Path_Type (Path));
      Util.Wait_For_File (Path     => Path,
                          Timespan => 2.0);

      Rcvr.Listen (Callback => Test_Utils.Echo'Access);

      S_Cli.Init;
      S_Cli.Connect (Path => Types.Unix_Path_Type (Path));
      S_Cli.Send (Item => Ref_Chunk);

      for I in 1 .. 30 loop
         C := Rcvr.Get_Rcv_Msg_Count;
         exit when C > 0;
         delay 0.1;
      end loop;

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);

      declare
         Buffer : Ada.Streams.Stream_Element_Array
           (1 .. Unix_TCP_Receiver.Buffsize);
         Last   : Ada.Streams.Stream_Element_Offset;
      begin
         S_Cli.Receive (Item => Buffer,
                        Last => Last);
         Rcvr.Stop;

         Assert (Condition => Buffer (Buffer'First .. Last) = Ref_Chunk,
                 Message   => "Response mismatch");
      end;

   exception
      when others =>
         Rcvr.Stop;
         raise;
   end Send_Unix_Stream;

   -------------------------------------------------------------------------

   procedure Send_V4_Datagram
   is
      use type Receivers.Count_Type;

      C    : Receivers.Count_Type := 0;
      Sock : aliased Inet.UDPv4_Socket_Type;
      Rcvr : UDPv4_Receiver.Receiver_Type (S => Sock'Access);
   begin
      Sock.Init;
      Sock.Bind (Address => Loopback_Addr_V4,
                 Port    => Test_Utils.Listen_Port);

      Rcvr.Listen (Callback => Test_Utils.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item     => Ref_Chunk,
                 Dst_Addr => Loopback_Addr_V4,
                 Dst_Port => Test_Utils.Listen_Port);

      for I in 1 .. 30 loop
         C := Rcvr.Get_Rcv_Msg_Count;
         exit when C > 0;
         delay 0.1;
      end loop;

      Rcvr.Stop;

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);
      Assert (Condition => Test_Utils.Get_Dump = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         Rcvr.Stop;
         raise;
   end Send_V4_Datagram;

   -------------------------------------------------------------------------

   procedure Send_V4_Stream
   is
      use type Receivers.Count_Type;

      C            : Receivers.Count_Type := 0;
      S_Srv, S_Cli : aliased Inet.TCPv4_Socket_Type;
      Rcvr         : TCPv4_Receiver.Receiver_Type (S => S_Srv'Access);
   begin
      S_Srv.Init;
      S_Srv.Bind (Address => Loopback_Addr_V4,
                  Port    => Test_Utils.Listen_Port);

      Rcvr.Listen (Callback => Test_Utils.Echo'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      S_Cli.Init;
      S_Cli.Bind (Address => Loopback_Addr_V4,
                  Port    => Test_Utils.Listen_Port + 1);
      S_Cli.Connect (Address => Loopback_Addr_V4,
                     Port    => Test_Utils.Listen_Port);
      S_Cli.Send (Item => Ref_Chunk);

      for I in 1 .. 30 loop
         C := Rcvr.Get_Rcv_Msg_Count;
         exit when C > 0;
         delay 0.1;
      end loop;

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);

      declare
         Buffer : Ada.Streams.Stream_Element_Array
           (1 .. TCPv4_Receiver.Buffsize);
         Last   : Ada.Streams.Stream_Element_Offset;
      begin
         S_Cli.Receive (Item => Buffer,
                        Last => Last);
         Rcvr.Stop;

         Assert (Condition => Buffer (Buffer'First .. Last) = Ref_Chunk,
                 Message   => "Response mismatch");
      end;

   exception
      when others =>
         Rcvr.Stop;
         raise;
   end Send_V4_Stream;

   -------------------------------------------------------------------------

   procedure Send_V6_Datagram
   is
      use type Receivers.Count_Type;

      C    : Receivers.Count_Type := 0;
      Sock : aliased Inet.UDPv6_Socket_Type;
      Rcvr : UDPv6_Receiver.Receiver_Type (S => Sock'Access);
   begin
      Sock.Init;
      Sock.Bind (Address => Loopback_Addr_V6,
                 Port    => Test_Utils.Listen_Port);

      Rcvr.Listen (Callback => Test_Utils.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item     => Ref_Chunk,
                 Dst_Addr => Loopback_Addr_V6,
                 Dst_Port => Test_Utils.Listen_Port);

      for I in 1 .. 30 loop
         C := Rcvr.Get_Rcv_Msg_Count;
         exit when C > 0;
         delay 0.1;
      end loop;

      Rcvr.Stop;

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);
      Assert (Condition => Test_Utils.Get_Dump = Ref_Chunk,
              Message   => "Result mismatch");

   exception
      when others =>
         Rcvr.Stop;
         raise;
   end Send_V6_Datagram;

   -------------------------------------------------------------------------

   procedure Send_V6_Stream
   is
      use type Receivers.Count_Type;

      C            : Receivers.Count_Type := 0;
      S_Srv, S_Cli : aliased Inet.TCPv6_Socket_Type;
      Rcvr         : TCPv6_Receiver.Receiver_Type (S => S_Srv'Access);
   begin
      S_Srv.Init;
      S_Srv.Bind (Address => Loopback_Addr_V6,
                  Port    => Test_Utils.Listen_Port);

      Rcvr.Listen (Callback => Test_Utils.Echo'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      S_Cli.Init;
      S_Cli.Bind (Address => Loopback_Addr_V6,
                  Port    => Test_Utils.Listen_Port + 1);
      S_Cli.Connect (Address => Loopback_Addr_V6,
                     Port    => Test_Utils.Listen_Port);
      S_Cli.Send (Item => Ref_Chunk);

      for I in 1 .. 30 loop
         C := Rcvr.Get_Rcv_Msg_Count;
         exit when C > 0;
         delay 0.1;
      end loop;

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);

      declare
         Buffer : Ada.Streams.Stream_Element_Array
           (1 .. TCPv4_Receiver.Buffsize);
         Last   : Ada.Streams.Stream_Element_Offset;
      begin
         S_Cli.Receive (Item => Buffer,
                        Last => Last);
         Rcvr.Stop;

         Assert (Condition => Buffer (Buffer'First .. Last) = Ref_Chunk,
                 Message   => "Response mismatch");
      end;

   exception
      when others =>
         Rcvr.Stop;
         raise;
   end Send_V6_Stream;

   -------------------------------------------------------------------------

   procedure Unix_Delete_Socket
   is
      Path : constant String := "./my_socket";
   begin
      declare
         Sock : Unix.UDP_Socket_Type;
      begin
         Sock.Init;
         Sock.Bind (Path => Types.Unix_Path_Type (Path));
         Assert (Condition => Ada.Directories.Exists (Name => Path),
                 Message   => "Path not found");
      end;
      Assert (Condition => not Ada.Directories.Exists (Name => Path),
              Message   => "Socket path still there");
   end Unix_Delete_Socket;

end Socket_Tests;
