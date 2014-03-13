--
--  Copyright (C) 2011-2013 secunet Security Networks AG
--  Copyright (C) 2011-2013 Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2011-2013 Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Directories;

with Anet.Constants;
with Anet.Sockets.Unix;
with Anet.Sockets.Netlink;
with Anet.Sockets.Packet;
with Anet.Receivers.Datagram;
with Anet.Receivers.Stream;
with Anet.Util;

with Test_Utils.Linux;

pragma Elaborate_All (Anet.Receivers.Datagram);
pragma Elaborate_All (Anet.Receivers.Stream);

package body Socket_Tests is

   use Ahven;
   use Anet;
   use Anet.Sockets;
   use type Ada.Streams.Stream_Element_Array;

   package Unix_UDP_Receiver is new Receivers.Datagram
     (Buffer_Size  => 1024,
      Socket_Type  => Unix.UDP_Socket_Type,
      Address_Type => Unix.Full_Path_Type,
      Receive      => Unix.Receive);

   package Unix_TCP_Receiver is new Receivers.Stream
     (Buffer_Size  => 1024,
      Socket_Type  => Unix.TCP_Socket_Type);

   package Netlink_Receiver is new Receivers.Datagram
     (Buffer_Size  => 1024,
      Socket_Type  => Netlink.Raw_Socket_Type,
      Address_Type => Netlink.Netlink_Addr_Type,
      Receive      => Netlink.Receive);

   package Packet_UDP_Receiver is new Receivers.Datagram
     (Buffer_Size  => 1024,
      Socket_Type  => Packet.UDP_Socket_Type,
      Address_Type => Packet.Ether_Addr_Type,
      Receive      => Packet.Receive);

   package Packet_Raw_Receiver is new Receivers.Datagram
     (Buffer_Size  => 1024,
      Socket_Type  => Packet.Raw_Socket_Type,
      Address_Type => Packet.Ether_Addr_Type,
      Receive      => Packet.Receive);

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Tests for Sockets package");
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
        (Routine => Send_Packet_Datagram'Access,
         Name    => "Send data (Packet, datagram)");
      T.Add_Test_Routine
        (Routine => Send_Packet_Raw'Access,
         Name    => "Send data (Packet, raw)");
      T.Add_Test_Routine
        (Routine => Send_Various_Buffers'Access,
         Name    => "Send data (various buffer ranges)");
      T.Add_Test_Routine
        (Routine => Unix_Delete_Socket'Access,
         Name    => "Unix socket removal");
      T.Add_Test_Routine
        (Routine => Valid_Unix_Paths'Access,
         Name    => "Unix path validation");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Send_Netlink_Raw
   is
      use type Receivers.Count_Type;

      C    : Receivers.Count_Type := 0;
      Sock : aliased Netlink.Raw_Socket_Type;
      Rcvr : Netlink_Receiver.Receiver_Type (S => Sock'Access);
      Pid  : constant Netlink.Netlink_Addr_Type := 23499;
   begin
      if not Test_Utils.Has_Root_Perms then
         Skip (Message => "Run as root");
      end if;

      Sock.Init (Protocol => Netlink.Proto_Netlink_Xfrm);
      Sock.Bind (Address => Pid);

      Rcvr.Listen (Callback => Test_Utils.Linux.Dump'Access);

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

   procedure Send_Packet_Datagram
   is
      use type Receivers.Count_Type;

      C    : Receivers.Count_Type := 0;
      Sock : aliased Packet.UDP_Socket_Type;
      Rcvr : Packet_UDP_Receiver.Receiver_Type (S => Sock'Access);
   begin
      if not Test_Utils.Has_Root_Perms then
         Skip (Message => "Run as root");
      end if;

      Sock.Init;
      Sock.Bind (Iface => "lo");

      Rcvr.Listen (Callback => Test_Utils.Linux.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item  => Ref_Chunk,
                 To    => Bcast_HW_Addr,
                 Iface => "lo");

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
   end Send_Packet_Datagram;

   -------------------------------------------------------------------------

   procedure Send_Packet_Raw
   is
      use type Receivers.Count_Type;

      C    : Receivers.Count_Type := 0;
      Sock : aliased Packet.Raw_Socket_Type;
      Rcvr : Packet_Raw_Receiver.Receiver_Type (S => Sock'Access);
   begin
      if not Test_Utils.Has_Root_Perms then
         Skip (Message => "Run as root");
      end if;

      Sock.Init;
      Sock.Bind (Iface => "lo");

      Rcvr.Listen (Callback => Test_Utils.Linux.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item  => Ref_Chunk);

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
   end Send_Packet_Raw;

   -------------------------------------------------------------------------

   procedure Send_Unix_Datagram
   is
      use type Receivers.Count_Type;

      C            : Receivers.Count_Type := 0;
      Path         : constant String      := "/tmp/mysock-"
        & Util.Random_String (Len => 8);
      S_Srv, S_Cli : aliased Unix.UDP_Socket_Type;
      Rcvr         : Unix_UDP_Receiver.Receiver_Type (S => S_Srv'Access);
   begin
      S_Srv.Init;
      S_Srv.Bind (Path => Unix.Path_Type (Path));
      Util.Wait_For_File (Path     => Path,
                          Timespan => 2.0);

      Rcvr.Listen (Callback => Test_Utils.Dump'Access);

      S_Cli.Init;
      S_Cli.Connect (Path => Unix.Path_Type (Path));
      S_Cli.Send (Item => Ref_Chunk);

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
   end Send_Unix_Datagram;

   -------------------------------------------------------------------------

   procedure Send_Unix_Stream
   is
      use type Receivers.Count_Type;

      C            : Receivers.Count_Type := 0;
      Path         : constant String      := "/tmp/mysock-"
        & Util.Random_String (Len => 8);
      S_Srv, S_Cli : aliased Unix.TCP_Socket_Type;
      Rcvr         : Unix_TCP_Receiver.Receiver_Type (S => S_Srv'Access);
   begin
      S_Srv.Init;
      S_Srv.Bind (Path => Unix.Path_Type (Path));
      Util.Wait_For_File (Path     => Path,
                          Timespan => 2.0);

      Rcvr.Listen (Callback => Test_Utils.Echo'Access);

      S_Cli.Init;
      S_Cli.Connect (Path => Unix.Path_Type (Path));
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

   procedure Send_Various_Buffers
   is
      use Ada.Streams;

      Last  : Stream_Element_Offset;
      R     : Stream_Element_Array (1 .. 10);
      S_Cli : aliased Unix.UDP_Socket_Type;
      Path  : constant String := "/tmp/mysock-" & Util.Random_String
        (Len => 8);

      task Receiver is
         entry Ready;
      end Receiver;

      task body Receiver is
         S_Srv : aliased Unix.UDP_Socket_Type;
      begin
         S_Srv.Init;
         S_Srv.Bind (Path => Unix.Path_Type (Path));
         loop
            S_Srv.Receive (Item => R,
                           Last => Last);
            accept Ready;
         end loop;
      end Receiver;
   begin
      Util.Wait_For_File (Path     => Path,
                          Timespan => 2.0);

      S_Cli.Init;
      S_Cli.Connect (Path => Unix.Path_Type (Path));

      declare
         B : constant Stream_Element_Array (1 .. 10) := (others => 12);
      begin
         S_Cli.Send (Item => B);
         select
            delay 2.0;
         then abort
            Receiver.Ready;
         end select;
         Assert (Condition => B = R,
                 Message   => "Result mismatch (1 .. 10)");
         Assert (Condition => Last = 10,
                 Message   => "Last mismatch (1 .. 10)");
         R := (others => 0);
      end;

      declare
         B : constant Stream_Element_Array (0 .. 9) := (others => 12);
      begin
         S_Cli.Send (Item => B);
         select
            delay 2.0;
         then abort
            Receiver.Ready;
         end select;
         Assert (Condition => B = R,
                 Message   => "Result mismatch (0 .. 9)");
         Assert (Condition => Last = 10,
                 Message   => "Last mismatch (0 .. 9)");
         R := (others => 0);
      end;

      declare
         B : constant Stream_Element_Array (5 .. 14) := (others => 12);
      begin
         S_Cli.Send (Item => B);
         select
            delay 2.0;
         then abort
            Receiver.Ready;
         end select;
         Assert (Condition => B = R,
                 Message   => "Result mismatch (5 .. 14)");
         Assert (Condition => Last = 10,
                 Message   => "Last mismatch (5 .. 14)");
         R := (others => 0);
      end;

      declare
         Empty : constant Stream_Element_Array (1 .. 0) := (others => 0);
      begin
         S_Cli.Send (Item => Empty);
         select
            delay 2.0;
         then abort
            Receiver.Ready;
         end select;
         Assert (Condition => Last = 0,
                 Message   => "Last mismatch (1 .. 0)");
      end;

      abort Receiver;

   exception
      when others =>
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         raise;
   end Send_Various_Buffers;

   -------------------------------------------------------------------------

   procedure Unix_Delete_Socket
   is
      Path : constant String := "./my_socket";
   begin
      declare
         Sock : Unix.UDP_Socket_Type;
      begin
         Sock.Init;
         Sock.Bind (Path => Unix.Path_Type (Path));
         Assert (Condition => Ada.Directories.Exists (Name => Path),
                 Message   => "Path not found");
      end;
      Assert (Condition => not Ada.Directories.Exists (Name => Path),
              Message   => "Socket path still there");
   end Unix_Delete_Socket;

   -------------------------------------------------------------------------

   procedure Valid_Unix_Paths
   is
      Too_Long : constant String := (1 .. Constants.UNIX_PATH_MAX + 1 => 'a');
   begin
      Assert (Condition => Unix.Is_Valid (Path => "/tmp/foopath"),
              Message   => "Invalid path '/tmp/foopath'");
      Assert (Condition => not Unix.Is_Valid (Path => ""),
              Message   => "Valid empty path");
      Assert (Condition => not Unix.Is_Valid (Path => Too_Long),
              Message   => "Valid Path '" & Too_Long & "'");
   end Valid_Unix_Paths;

end Socket_Tests;
