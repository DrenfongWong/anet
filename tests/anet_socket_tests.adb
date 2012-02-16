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

with Anet.OS;
with Anet.Sockets;
with Anet.Test_Utils;

package body Anet_Socket_Tests is

   use Ahven;
   use Anet;
   use Anet.Sockets;

   -------------------------------------------------------------------------

   procedure Get_Loopback_Interface_Index
   is
   begin
      declare
         Too_Long : constant String (2 .. 18) := (others => 'b');
         Index    : Positive;
         pragma Unreferenced (Index);
      begin
         Index := Get_Iface_Index (Name => Too_Long);
         Fail (Message => "Expected socket error (too long)");

      exception
         when Socket_Error => null;
      end;

      declare
         Index : Positive;
         pragma Unreferenced (Index);
      begin
         Index := Get_Iface_Index (Name => "nonexistent");
         Fail (Message => "Expected socket error (nonexistent)");

      exception
         when Socket_Error => null;
      end;

      declare
         Valid : constant String (2 .. 3) := "lo";
      begin
         Assert (Condition => Get_Iface_Index (Name => Valid) = 1,
                 Message   => "Loopback index not 1");
      end;
   end Get_Loopback_Interface_Index;

   -------------------------------------------------------------------------

   procedure Get_Loopback_Interface_IP
   is
      Ref_IP : constant IPv4_Addr_Type := (127, 0, 0, 1);
   begin
      Assert (Condition => Get_Iface_IP (Name => "lo") = Ref_IP,
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
        (Routine => Send_V4'Access,
         Name    => "Send data (IPv4)");
      T.Add_Test_Routine
        (Routine => Send_V6'Access,
         Name    => "Send data (IPv6)");
      T.Add_Test_Routine
        (Routine => Send_Multicast_V6'Access,
         Name    => "Send data (IPv6 multicast)");
      T.Add_Test_Routine
        (Routine => Send_Unix'Access,
         Name    => "Send data (Unix)");
      T.Add_Test_Routine
        (Routine => Receive_V4'Access,
         Name    => "Receive data (IPv4)");
      T.Add_Test_Routine
        (Routine => Receive_V6'Access,
         Name    => "Receive data (IPv6)");
      T.Add_Test_Routine
        (Routine => Receive_Multicast_V6'Access,
         Name    => "Receive data (IPv6 multicast)");
      T.Add_Test_Routine
        (Routine => Receive_Unix'Access,
         Name    => "Receive data (Unix)");
      T.Add_Test_Routine
        (Routine => Listen_Callbacks'Access,
         Name    => "Data reception callback handling");
      T.Add_Test_Routine
        (Routine => IP_Addr_Conversion'Access,
         Name    => "IP address type conversion");
      T.Add_Test_Routine
        (Routine => Get_Loopback_Interface_Index'Access,
         Name    => "Get iface index for loopback");
      T.Add_Test_Routine
        (Routine => Get_Loopback_Interface_Mac'Access,
         Name    => "Get iface hw addr for loopback");
      T.Add_Test_Routine
        (Routine => Get_Loopback_Interface_IP'Access,
         Name    => "Get iface IP addr for loopback");
   end Initialize;

   -------------------------------------------------------------------------

   procedure IP_Addr_Conversion
   is
      use Ada.Streams;

      A1_Str : constant String       := "0.0.0.0";
      A2_Str : constant String       := "10.1.1.42";
      A3_Str : constant String
        := "FF02:0000:0000:0000:0000:0000:0001:0002";
      A2_Dat : constant Stream_Element_Array := (10, 1, 1, 42);
      A3_Dat : constant Stream_Element_Array
        := (255, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2);

      Addr1  : constant IP_Addr_Type := To_IP_Addr (Str => A1_Str);
      Addr2  : constant IP_Addr_Type := To_IP_Addr (Str => A2_Str);
      Addr3  : constant IP_Addr_Type := To_IP_Addr (Str => A3_Str);
   begin
      Assert (Condition => To_String (Address => Addr1) = A1_Str,
              Message   => "Addr1 mismatch");
      Assert (Condition => To_String (Address => Addr2) = A2_Str,
              Message   => "Addr2 mismatch");
      Assert (Condition => To_String (Address => Addr3) = A3_Str,
              Message   => "Addr3 mismatch");

      Assert (Condition => To_IP_Addr (Data => A2_Dat) = Addr2,
              Message   => "Addr2 mismatch (data)");
      Assert (Condition => To_IP_Addr (Data => A3_Dat) = Addr3,
              Message   => "Addr3 mismatch (data)");

      declare
         Address : IP_Addr_Type;
         pragma Unreferenced (Address);
      begin
         Address := To_IP_Addr (Str => "");
         Fail (Message => "Expected constraint error");

      exception
         when Constraint_Error => null;
      end;

      declare
         Address : IP_Addr_Type;
         pragma Unreferenced (Address);
      begin
         Address := To_IP_Addr (Data => (1 .. 5 => 0));
         Fail (Message => "Expected constraint error (data)");

      exception
         when Constraint_Error => null;
      end;

   end IP_Addr_Conversion;

   -------------------------------------------------------------------------

   procedure Listen_Callbacks
   is
      C    : Count_Type := 0;
      Sock : Socket_Type;
   begin
      Sock.Create;
      Sock.Bind (Address => Loopback_Addr_V4,
                 Port    => Test_Utils.Listen_Port);
      Sock.Listen (Callback => Test_Utils.Dump'Access);

      Anet.Test_Utils.Send_Data (Filename => "data/chunk1.dat");

      for I in 1 .. 30 loop
         C := Get_Rcv_Msg_Count (Socket => Sock);
         exit when C > 0;
         delay 0.1;
      end loop;

      Sock.Stop;

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         Sock.Stop;
         OS.Delete_File (Filename => Test_Utils. Dump_File);
         raise;
   end Listen_Callbacks;

   -------------------------------------------------------------------------

   procedure Receive_Multicast_V6
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Socket_Type;
      Grp    : constant IP_Addr_Type := To_IP_Addr
        (Str => "ff01:0000:0000:0000:0000:0000:0001:0002");

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sender : Sender_Info_Type;
      begin
         Sock.Create (Family => Family_Inet6);
         Sock.Bind (Address => Grp,
                    Port    => Test_Utils.Listen_Port);
         Sock.Join_Multicast_Group (Group => Grp);

         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);
         Test_Utils.Dump (Data => Buffer (Buffer'First .. Last),
                          Src  => Sender);
         accept Wait;
      end Receiver;
   begin
      Anet.Test_Utils.Send_Data (Dst_IP   => Grp,
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

   procedure Receive_Unix
   is
      Path   : constant String := "obj/mysock2";
      Cmd    : constant String := "socat -u EXEC:'cat data/chunk1.dat"
        & "' UNIX-CLIENT:" & Path;

      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Socket_Type;

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sender : Sender_Info_Type;
         S2     : Socket_Type;
      begin
         begin
            Sock.Create (Family => Family_Unix,
                         Mode   => Stream_Socket);
            Sock.Bind_Unix (Path => Path);
            Sock.Listen_Unix;
            Sock.Accept_Unix (New_Socket => S2);
            S2.Receive (Src  => Sender,
                        Item => Buffer,
                        Last => Last);
            Test_Utils.Dump (Data => Buffer (Buffer'First .. Last),
                             Src  => Sender);
         end;

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
   end Receive_Unix;

   -------------------------------------------------------------------------

   procedure Receive_V4
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1500);
      Last   : Ada.Streams.Stream_Element_Offset;
      Sock   : Socket_Type;

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
         Sender : Sender_Info_Type;
      begin
         Sock.Create;
         Sock.Bind (Address => Loopback_Addr_V4,
                    Port    => Test_Utils.Listen_Port);
         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);
         Test_Utils.Dump (Data => Buffer (Buffer'First .. Last),
                          Src  => Sender);
         accept Wait;
      end Receiver;
   begin
      Anet.Test_Utils.Send_Data (Filename => "data/chunk1.dat");

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
   end Receive_V4;

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
         Sender : Sender_Info_Type;
      begin
         Sock.Create (Family => Family_Inet6);
         Sock.Bind (Address => Loopback_Addr_V6,
                    Port    => Test_Utils.Listen_Port);

         Sock.Receive (Src  => Sender,
                       Item => Buffer,
                       Last => Last);
         Test_Utils.Dump (Data => Buffer (Buffer'First .. Last),
                          Src  => Sender);
         accept Wait;
      end Receiver;
   begin
      Anet.Test_Utils.Send_Data (Dst_IP   => Loopback_Addr_V6,
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

   procedure Send_Multicast_V6
   is
      Data : constant Ada.Streams.Stream_Element_Array
        := OS.Read_File (Filename => "data/chunk1.dat");
      C    : Count_Type := 0;
      Sock : Socket_Type;
      Grp  : constant IP_Addr_Type := To_IP_Addr
        (Str => "ff01:0000:0000:0000:0000:0000:0001:0002");
   begin
      Sock.Create (Family => Family_Inet6);

      Sock.Bind (Address => Grp,
                 Port    => Test_Utils.Listen_Port);

      Sock.Join_Multicast_Group (Group => Grp);

      Sock.Listen (Callback => Test_Utils.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item     => Data,
                 Dst_IP   => Grp,
                 Dst_Port => Test_Utils.Listen_Port);

      for I in 1 .. 30 loop
         C := Get_Rcv_Msg_Count (Socket => Sock);
         exit when C > 0;
         delay 0.1;
      end loop;

      Sock.Stop;

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         Sock.Stop;
         OS.Delete_File (Filename => Test_Utils.Dump_File);
         raise;
   end Send_Multicast_V6;

   -------------------------------------------------------------------------

   procedure Send_Unix
   is
      Data : constant Ada.Streams.Stream_Element_Array
        := OS.Read_File (Filename => "data/chunk1.dat");

      Path : constant String := "/tmp/mysock";
      Cmd  : constant String := "socat UNIX-LISTEN:" & Path & " "
        & Test_Utils.Dump_File;
      Sock : Socket_Type;

      task Receiver is
         entry Wait;
      end Receiver;

      task body Receiver is
      begin
         OS.Execute (Command => Cmd);

         accept Wait;
      end Receiver;
   begin
      Sock.Create (Family => Family_Unix,
                   Mode   => Stream_Socket);

      --  Give receiver/socat enough time to create socket

      delay 0.1;

      Sock.Connect (Path => Path);
      Sock.Send (Item => Data);
      Sock.Stop;

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
         Sock.Stop;
         if not Receiver'Terminated then
            abort Receiver;
         end if;
         OS.Delete_File (Filename => Test_Utils.Dump_File);
         raise;
   end Send_Unix;

   -------------------------------------------------------------------------

   procedure Send_V4
   is
      Data : constant Ada.Streams.Stream_Element_Array
        := OS.Read_File (Filename => "data/chunk1.dat");
      C    : Count_Type := 0;
      Sock : Socket_Type;
   begin
      Sock.Create;
      Sock.Bind (Address => Loopback_Addr_V4,
                 Port    => Test_Utils.Listen_Port);

      Sock.Listen (Callback => Test_Utils.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item     => Data,
                 Dst_IP   => Loopback_Addr_V4,
                 Dst_Port => Test_Utils.Listen_Port);

      for I in 1 .. 30 loop
         C := Get_Rcv_Msg_Count (Socket => Sock);
         exit when C > 0;
         delay 0.1;
      end loop;

      Sock.Stop;

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         Sock.Stop;
         OS.Delete_File (Filename => Test_Utils.Dump_File);
         raise;
   end Send_V4;

   -------------------------------------------------------------------------

   procedure Send_V6
   is
      Data : constant Ada.Streams.Stream_Element_Array
        := OS.Read_File (Filename => "data/chunk1.dat");
      C    : Count_Type := 0;
      Sock : Socket_Type;
   begin
      Sock.Create (Family => Family_Inet6);
      Sock.Bind (Address => Loopback_Addr_V6,
                 Port    => Test_Utils.Listen_Port);

      Sock.Listen (Callback => Test_Utils.Dump'Access);

      --  Precautionary delay to make sure receiver task is ready.

      delay 0.2;

      Sock.Send (Item     => Data,
                 Dst_IP   => Loopback_Addr_V6,
                 Dst_Port => Test_Utils.Listen_Port);

      for I in 1 .. 30 loop
         C := Get_Rcv_Msg_Count (Socket => Sock);
         exit when C > 0;
         delay 0.1;
      end loop;

      Sock.Stop;

      Assert (Condition => C = 1,
              Message   => "Message count not 1:" & C'Img);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/chunk1.dat",
               Filename2 => Test_Utils.Dump_File),
              Message   => "Result mismatch");

      OS.Delete_File (Filename => Test_Utils.Dump_File);

   exception
      when others =>
         Sock.Stop;
         OS.Delete_File (Filename => Test_Utils.Dump_File);
         raise;
   end Send_V6;

end Anet_Socket_Tests;
