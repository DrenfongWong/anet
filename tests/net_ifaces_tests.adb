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

with Anet.Net_Ifaces;
with Anet.Sockets;

with Test_Utils;
with Test_Constants;

package body Net_Ifaces_Tests is

   use Ahven;
   use Anet;

   -------------------------------------------------------------------------

   procedure Get_Loopback_Interface_Index
   is
   begin
      declare
         Index : Positive;
         pragma Unreferenced (Index);
      begin
         Index := Net_Ifaces.Get_Iface_Index (Name => "nonexistent");
         Fail (Message => "Expected socket error (nonexistent)");

      exception
         when Sockets.Socket_Error => null;
      end;

      Assert (Condition => Net_Ifaces.Get_Iface_Index
              (Name => Test_Constants.Loopback_Iface_Name) = 1,
              Message   => "Loopback index not 1");
   end Get_Loopback_Interface_Index;

   -------------------------------------------------------------------------

   procedure Get_Loopback_Interface_IP
   is
   begin
      Assert (Condition => Net_Ifaces.Get_Iface_IP
              (Name => Test_Constants.Loopback_Iface_Name) = Loopback_Addr_V4,
              Message   => "Loopback IP not 127.0.0.1");
   end Get_Loopback_Interface_IP;

   -------------------------------------------------------------------------

   procedure Get_Loopback_Interface_Mac
   is
      use type Test_Utils.OS_Type;

      Ref_Mac : constant Hardware_Addr_Type (1 .. 6) := (others => 0);
   begin
      if Test_Utils.OS = Test_Utils.BSD then
         Skip (Message => "Not supported");
      end if;

      Assert (Condition => Net_Ifaces.Get_Iface_Mac
              (Name => Test_Constants.Loopback_Iface_Name) = Ref_Mac,
              Message   => "Loopback Mac not zero");

      declare
         Mac : Hardware_Addr_Type (1 .. 6);
         pragma Unreferenced (Mac);
      begin
         Mac := Net_Ifaces.Get_Iface_Mac (Name => "nonexistent");
         Fail (Message => "Expected socket error (nonexistent)");

      exception
         when Sockets.Socket_Error => null;
      end;
   end Get_Loopback_Interface_Mac;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Tests for network interfaces package");
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

end Net_Ifaces_Tests;
