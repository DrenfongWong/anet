--
--  Copyright (C) 2011 secunet Security Networks AG
--  Copyright (C) 2011 Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2011 Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Text_IO;

with Ahven.Text_Runner;
with Ahven.Framework;

with Type_Tests;
with OS_Tests;
with Util_Tests;
with Socket_Tests;
with UDP_Tests;
with IP_Tests;
with Stream_Tests;
with Net_Ifaces_Tests;

procedure Test_Runner is
   use Ahven.Framework;

   Name : constant String := "Anet tests";
   S    : constant Test_Suite_Access := Create_Suite (Suite_Name => Name);
begin
   Add_Test (Suite => S.all,
             T     => new Type_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new OS_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Util_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Socket_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new UDP_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new IP_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Stream_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Net_Ifaces_Tests.Testcase);

   Ada.Text_IO.Put_Line ("Running " & Name & " ... please wait");

   Ahven.Text_Runner.Run (Suite => S);
   Release_Suite (T => S);
end Test_Runner;
