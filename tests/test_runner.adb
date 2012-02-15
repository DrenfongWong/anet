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

with Ahven.Text_Runner;
with Ahven.Framework;

with Anet_Tests;
with Anet_OS_Tests;
with Anet_Util_Tests;
with Anet_Socket_Tests;
with Anet_UDP_Tests;
with Anet_IP_Tests;

procedure Test_Runner is
   use Ahven.Framework;

   S : constant Test_Suite_Access := Create_Suite (Suite_Name => "Anet tests");
begin
   Add_Test (Suite => S.all,
             T     => new Anet_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Anet_OS_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Anet_Util_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Anet_Socket_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Anet_UDP_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Anet_IP_Tests.Testcase);

   Ahven.Text_Runner.Run (Suite => S);
   Release_Suite (T => S);
end Test_Runner;
