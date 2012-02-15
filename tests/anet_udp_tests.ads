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

with Ahven.Framework;

package Anet_UDP_Tests is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Testcase);
   --  Initialize testcase.

   procedure Create_UDP_Header;
   --  Verify UDP header creation.

   procedure Validate_UDP_Checksum;
   --  Test UDP checksum validation.

end Anet_UDP_Tests;
