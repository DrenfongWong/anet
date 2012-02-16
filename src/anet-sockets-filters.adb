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

with Anet.Constants;

package body Anet.Sockets.Filters is

   type Sock_Fprog_Type is record
      Len    : Interfaces.C.unsigned_short;
      --  Number of filter blocks.
      Filter : System.Address;
   end record;
   --  Required for SO_ATTACH_FILTER (struct sock_fprog).

   -------------------------------------------------------------------------

   procedure Set_Filter
     (Socket : Socket_Type;
      Filter : Sock_Filter_Array)
   is
      use type Interfaces.C.int;

      Res  : Interfaces.C.int;
      Meta : Sock_Fprog_Type := (Len    => Filter'Length,
                                 Filter => Filter'Address);
   begin
      Res := C_Setsockopt
        (S       => Interfaces.C.int (Socket.Sock_FD),
         Level   => Constants.Sys.SOL_SOCKET,
         Optname => Constants.SO_ATTACH_FILTER,
         Optval  => Meta'Address,
         Optlen  => Meta'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable set socket filter - "
           & Get_Errno_String;
      end if;
   end Set_Filter;

end Anet.Sockets.Filters;
