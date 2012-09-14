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

package Anet.Sockets.Unix is

   type Unix_Socket_Type is abstract new Socket_Type with private;
   --  UNIX domain socket.

   procedure Bind
     (Socket : in out Unix_Socket_Type;
      Path   :        Unix_Path_Type);
   --  Bind given UNIX domain socket to path.

   type UDP_Socket_Type is new Unix_Socket_Type with private;
   --  UNIX domain socket in datagram mode.

   function Create return UDP_Socket_Type;
   --  Create new UNIX/UDP socket.

   type TCP_Socket_Type is new Unix_Socket_Type with private;
   --  UNIX domain socket in stream mode.

   function Create return TCP_Socket_Type;
   --  Create new UNIX/TCP socket.

private

   type Unix_Socket_Type is abstract new Socket_Type with null record;

   type UDP_Socket_Type is new Unix_Socket_Type with null record;

   type TCP_Socket_Type is new Unix_Socket_Type with null record;

end Anet.Sockets.Unix;
