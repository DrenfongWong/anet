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

with Anet.Sockets.Thin;

package body Anet.Sockets.Unix is

   -------------------------------------------------------------------------

   procedure Bind
     (Socket : in out Unix_Socket_Type;
      Path   :        Unix_Path_Type)
   is
   begin
      Thin.Bind_Unix_Socket (Socket => Socket.Sock_FD,
                             Path   => Path);
      Socket.Address.Path := Ada.Strings.Unbounded.To_Unbounded_String
        (String (Path));
   end Bind;

   -------------------------------------------------------------------------

   function Create return UDP_Socket_Type
   is
   begin
      return Socket : UDP_Socket_Type do
         Create (Socket => Socket,
                 Family => Family_Unix,
                 Mode   => Datagram_Socket);
      end return;
   end Create;

   -------------------------------------------------------------------------

   function Create return TCP_Socket_Type
   is
   begin
      return Socket : TCP_Socket_Type do
         Create (Socket => Socket,
                 Family => Family_Unix,
                 Mode   => Stream_Socket);
      end return;
   end Create;

end Anet.Sockets.Unix;
