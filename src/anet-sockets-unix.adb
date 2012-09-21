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

with Anet.OS;
with Anet.Sockets.Thin.Unix;

package body Anet.Sockets.Unix is

   -------------------------------------------------------------------------

   procedure Accept_Connection
     (Socket     :     TCP_Socket_Type;
      New_Socket : out TCP_Socket_Type)
   is
      Sock_Un   : Thin.Sockaddr_Un_Type;
      Sock_Addr : System.Address;
      Sock_Len  : Integer := 0;
   begin
      New_Socket.Path := Socket.Path;
      Sock_Addr       := Sock_Un'Address;
      Sock_Len        := Sock_Un'Size / 8;

      Thin.Accept_Socket (Socket       => Socket.Sock_FD,
                          Sockaddr     => Sock_Addr,
                          Sockaddr_Len => Sock_Len,
                          New_Socket   => New_Socket.Sock_FD);
   end Accept_Connection;

   -------------------------------------------------------------------------

   procedure Bind
     (Socket : in out Unix_Socket_Type;
      Path   :        Types.Unix_Path_Type)
   is
      Result : Boolean;
   begin
      Thin.Unix.Bind (Socket  => Socket.Sock_FD,
                      Path    => Path,
                      Success => Result);

      if not Result then
         raise Socket_Error with "Unable to bind unix socket to path "
           & String (Path) & " - " & Get_Errno_String;
      end if;

      Socket.Path := Ada.Strings.Unbounded.To_Unbounded_String
        (String (Path));
   end Bind;

   -------------------------------------------------------------------------

   procedure Close (Socket : in out Unix_Socket_Type)
   is
   begin
      if Socket.Sock_FD /= -1 then
         OS.Delete_File (Filename => Ada.Strings.Unbounded.To_String
                         (Socket.Path));
         Socket_Type (Socket).Close;
      end if;
   end Close;

   -------------------------------------------------------------------------

   procedure Connect
     (Socket : in out Unix_Socket_Type;
      Path   :        Types.Unix_Path_Type)
   is
      Result : Boolean;
   begin
      Thin.Unix.Connect
        (Socket  => Socket.Sock_FD,
         Path    => Path,
         Success => Result);

      if not Result then
         raise Socket_Error with "Unable to connect unix socket to path "
           & String (Path) & " - " & Get_Errno_String;
      end if;
   end Connect;

   -------------------------------------------------------------------------

   procedure Init (Socket : in out UDP_Socket_Type)
   is
   begin
      Init (Socket => Socket,
            Family => Family_Unix,
            Mode   => Datagram_Socket);
   end Init;

   -------------------------------------------------------------------------

   procedure Init (Socket : in out TCP_Socket_Type)
   is
   begin
      Init (Socket => Socket,
            Family => Family_Unix,
            Mode   => Stream_Socket);
   end Init;

   -------------------------------------------------------------------------

   procedure Receive
     (Socket :     UDP_Socket_Type;
      Src    : out Types.Unix_Path_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      Thin.Receive_Socket (Socket => Socket.Sock_FD,
                           Data   => Item,
                           Last   => Last);
      Src := Types.Unix_Path_Type (Ada.Strings.Unbounded.To_String
                                   (Socket.Path));
   end Receive;

end Anet.Sockets.Unix;
