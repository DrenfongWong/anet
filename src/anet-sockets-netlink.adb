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

with Interfaces.C;

with Anet.Sockets.Thin;

package body Anet.Sockets.Netlink is

   package C renames Interfaces.C;

   -------------------------------------------------------------------------

   procedure Bind
     (Socket  : in out Netlink_Socket_Type;
      Address :        Netlink_Addr_Type)
   is
      Res   : C.int;
      Value : Thin.Sockaddr_Nl_Type
        := (Nl_Pid => Interfaces.Unsigned_32 (Address),
            others => <>);
   begin
      Res := Thin.C_Bind (S       => Socket.Sock_FD,
                          Name    => Value'Address,
                          Namelen => Value'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable to bind Netlink socket - "
           & Get_Errno_String;
      end if;
   end Bind;

   -------------------------------------------------------------------------

   procedure Init (Socket : in out Raw_Socket_Type)
   is
   begin
      Init (Socket => Socket,
            Family => Family_Netlink,
            Mode   => Raw_Socket);
   end Init;

   -------------------------------------------------------------------------

   procedure Receive
     (Socket :     Netlink_Socket_Type;
      Src    : out Netlink_Addr_Type;
      Data   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Res   : C.int;
      Saddr : Thin.Sockaddr_Nl_Type;
      Len   : aliased C.int := Saddr'Size / 8;
   begin
      Res := Thin.C_Recvfrom (S       => Socket.Sock_FD,
                              Msg     => Data'Address,
                              Len     => Data'Length,
                              Flags   => 0,
                              From    => Saddr'Address,
                              Fromlen => Len'Access);

      if Res = C_Failure then
         raise Socket_Error with "Error receiving data from Netlink socket: "
           & Get_Errno_String;
      end if;

      Src  := Netlink_Addr_Type (Saddr.Nl_Pid);
      Last := Data'First + Ada.Streams.Stream_Element_Offset (Res - 1);
   end Receive;

   -------------------------------------------------------------------------

   procedure Send
     (Socket : Netlink_Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      To     : Netlink_Addr_Type := 0)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Res        : C.int;
      Sent_Bytes : Ada.Streams.Stream_Element_Offset;
      Dst        : Thin.Sockaddr_Nl_Type
        := (Nl_Pid => Interfaces.Unsigned_32 (To),
            others => <>);
   begin
      Res := Thin.C_Sendto (S     => Socket.Sock_FD,
                            Buf   => Item'Address,
                            Len   => Item'Length,
                            Flags => 0,
                            To    => Dst'Address,
                            Tolen => Dst'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable to send data on Netlink socket - "
           & Get_Errno_String;
      end if;

      Sent_Bytes := Item'First + Ada.Streams.Stream_Element_Offset (Res - 1);
      if Sent_Bytes /= Item'Length then
         raise Socket_Error with "Incomplete Netlink send operation, only"
           & Sent_Bytes'Img & " of" & Item'Length'Img & " bytes sent";
      end if;
   end Send;

end Anet.Sockets.Netlink;
