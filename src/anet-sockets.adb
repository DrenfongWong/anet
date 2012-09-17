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

with Anet.Sockets.Thin.Inet;
with Anet.Sockets.Thin.Unix;
with Anet.Sockets.Thin.Packet;

package body Anet.Sockets is

   function Get_Iface_Index
     (Name : Types.Iface_Name_Type)
      return Positive
      renames Thin.Get_Iface_Index;

   function Get_Iface_Mac
     (Name : Types.Iface_Name_Type)
      return Hardware_Addr_Type
      renames Thin.Get_Iface_Mac;

   function Get_Iface_IP
     (Name : Types.Iface_Name_Type)
      return IPv4_Addr_Type
      renames Thin.Get_Iface_IP;

   function Is_Iface_Up
     (Name : Types.Iface_Name_Type)
      return Boolean
      renames Thin.Is_Iface_Up;

   procedure Set_Iface_State
     (Name  : Types.Iface_Name_Type;
      State : Boolean)
      renames Thin.Set_Iface_State;

   -------------------------------------------------------------------------

   procedure Accept_Connection
     (Socket     :     Socket_Type;
      New_Socket : out Socket_Type)
   is
      Sock_In   : Thin.Inet.Sockaddr_In_Type (Family => Family_Inet);
      Sock_In6  : Thin.Inet.Sockaddr_In_Type (Family => Family_Inet6);
      Sock_Un   : Thin.Unix.Sockaddr_Un_Type;
      Sock_Addr : System.Address;
      Sock_Len  : Integer := 0;
   begin
      New_Socket.Address := Socket.Address;

      case Socket.Address.Family is
         when Family_Inet  =>
            Sock_Addr := Sock_In'Address;
            Sock_Len  := Sock_In'Size / 8;
         when Family_Inet6 =>
            Sock_Addr := Sock_In6'Address;
            Sock_Len  := Sock_In6'Size / 8;
         when Family_Unix  =>
            Sock_Addr := Sock_Un'Address;
            Sock_Len  := Sock_Un'Size / 8;
         when others       =>
            raise Socket_Error with "Accept operation not supported for "
              & Socket.Address.Family'Img & " sockets";
      end case;

      Thin.Accept_Socket (Socket       => Socket.Sock_FD,
                          Sockaddr     => Sock_Addr,
                          Sockaddr_Len => Sock_Len,
                          New_Socket   => New_Socket.Sock_FD);
   end Accept_Connection;

   -------------------------------------------------------------------------

   procedure Close (Socket : in out Socket_Type)
   is
   begin
      if Socket.Sock_FD /= -1 then
         Thin.Close_Socket (Socket => Socket.Sock_FD);
         Socket.Sock_FD := -1;
      end if;
   end Close;

   -------------------------------------------------------------------------

   procedure Connect
     (Socket : in out Socket_Type;
      Dst    :        Socket_Addr_Type)
   is
   begin
      Thin.Connect_Socket (Socket => Socket.Sock_FD,
                           Dst    => Dst);
   end Connect;

   -------------------------------------------------------------------------

   procedure Create
     (Socket : in out Socket_Type;
      Family :        Family_Type;
      Mode   :        Mode_Type)
   is
      Addr : Socket_Addr_Type (Family => Family);
   begin
      Thin.Create_Socket (Socket => Socket.Sock_FD,
                          Family => Family,
                          Mode   => Mode);
      Socket.Address := Addr;
   end Create;

   -------------------------------------------------------------------------

   procedure Finalize (Socket : in out Socket_Type)
   is
   begin
      Socket_Type'Class (Socket).Close;
   end Finalize;

   -------------------------------------------------------------------------

   procedure Join_Multicast_Group
     (Socket : Socket_Type;
      Group  : Socket_Addr_Type;
      Iface  : Types.Iface_Name_Type := "")
   is
   begin
      Thin.Join_Multicast_Group (Socket => Socket.Sock_FD,
                                 Group  => Group,
                                 Iface  => Iface);
   end Join_Multicast_Group;

   -------------------------------------------------------------------------

   procedure Listen
     (Socket  : Socket_Type;
      Backlog : Positive := 1)
   is
   begin
      Thin.Listen_Socket (Socket  => Socket.Sock_FD,
                          Backlog => Backlog);
   end Listen;

   -------------------------------------------------------------------------

   procedure Receive
     (Socket :     Socket_Type;
      Src    : out Socket_Addr_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      if Socket.Address.Family = Family_Packet then
         Thin.Packet.Receive (Socket      => Socket.Sock_FD,
                              Data        => Item,
                              Last        => Last,
                              Src_HW_Addr => Src.HW_Addr);
      elsif Socket.Address.Family = Family_Unix then
         Thin.Unix.Receive (Socket => Socket.Sock_FD,
                            Data   => Item,
                            Last   => Last);
      else
         Thin.Inet.Receive (Socket => Socket.Sock_FD,
                            Data   => Item,
                            Last   => Last,
                            Source => Src);
      end if;
   end Receive;

   -------------------------------------------------------------------------

   procedure Send
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Len : Ada.Streams.Stream_Element_Offset;
   begin
      Thin.Send_Socket
        (Socket => Socket.Sock_FD,
         Data   => Item,
         Last   => Len);

      if Len /= Item'Length then
         raise Socket_Error with "Incomplete send operation on unix socket"
         & ", only" & Len'Img & " of" & Item'Length'Img & " bytes sent";
      end if;
   end Send;

   -------------------------------------------------------------------------

   procedure Set_Socket_Option
     (Socket : Socket_Type;
      Option : Option_Name_Bool;
      Value  : Boolean)
   is
   begin
      Thin.Set_Socket_Option
        (Socket => Socket.Sock_FD,
         Option => Option,
         Value  => Value);
   end Set_Socket_Option;

   -------------------------------------------------------------------------

   procedure Set_Socket_Option
     (Socket : Socket_Type;
      Option : Option_Name_Str;
      Value  : String)
   is
   begin
      Thin.Set_Socket_Option
        (Socket => Socket.Sock_FD,
         Option => Option,
         Value  => Value);
   end Set_Socket_Option;

   -------------------------------------------------------------------------

   function To_String (Address : Socket_Addr_Type) return String
   is
   begin
      case Address.Family is
         when Family_Inet   =>
            return To_String (Address => Address.Addr_V4)
              & " (" & Address.Port_V4'Img & " )";
         when Family_Inet6  =>
            return To_String (Address => Address.Addr_V6)
              & " (" & Address.Port_V6'Img & " )";
         when Family_Packet =>
            return To_String (Address => Address.HW_Addr);
         when Family_Unix   =>
            return Ada.Strings.Unbounded.To_String (Source => Address.Path);
      end case;
   end To_String;

end Anet.Sockets;
