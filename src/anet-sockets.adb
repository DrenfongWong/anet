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

with Anet.Sockets.Thin;

package body Anet.Sockets is

   function Get_Iface_Index
     (Name : String)
      return Positive
      renames Thin.Get_Iface_Index;

   function Get_Iface_Mac
     (Name : String)
      return Hardware_Addr_Type
      renames Thin.Get_Iface_Mac;

   function Get_Iface_IP
     (Name : String)
      return IPv4_Addr_Type
      renames Thin.Get_Iface_IP;

   function Is_Iface_Up
     (Name : String)
      return Boolean
      renames Thin.Is_Iface_Up;

   procedure Set_Iface_State
     (Name  : String;
      State : Boolean)
      renames Thin.Set_Iface_State;

   -------------------------------------------------------------------------

   procedure Accept_Unix
     (Socket     :     Socket_Type;
      New_Socket : out Socket_Type)
   is
   begin
      New_Socket.Family := Socket.Family;
      Thin.Accept_Socket (Socket     => Socket.Sock_FD,
                          New_Socket => New_Socket.Sock_FD);
   end Accept_Unix;

   -------------------------------------------------------------------------

   procedure Bind
     (Socket  : in out Socket_Type;
      Address :        Socket_Addr_Type := (Addr_V4 => Any_Addr, others => <>);
      Iface   :        String           := "")
   is
   begin
      Thin.Set_Socket_Option
        (Socket => Socket.Sock_FD,
         Option => Reuse_Address,
         Value  => True);

      Thin.Bind_Socket (Socket  => Socket.Sock_FD,
                        Address => Address);

      if Iface'Length /= 0 then
         Thin.Set_Socket_Option
           (Socket => Socket.Sock_FD,
            Level  => Thin.Socket_Level,
            Option => Bind_To_Device,
            Value  => Iface);
      end if;
   end Bind;

   -------------------------------------------------------------------------

   procedure Bind_Packet
     (Socket : in out Socket_Type;
      Iface  :        String)
   is
   begin
      Thin.Bind_Socket (Socket => Socket.Sock_FD,
                        Iface  => Iface);
   end Bind_Packet;

   -------------------------------------------------------------------------

   procedure Bind_Unix
     (Socket : in out Socket_Type;
      Path   :        String)
   is
   begin
      Thin.Bind_Unix_Socket (Socket => Socket.Sock_FD,
                             Path  => Path);
   end Bind_Unix;

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
      Path   :        String)
   is
   begin
      Thin.Connect_Socket (Socket => Socket.Sock_FD,
                           Path   => Path);
   end Connect;

   -------------------------------------------------------------------------

   procedure Create
     (Socket : out Socket_Type;
      Family :     Family_Type := Family_Inet;
      Mode   :     Mode_Type   := Datagram_Socket)
   is
   begin
      Thin.Create_Socket (Socket => Socket.Sock_FD,
                          Family => Family,
                          Mode   => Mode);
      Socket.Family := Family;
   end Create;

   -------------------------------------------------------------------------

   procedure Finalize (Socket : in out Socket_Type)
   is
   begin
      Socket.Close;
   end Finalize;

   -------------------------------------------------------------------------

   procedure Join_Multicast_Group
     (Socket : Socket_Type;
      Group  : Socket_Addr_Type;
      Iface  : String := "")
   is
   begin
      Thin.Join_Multicast_Group (Socket => Socket.Sock_FD,
                                 Group  => Group,
                                 Iface  => Iface);
   end Join_Multicast_Group;

   -------------------------------------------------------------------------

   procedure Listen_Unix
     (Socket  : Socket_Type;
      Backlog : Positive := 1)
   is
   begin
      Thin.Listen_Socket (Socket  => Socket.Sock_FD,
                          Backlog => Backlog);
   end Listen_Unix;

   -------------------------------------------------------------------------

   procedure Receive
     (Socket :     Socket_Type;
      Src    : out Socket_Addr_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      if Socket.Family = Family_Packet then
         Thin.Receive_Socket (Socket      => Socket.Sock_FD,
                              Data        => Item,
                              Last        => Last,
                              Src_HW_Addr => Src.HW_Addr);
      elsif Socket.Family = Family_Unix then
         Thin.Receive_Socket (Socket => Socket.Sock_FD,
                              Data   => Item,
                              Last   => Last);
      else
         Thin.Receive_Socket (Socket => Socket.Sock_FD,
                              Data   => Item,
                              Last   => Last,
                              Source => Src);
      end if;
   end Receive;

   -------------------------------------------------------------------------

   procedure Send
     (Socket : Socket_Type;
      Item   : Ada.Streams.Stream_Element_Array;
      Dst    : Socket_Addr_Type)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Len : Ada.Streams.Stream_Element_Offset;
   begin
      Thin.Send_Socket (Socket => Socket.Sock_FD,
                        Data   => Item,
                        Last   => Len,
                        Dst    => Dst);

      if Len /= Item'Length then
         raise Socket_Error with "Incomplete send operation to "
           & To_String (Address => Dst) & ", only" & Len'Img & " of"
           & Item'Length'Img & " bytes sent";
      end if;
   end Send;

   -------------------------------------------------------------------------

   procedure Send
     (Socket :     Socket_Type;
      Item   :     Ada.Streams.Stream_Element_Array;
      To     :     Hardware_Addr_Type;
      Iface  :     String)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Len : Ada.Streams.Stream_Element_Offset;
   begin
      Thin.Send_Socket
        (Socket => Socket.Sock_FD,
         Data   => Item,
         Last   => Len,
         To     => To,
         Iface  => Iface);

      if Len /= Item'Length then
         raise Socket_Error with "Incomplete packet send operation to "
           & To_String (Address => To) & ", only" & Len'Img & " of"
           & Item'Length'Img & " bytes sent";
      end if;
   end Send;

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
              & "(" & Address.Port_V4'Img & " )";
         when Family_Inet6  =>
            return To_String (Address => Address.Addr_V6)
              & "(" & Address.Port_V6'Img & " )";
         when Family_Packet =>
            return To_String (Address => Address.HW_Addr);
         when Family_Unix   =>
            return Ada.Strings.Unbounded.To_String (Source => Address.Path);
      end case;
   end To_String;

end Anet.Sockets;
