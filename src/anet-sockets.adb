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
      Address :        IP_Addr_Type := Any_Addr_V4;
      Port    :        Port_Type;
      Iface   :        String       := "")
   is
   begin
      Thin.Set_Socket_Option
        (Socket => Socket.Sock_FD,
         Option => Reuse_Address,
         Value  => True);

      Thin.Bind_Socket (Socket  => Socket.Sock_FD,
                        Address => Address,
                        Port    => Port);

      if Iface'Length /= 0 then
         Thin.Set_Socket_Option
           (Socket => Socket.Sock_FD,
            Level  => Thin.Socket_Level,
            Option => Bind_To_Device,
            Value  => Iface);
      end if;
   end Bind;

   -------------------------------------------------------------------------

   procedure Bind
     (Socket : in out Socket_Type;
      Iface  :        String)
   is
   begin
      Thin.Bind_Socket (Socket => Socket.Sock_FD,
                        Iface  => Iface);
   end Bind;

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
      Group  : IP_Addr_Type;
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
      Src    : out Sender_Info_Type;
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
     (Socket   : Socket_Type;
      Item     : Ada.Streams.Stream_Element_Array;
      Dst_IP   : IP_Addr_Type;
      Dst_Port : Port_Type)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Len : Ada.Streams.Stream_Element_Offset;
   begin
      Thin.Send_Socket (Socket   => Socket.Sock_FD,
                        Data     => Item,
                        Last     => Len,
                        Dst_IP   => Dst_IP,
                        Dst_Port => Dst_Port);

      if Len /= Item'Length then
         raise Socket_Error with "Incomplete send operation to "
           & To_String (Address => Dst_IP) & ", only" & Len'Img & " of"
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

   function To_IP_Addr (Str : String) return IP_Addr_Type
   is
   begin
      if Str = "" then
         raise Constraint_Error with "Unable to convert empty string to IP";
      end if;

      declare
         Addr : IPv4_Addr_Type;
      begin
         Addr := To_IPv4_Addr (Str);

         return IP : IP_Addr_Type (Family => Family_Inet) do
            IP.Addr_V4 := Addr;
         end return;

      exception
         when Constraint_Error => null;

            --  Ignore constraint error because we will try to convert the
            --  string to an IPv6 address.

      end;

      declare
         Addr : IPv6_Addr_Type;
      begin
         Addr := To_IPv6_Addr (Str);

         return IP : IP_Addr_Type (Family => Family_Inet6) do
            IP.Addr_V6 := Addr;
         end return;
      end;
   end To_IP_Addr;

   -------------------------------------------------------------------------

   function To_IP_Addr
     (Data : Ada.Streams.Stream_Element_Array)
      return IP_Addr_Type
   is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      if Data'Length = 4 then
         return (Family  => Family_Inet,
                 Addr_V4 => (1 => Byte (Data (Data'First)),
                             2 => Byte (Data (Data'First + 1)),
                             3 => Byte (Data (Data'First + 2)),
                             4 => Byte (Data (Data'Last))));
      elsif Data'Length = 16 then
         return (Family  => Family_Inet6,
                 Addr_V6 => (1  => Byte (Data (Data'First)),
                             2  => Byte (Data (Data'First +  1)),
                             3  => Byte (Data (Data'First +  2)),
                             4  => Byte (Data (Data'First +  3)),
                             5  => Byte (Data (Data'First +  4)),
                             6  => Byte (Data (Data'First +  5)),
                             7  => Byte (Data (Data'First +  6)),
                             8  => Byte (Data (Data'First +  7)),
                             9  => Byte (Data (Data'First +  8)),
                             10 => Byte (Data (Data'First +  9)),
                             11 => Byte (Data (Data'First + 10)),
                             12 => Byte (Data (Data'First + 11)),
                             13 => Byte (Data (Data'First + 12)),
                             14 => Byte (Data (Data'First + 13)),
                             15 => Byte (Data (Data'First + 14)),
                             16 => Byte (Data (Data'Last))));
      end if;

      raise Constraint_Error with
        "Invalid data size for IP address conversion:" & Data'Length'Img;
   end To_IP_Addr;

   -------------------------------------------------------------------------

   function To_String (Address : IP_Addr_Type) return String
   is
   begin
      case Address.Family is
         when Family_Inet  =>
            return To_String (Address => Address.Addr_V4);
         when Family_Inet6 =>
            return To_String (Address => Address.Addr_V6);
      end case;
   end To_String;

end Anet.Sockets;
