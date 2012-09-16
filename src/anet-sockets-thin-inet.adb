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

with Anet.Constants;
with Anet.Byte_Swapping;

package body Anet.Sockets.Thin.Inet is

   package C renames Interfaces.C;

   -------------------------------------------------------------------------

   procedure Bind
     (Socket  : Integer;
      Address : Socket_Addr_Type)
   is
      use type C.int;

      Res   : C.int;
      Value : constant Sockaddr_In_Type := To_Sock_Addr (Address => Address);
   begin
      Res := C_Bind (S       => C.int (Socket),
                     Name    => Value'Address,
                     Namelen => Value'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Unable to bind socket to "
           & To_String (Address => Address) & " - " & Get_Errno_String;
      end if;
   end Bind;

   -------------------------------------------------------------------------

   procedure Get_Socket_Info
     (Sock_Addr :     Sockaddr_In_Type;
      Source    : out Socket_Addr_Type)
   is
      use type Interfaces.C.unsigned_short;
   begin
      if Sock_Addr.Sin_Family = Constants.Sys.AF_INET then
         Source.Addr_V4 := Sock_Addr.Sin_Addr;
         Source.Port_V4 := Byte_Swapping.Host_To_Network
           (Input => Port_Type (Sock_Addr.Sin_Port));
      elsif Sock_Addr.Sin_Family = Constants.Sys.AF_INET6 then
         Source.Addr_V6 := Sock_Addr.Sin6_Addr;
         Source.Port_V6 := Byte_Swapping.Host_To_Network
           (Input => Port_Type (Sock_Addr.Sin_Port));
      else
         raise Socket_Error with "Invalid source address family";
      end if;
   end Get_Socket_Info;

   -------------------------------------------------------------------------

   procedure Receive
     (Socket :     Integer;
      Data   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      Source : out Socket_Addr_Type)
   is
      use type Interfaces.C.int;
      use type Ada.Streams.Stream_Element_Offset;

      Res : C.int;
      Sin : Sockaddr_In_Type;
      Len : aliased C.int := Sin'Size / 8;
   begin
      Res := C_Recvfrom (S       => C.int (Socket),
                         Msg     => Data'Address,
                         Len     => Data'Length,
                         Flags   => 0,
                         From    => Sin'Address,
                         Fromlen => Len'Access);

      if Res = C_Failure then
         raise Socket_Error with "Error receiving data: " & Get_Errno_String;
      end if;

      Last := Data'First + Ada.Streams.Stream_Element_Offset (Res - 1);

      if Len /= 0 then
         Get_Socket_Info (Sock_Addr => Sin,
                          Source    => Source);
      else
         Source := No_Addr;
      end if;
   end Receive;

   -------------------------------------------------------------------------

   procedure Send
     (Socket :     Integer;
      Data   :     Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      Dst    :     Socket_Addr_Type)
   is
      use type Interfaces.C.int;
      use type Ada.Streams.Stream_Element_Offset;

      Res : C.int;
      Sin : constant Sockaddr_In_Type := To_Sock_Addr (Address => Dst);
   begin
      Res := C_Sendto (S     => C.int (Socket),
                       Buf   => Data'Address,
                       Len   => Data'Length,
                       Flags => 0,
                       To    => Sin'Address,
                       Tolen => Sin'Size / 8);

      if Res = C_Failure then
         raise Socket_Error with "Error sending data to "
           & To_String (Address => Dst) & " - " & Get_Errno_String;
      end if;

      Last := Data'First + Ada.Streams.Stream_Element_Offset (Res - 1);
   end Send;

   -------------------------------------------------------------------------

   function To_Sock_Addr (Address : Socket_Addr_Type) return Sockaddr_In_Type
   is
   begin
      case Address.Family is
         when Family_Inet  =>
            return
              (Family     => Family_Inet,
               Sin_Family => Constants.Sys.AF_INET,
               Sin_Port   => C.unsigned_short
                 (Byte_Swapping.Host_To_Network (Input => Address.Port_V4)),
               Sin_Addr   => Address.Addr_V4,
               Sin_Zero   => <>);
         when Family_Inet6 =>
            return
              (Family     => Family_Inet6,
               Sin_Family => Constants.Sys.AF_INET6,
               Sin_Port   => C.unsigned_short
                 (Byte_Swapping.Host_To_Network (Input => Address.Port_V6)),
               Sin6_Addr  => Address.Addr_V6,
               others     => 0);
         when others =>
            raise Socket_Error with "Unable to convert '" & To_String
              (Address => Address) & "' to IPv4 or IPv6 socket address";
      end case;
   end To_Sock_Addr;

end Anet.Sockets.Thin.Inet;