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
with Anet.Constants;

package body Anet.Net_Ifaces is

   use Anet.Sockets.Thin;

   package C renames Interfaces.C;

   -------------------------------------------------------------------------

   function Get_Iface_Index (Name : Types.Iface_Name_Type) return Positive
   is
      Req : constant If_Req_Type := Query_Iface
        (Iface_Name => Name,
         Request    => If_Index);
   begin
      return Positive (Req.Ifr_Ifindex);
   end Get_Iface_Index;

   -------------------------------------------------------------------------

   function Get_Iface_IP (Name : Types.Iface_Name_Type) return IPv4_Addr_Type
   is
      Req : constant If_Req_Type := Query_Iface
        (Iface_Name => Name,
         Request    => If_Addr);

      --  The actual IP address is at range 3 .. 6. The first two bytes of the
      --  result are the sin_port value.

      Offset : constant := 2;
   begin
      return Result : IPv4_Addr_Type do
         for B in Result'Range loop
            Result (B) := Character'Pos
              (C.To_Ada
                 (Req.Ifr_Addr.Sa_Data
                    (C.size_t (B + Offset))));
         end loop;
      end return;
   end Get_Iface_IP;

   -------------------------------------------------------------------------

   function Get_Iface_Mac
     (Name : Types.Iface_Name_Type)
      return Hardware_Addr_Type
   is
      Req : constant If_Req_Type := Query_Iface
        (Iface_Name => Name,
         Request    => If_Hwaddr);
   begin
      return Result : Hardware_Addr_Type (1 .. 6) do
         for B in Result'Range loop
            Result (B) := Character'Pos
              (C.To_Ada
                 (Req.Ifr_Hwaddr.Sa_Data
                    (C.size_t (B))));
         end loop;
      end return;
   end Get_Iface_Mac;

   -------------------------------------------------------------------------

   function Is_Iface_Up (Name : Types.Iface_Name_Type) return Boolean
   is
      use type Interfaces.C.short;

      Req : constant If_Req_Type := Query_Iface
        (Iface_Name => Name,
         Request    => If_Flags);
   begin
      return (Req.Ifr_Flags mod 2) = Constants.IFF_UP;
   end Is_Iface_Up;

   -------------------------------------------------------------------------

   procedure Set_Iface_State
     (Name  : Types.Iface_Name_Type;
      State : Boolean)
   is
      use type C.int;

      Res, Sock : C.int;
   begin
      Sock := C_Socket (Domain   => Constants.Sys.AF_INET,
                        Typ      => Constants.Sys.SOCK_DGRAM,
                        Protocol => 0);

      declare
         C_Name : constant C.char_array := C.To_C (String (Name));
         Req    : aliased If_Req_Type (Name => If_Flags);
      begin
         Req.Ifr_Name (1 .. C_Name'Length) := C_Name;

         if State then
            Req.Ifr_Flags := Constants.IFF_UP;
         else
            Req.Ifr_Flags := 0;
         end if;

         Res := C_Ioctl (S   => Sock,
                         Req => Set_Requests (If_Flags),
                         Arg => Req'Access);
         if Res = C_Failure then
            raise Sockets.Socket_Error with "Ioctl (" & If_Flags'Img
              & ") failed on interface '" & String (Name) & "': "
              & Get_Errno_String;
         end if;

      exception
         when Sockets.Socket_Error =>
            Res := C_Close (Fd => Sock);
            raise;
      end;
      Res := C_Close (Fd => Sock);
      pragma Unreferenced (Res);
   end Set_Iface_State;

end Anet.Net_Ifaces;
