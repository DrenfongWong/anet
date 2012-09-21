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

package body Anet.Sockets.Thin is

   package C renames Interfaces.C;

   function Ioctl_Get
     (Socket     : C.int;
      Request    : Netdev_Request_Name;
      Iface_Name : Types.Iface_Name_Type)
      return If_Req_Type;
   --  Execute netdevice ioctl get request on interface with given name. The
   --  specified socket must have been created beforehand. The procedure
   --  returns the resulting interface request record to the caller.

   -------------------------------------------------------------------------

   function Ioctl_Get
     (Socket     : C.int;
      Request    : Netdev_Request_Name;
      Iface_Name : Types.Iface_Name_Type)
      return If_Req_Type
   is
      Res    : C.int;
      C_Name : constant C.char_array := C.To_C (String (Iface_Name));
      If_Req : aliased If_Req_Type (Name => Request);
   begin
      If_Req.Ifr_Name (1 .. C_Name'Length) := C_Name;
      Res := C_Ioctl (S   => Socket,
                      Req => Get_Requests (Request),
                      Arg => If_Req'Access);
      if Res = C_Failure then
         raise Socket_Error with "Ioctl (" & Request'Img
           & ") failed on interface '" & String (Iface_Name) & "': "
           & Get_Errno_String;
      end if;

      return If_Req;
   end Ioctl_Get;

   -------------------------------------------------------------------------

   function Query_Iface
     (Iface_Name : Types.Iface_Name_Type;
      Request    : Netdev_Request_Name)
      return If_Req_Type
   is
      Req  : If_Req_Type;
      Sock : C.int;
      Res  : C.int;
      pragma Unreferenced (Res);
      --  Ignore socket close errors.
   begin
      Sock := C_Socket (Domain   => Constants.Sys.AF_INET,
                        Typ      => Constants.Sys.SOCK_DGRAM,
                        Protocol => 0);

      begin
         Req := Ioctl_Get (Socket     => Sock,
                           Request    => Request,
                           Iface_Name => Iface_Name);

      exception
         when Socket_Error =>
            Res := C_Close (Fd => Sock);
            raise;
      end;
      Res := C_Close (Fd => Sock);

      return Req;
   end Query_Iface;

end Anet.Sockets.Thin;
