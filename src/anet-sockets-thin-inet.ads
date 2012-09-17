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

package Anet.Sockets.Thin.Inet is

   type Sockaddr_In_Type (Family : Family_Inet_Type := Family_Inet) is record
      Sin_Family : Interfaces.C.unsigned_short;
      --  Address family
      Sin_Port   : Interfaces.C.unsigned_short;
      --  Port in network byte order

      case Family is
         when Family_Inet =>
            Sin_Addr : IPv4_Addr_Type      := (others => 0);
            --  IPv4 address
            Sin_Zero : Byte_Array (1 .. 8) := (others => 0);
            --  Padding
         when Family_Inet6 =>
            Sin_Flowinfo : Interfaces.C.unsigned;
            --  IPv6 flow information
            Sin6_Addr    : IPv6_Addr_Type := (others => 0);
            --  IPv6 address
            Sin_Scope_ID : Interfaces.C.unsigned;
            --  Scope ID
      end case;
   end record;
   pragma Unchecked_Union (Sockaddr_In_Type);
   pragma Convention (C, Sockaddr_In_Type);
   --  Low-level Internet socket address type (struct sockaddr_in, struct
   --  sockaddr_in6).

   procedure Bind
     (Socket  :     Integer;
      Address :     Sockaddr_In_Type;
      Success : out Boolean);
   --  Bind given socket to specified sockaddr. Success is set to True if the
   --  bind operation succeeded, False otherwise.

   procedure Send
     (Socket  :     Integer;
      Data    :     Ada.Streams.Stream_Element_Array;
      Last    : out Ada.Streams.Stream_Element_Offset;
      Dst     :     Sockaddr_In_Type;
      Success : out Boolean);
   --  Send data to another socket specified by destination. Last is the index
   --  value which designates the last sent stream element. Success is set to
   --  True if the send operation succeeded, False otherwise.

   procedure Receive
     (Socket :     Integer;
      Data   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset;
      Source : out Socket_Addr_Type);
   --  Receive data from given socket. Last is the index value which designates
   --  the last stream element in data. The source IP and port specify the
   --  sender socket from which the data was received.

   procedure Connect
     (Socket  :     Integer;
      Dst     :     Sockaddr_In_Type;
      Success : out Boolean);
   --  Connect given socket to specified destination address. Success is set to
   --  True if the connect operation succeeded, False otherwise.

   procedure Get_Socket_Info
     (Sock_Addr :     Sockaddr_In_Type;
      Source    : out Socket_Addr_Type);
   --  Get IP address and port from given low-level inet sock address.

   function To_Sock_Addr (Address : Socket_Addr_Type) return Sockaddr_In_Type;
   --  Return inet sock address for given socket address.

end Anet.Sockets.Thin.Inet;
