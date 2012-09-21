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
     (Socket     :     Integer;
      Data       : out Ada.Streams.Stream_Element_Array;
      Last       : out Ada.Streams.Stream_Element_Offset;
      Source_Len : out Natural;
      Source     : out Sockaddr_In_Type;
      Success    : out Boolean);
   --  Receive data from given socket. Last is the index value which designates
   --  the last stream element in data.  Success is set to True if the receive
   --  operation succeeded, False otherwise.
   --
   --  Source_Len indicates the size in bytes of the senders' address. A
   --  Source_Len of zero means that the underlying protocol did not provide a
   --  sender adresses. If an address is supplied by the underlying protocol,
   --  it is returned in the sockaddr Source argument.

   procedure Connect
     (Socket  :     Integer;
      Dst     :     Sockaddr_In_Type;
      Success : out Boolean);
   --  Connect given socket to specified destination address. Success is set to
   --  True if the connect operation succeeded, False otherwise.

   procedure Join_Multicast_Group
     (Socket    :     Integer;
      Group     :     Sockaddr_In_Type;
      Iface_Idx :     Natural := 0;
      Success   : out Boolean);
   --  Join the given multicast group on the interface specified by index.
   --  Success is set to True if the join operation succeeded, False otherwise.

end Anet.Sockets.Thin.Inet;
