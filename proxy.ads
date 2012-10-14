-- Copyright (C) 2012 Aaron Riekenberg (aaron.riekenberg@gmail.com)
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in 
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Finalization;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Epoll;
with GNAT.Sockets;
with Socket_Util;

package Proxy is

  pragma Elaborate_Body;

  type Proxy_T is new Ada.Finalization.Limited_Controlled with private;
  type Proxy_Ptr_T is access Proxy_T;

  type Address_And_Port is
    record
      Address : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String;
      Port : Natural := 0;
    end record;

  type Address_And_Port_Array is 
    array (Positive range <>) of Address_And_Port;

  Default_Buffer_Size : constant Ada.Streams.Stream_Element_Offset := 65_536;

  function Create(
    Listen_Address_And_Ports : in Address_And_Port_Array;
    Remote_Address_And_Port : in Address_And_Port;
    Buffer_Size : in Ada.Streams.Stream_Element_Offset := Default_Buffer_Size)
    return Proxy_Ptr_T;

  procedure Run(
    Proxy : in out Proxy_T);

  procedure Poll(
    Proxy : in out Proxy_T;
    Block : in Boolean := False);

  procedure Destroy(
    Proxy : in out Proxy_Ptr_T);

private

  Null_File_Descriptor : constant Integer := -1;

  package Server is

    type Server_Socket_Info_T is
      new Ada.Finalization.Limited_Controlled with private;
    type Server_Socket_Info_Ptr_T is access Server_Socket_Info_T;

    function Create(
      Proxy_Ptr : in Proxy_Ptr_T;
      Listen_Address_And_Port : in Address_And_Port) return Server_Socket_Info_Ptr_T;

    function Get_File_Descriptor(
      Server_Socket_Info : in Server_Socket_Info_T) return Integer;

    function Get_Listen_Address(
      Server_Socket_Info : in Server_Socket_Info_T) return GNAT.Sockets.Sock_Addr_Type;

    procedure Handle_Ready(
      Server_Socket_Info : in out Server_Socket_Info_T);

    procedure Destroy(
      Server_Socket_Info_Ptr : in out Server_Socket_Info_Ptr_T);

  private

    type Server_Socket_Info_T is
      new Ada.Finalization.Limited_Controlled with
      record
        Proxy_Ptr : Proxy_Ptr_T := null;
        Server_Socket : GNAT.Sockets.Socket_Type := GNAT.Sockets.No_Socket;
        File_Descriptor : Integer := Null_File_Descriptor;
        Listen_Address : GNAT.Sockets.Sock_Addr_Type := GNAT.Sockets.No_Sock_Addr;
      end record;

    overriding
    procedure Finalize(
      Server_Socket_Info : in out Server_Socket_Info_T);

  end Server;


  package Client is

    type Client_Socket_Info_T(
      Buffer_Size : Ada.Streams.Stream_Element_Offset) is
      new Ada.Finalization.Limited_Controlled with private;
    type Client_Socket_Info_Ptr_T is access Client_Socket_Info_T;

    function Create(
      Buffer_Size : in Ada.Streams.Stream_Element_Offset;
      Proxy_Ptr : in Proxy_Ptr_T;
      Client_Socket : in GNAT.Sockets.Socket_Type;
      Related_Client_Socket_Info_Ptr : in Client_Socket_Info_Ptr_T;
      Client_Address : in GNAT.Sockets.Sock_Addr_Type;
      Server_Address : in GNAT.Sockets.Sock_Addr_Type;
      Waiting_For_Read : in Boolean;
      Waiting_For_Connect : in Boolean) return Client_Socket_Info_Ptr_T;

    function Is_Waiting_For_Connect(
      Client_Socket_Info : in Client_Socket_Info_T) return Boolean;

    procedure Set_Related_Info_Ptr(
      Client_Socket_Info : in out Client_Socket_Info_T;
      Related_Client_Socket_Info_Ptr : in Client_Socket_Info_Ptr_T);

    function Get_File_Descriptor(
      Client_Socket_Info : in Client_Socket_Info_T) return Integer;

    function Get_Client_Address(
      Client_Socket_Info : in Client_Socket_Info_T) return GNAT.Sockets.Sock_Addr_Type;

    function Get_Server_Address(
      Client_Socket_Info : in Client_Socket_Info_T) return GNAT.Sockets.Sock_Addr_Type;

    function Get_Related_Client_Socket_Info(
      Client_Socket_Info : in Client_Socket_Info_T) return Client_Socket_Info_Ptr_T;

    procedure Handle_Error_Ready(
      Client_Socket_Info : in out Client_Socket_Info_T);

    procedure Handle_Read_Ready(
      Client_Socket_Info : in out Client_Socket_Info_T);

    procedure Handle_Related_Client_Info_Destroyed(
      Client_Socket_Info : in out Client_Socket_Info_T);

    procedure Handle_Write_Ready(
      Client_Socket_Info : in out Client_Socket_Info_T);

    procedure Destroy(
      Client_Socket_Info_Ptr : in out Client_Socket_Info_Ptr_T);

  private

    type Client_Socket_Info_T(
      Buffer_Size : Ada.Streams.Stream_Element_Offset) is
      new Ada.Finalization.Limited_Controlled with
      record
        Proxy_Ptr : Proxy_Ptr_T := null;
        Client_Socket : GNAT.Sockets.Socket_Type := GNAT.Sockets.No_Socket;
        File_Descriptor : Integer := Null_File_Descriptor;
        Related_Client_Socket_Info_Ptr : Client_Socket_Info_Ptr_T := null;
        Client_Address : GNAT.Sockets.Sock_Addr_Type := GNAT.Sockets.No_Sock_Addr;
        Server_Address : GNAT.Sockets.Sock_Addr_Type := GNAT.Sockets.No_Sock_Addr;
        Write_Buffer : Ada.Streams.Stream_Element_Array(1 .. Buffer_Size);
        First_Write_Buffer_Write_Index : Ada.Streams.Stream_Element_Offset := 1;
        Last_Write_Buffer_Write_Index : Ada.Streams.Stream_Element_Offset := 0;
        Waiting_For_Read : Boolean := False;
        Waiting_For_Connect : Boolean := False;
        Waiting_For_Write : Boolean := False;
        Destroy_When_Write_Finishes : Boolean := False;
      end record;

    procedure Update_Epoll(
      Client_Socket_Info : in out Client_Socket_Info_T);

    procedure Set_Waiting_For_Connect(
      Client_Socket_Info : in out Client_Socket_Info_T;
      Waiting_For_Connect : Boolean);

    procedure Set_Waiting_For_Read(
      Client_Socket_Info : in out Client_Socket_Info_T;
      Waiting_For_Read : Boolean);

    procedure Set_Waiting_For_Write(
      Client_Socket_Info : in out Client_Socket_Info_T;
      Waiting_For_Write : Boolean);

    procedure Write_Buffered_Data(
      Client_Socket_Info : in out Client_Socket_Info_T;
      Write_Completed : out Boolean);

    overriding
    procedure Finalize(
      Client_Socket_Info : in out Client_Socket_Info_T);

  end Client;

  use type Server.Server_Socket_Info_Ptr_T;
  use type Client.Client_Socket_Info_Ptr_T;

  package Server_Socket_Maps is
    new Ada.Containers.Hashed_Maps(
      Key_Type => Integer,
      Element_Type => Server.Server_Socket_Info_Ptr_T,
      Hash => Ada.Containers.Hash_Type'Mod,
      Equivalent_Keys => "=");

  package Client_Socket_Maps is
    new Ada.Containers.Hashed_Maps(
      Key_Type => Integer,
      Element_Type => Client.Client_Socket_Info_Ptr_T,
      Hash => Ada.Containers.Hash_Type'Mod,
      Equivalent_Keys => "=");

  package Client_Socket_Vectors is
    new Ada.Containers.Vectors(
      Index_Type => Positive,
      Element_Type => Client.Client_Socket_Info_Ptr_T);

  type Proxy_T is
    new Ada.Finalization.Limited_Controlled with
    record
      Proxy_Ptr : Proxy_Ptr_T := null;
      Buffer_Size : Ada.Streams.Stream_Element_Offset := 0;
      Epoll_Ptr : Epoll.Epoll_Ptr_T := null;
      Remote_Address : GNAT.Sockets.Sock_Addr_Type := GNAT.Sockets.No_Sock_Addr;
      Server_Socket_Info_Map : Server_Socket_Maps.Map :=
        Server_Socket_Maps.Empty_Map;
      Client_Socket_Info_Map : Client_Socket_Maps.Map :=
        Client_Socket_Maps.Empty_Map;
      Client_Socket_Info_To_Destroy : Client_Socket_Vectors.Vector :=
        Client_Socket_Vectors.Empty_Vector;
    end record;

  function Get_Epoll_Ptr(
    Proxy : in Proxy_T) return Epoll.Epoll_Ptr_T;

  procedure Create_Server_Sockets(
    Proxy : in out Proxy_T;
    Listen_Address_And_Ports : in Address_And_Port_Array);

  procedure Free_Pending_Client_Socket_Info(
    Proxy : in out Proxy_T);

  procedure Dispatch_Ready_FD(
    Proxy : in out Proxy_T;
    Ready_FD : in Integer;
    Read_Ready : Boolean;
    Write_Ready : Boolean;
    Error_Ready : Boolean);

  procedure Handle_Client_Socket_Accepted(
    Proxy : in out Proxy_T;
    Client_Socket : in GNAT.Sockets.Socket_Type;
    Client_Address : in GNAT.Sockets.Sock_Addr_Type);

  function Create_Remote_Socket(
    Proxy : in Proxy_T) return Client.Client_Socket_Info_Ptr_T;

  procedure Destroy_Client_Socket_Info(
    Proxy : in out Proxy_T;
    Client_Socket_Info : in Client.Client_Socket_Info_T);

  overriding
  procedure Finalize(
    Proxy : in out Proxy_T);

end Proxy;
