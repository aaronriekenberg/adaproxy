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
with Ada.Finalization;
with Interfaces.C;

package Epoll is

  pragma Elaborate_Body;

  Epoll_Exception : exception;

  type Epoll_T is new Ada.Finalization.Limited_Controlled with private;
  type Epoll_Ptr_T is access Epoll_T;

  function Create
    return Epoll_Ptr_T;

  procedure Register_FD(
    Epoll : in out Epoll_T;
    FD : in Integer;
    Interested_In_Read_Events : Boolean := False;
    Interested_In_Write_Events : Boolean := False);

  procedure Unregister_FD(
    Epoll : in out Epoll_T;
    FD : in Integer);

  procedure Poll(
    Epoll : in out Epoll_T;
    Block : in Boolean := True;
    Handle_Ready_FD : not null access procedure(
      Ready_FD : Integer;
      Read_Ready : Boolean;
      Write_Ready : Boolean;
      Error_Ready : Boolean));

  function Epoll_FD(
    Epoll : in Epoll_T)
    return Integer;

  procedure Destroy(
    Epoll : in out Epoll_Ptr_T);

private

  use type Interfaces.C.Int;

  type FD_State is record
    Interested_In_Read_Events : Boolean := False;
    Interested_In_Write_Events : Boolean := False;
  end record;

  package FD_State_Maps is
    new Ada.Containers.Hashed_Maps(
      Key_Type => Interfaces.C.Int,
      Element_Type => FD_State,
      Hash => Ada.Containers.Hash_Type'Mod,
      Equivalent_Keys => "=");

  Null_Epoll_FD : constant Interfaces.C.Int := -1;

  type Epoll_T is new Ada.Finalization.Limited_Controlled with
    record
      Epoll_FD : Interfaces.C.Int := Null_Epoll_FD;
      FD_State_Map : FD_State_Maps.Map := FD_State_Maps.Empty_Map;
    end record;

  overriding
  procedure Finalize(
    Epoll : in out Epoll_T);

end Epoll;
