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
