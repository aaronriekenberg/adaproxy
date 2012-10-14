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

with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;

package body Epoll is

  use type Interfaces.C.Int;
  use type Interfaces.C.Unsigned;


  -- Events
  EPOLLIN : constant Interfaces.C.Unsigned := 16#001#;
  EPOLLPRI : constant Interfaces.C.Unsigned := 16#002#;
  EPOLLOUT : constant Interfaces.C.Unsigned := 16#004#;
  EPOLLRDNORM : constant Interfaces.C.Unsigned := 16#040#;
  EPOLLRDBAND : constant Interfaces.C.Unsigned := 16#080#;
  EPOLLWRNORM : constant Interfaces.C.Unsigned := 16#100#;
  EPOLLWRBAND : constant Interfaces.C.Unsigned := 16#200#;
  EPOLLMSG : constant Interfaces.C.Unsigned := 16#400#;
  EPOLLERR : constant Interfaces.C.Unsigned := 16#008#;
  EPOLLHUP : constant Interfaces.C.Unsigned := 16#010#;
  EPOLLRDHUP : constant Interfaces.C.Unsigned := 16#2000#;
  EPOLLONESHOT : constant Interfaces.C.Unsigned := (2 ** 30);
  EPOLLET : constant Interfaces.C.Unsigned := (2 ** 31);


  type Epoll_Ctl_Opcode_T is (
    EPOLL_CTL_ADD,
    EPOLL_CTL_DEL,
    EPOLL_CTL_MOD);

  pragma Convention(C, Epoll_Ctl_Opcode_T);

  for Epoll_Ctl_Opcode_T use (
    EPOLL_CTL_ADD => 1,
    EPOLL_CTL_DEL => 2,
    EPOLL_CTL_MOD => 3);

  for Epoll_Ctl_Opcode_T'Size use Interfaces.C.Int'Size;
  for Epoll_Ctl_Opcode_T'Alignment use Interfaces.C.Int'Alignment;

  -- Timeouts
  Infinite_Timeout : constant Interfaces.C.Int := -1;
  Zero_Timeout : constant Interfaces.C.Int := 0;

  -- Interrupted errno
  EINTR : constant Integer := 4;


  -- epoll_data
  type Epoll_Data_T is
    record
      FD : Interfaces.C.Int;
      Pad : Interfaces.C.Int;
    end record;

  pragma Convention(C, Epoll_Data_T);

  for Epoll_Data_T'Size use (8 * 8);

  for Epoll_Data_T use
    record
      FD  at 0 range 0 .. 31;
      Pad at 4 range 0 .. 31;
    end record;

  Null_Epoll_Data : constant Epoll_Data_T :=
    (FD => 0,
     Pad => 0);


  type Epoll_Event_T is
    record
      Events : Interfaces.C.Unsigned;
      Data : Epoll_Data_T;
    end record;

  pragma Convention(C, Epoll_Event_T);

  Epoll_Event_T_Size : constant := (12 * 8);

  for Epoll_Event_T'Size use Epoll_Event_T_Size;

  for Epoll_Event_T use
    record
      Events at 0 range 0 .. 31;
      Data   at 4 range 0 .. 63;
    end record;

  Null_Epoll_Event : constant Epoll_Event_T :=
    (Events => 0,
     Data => Null_Epoll_Data);


  type Epoll_Event_Array_T is array (Positive range <>) of aliased Epoll_Event_T;

  pragma Convention(C, Epoll_Event_Array_T);

  for Epoll_Event_Array_T'Component_Size use Epoll_Event_T_Size;


  function Epoll_Create(
    Size : in Interfaces.C.Int := 1) 
    return Interfaces.C.Int;
  pragma Import(C, Epoll_Create, "epoll_create");


  function Epoll_Ctl(
    Epoll_FD : in Interfaces.C.Int;
    Operation : in Epoll_Ctl_Opcode_T;
    FD : in Interfaces.C.Int;
    Event : access Epoll_Event_T)
    return Interfaces.C.Int;
  pragma Import(C, Epoll_Ctl, "epoll_ctl");


  function Epoll_Wait(
    Epoll_FD : in Interfaces.C.Int;
    Events : access Epoll_Event_T;
    Max_Events : in Interfaces.C.Int;
    Timeout : in Interfaces.C.Int)
    return Interfaces.C.Int;
  pragma Import(C, Epoll_Wait, "epoll_wait");


  function Signal_Safe_Epoll_Wait(
    Epoll_FD : in Interfaces.C.Int;
    Events : access Epoll_Event_T;
    Max_Events : in Interfaces.C.Int;
    Timeout : in Interfaces.C.Int)
    return Interfaces.C.Int is

    Interrupted : Boolean;
    Ret_Val : Interfaces.C.Int;
    Next_Timeout : Interfaces.C.Int := Timeout;

  begin
    loop
      Ret_Val := Epoll_Wait(
        Epoll_FD => Epoll_FD,
        Events => Events,
        Max_Events => Max_Events,
        Timeout => Next_Timeout);
      Interrupted :=
        ((Ret_Val < 0) and then
         (GNAT.OS_Lib.Errno = EINTR));
      Next_Timeout := 0;
      exit when (Interrupted = False);
    end loop;

    return Ret_Val;
  end Signal_Safe_Epoll_Wait;


  function Create 
    return Epoll_Ptr_T is

    Epoll_FD : Interfaces.C.Int := Null_Epoll_FD;
    Epoll_Ptr : Epoll_Ptr_T := null;

  begin
    Epoll_FD := Epoll_Create;
    if (Epoll_FD < 0) then
      raise Epoll_Exception with 
        ("Epoll_Create failed errno" & Integer'Image(GNAT.OS_Lib.Errno));
    end if;

    Epoll_Ptr := 
      new Epoll_T'(
        Ada.Finalization.Limited_Controlled with
          Epoll_FD => Epoll_FD,
          FD_State_Map => FD_State_Maps.Empty_Map);
    return Epoll_Ptr;
  end Create;


  procedure Register_FD(
    Epoll : in out Epoll_T;
    FD : in Integer;
    Interested_In_Read_Events : Boolean := False;
    Interested_In_Write_Events : Boolean := False) is

    FD_Int : constant Interfaces.C.Int := Interfaces.C.Int(FD);

    function Create_Epoll_Event return Epoll_Event_T is
      Epoll_Event : Epoll_Event_T := Null_Epoll_Event;
    begin
      if Interested_In_Read_Events then
        Epoll_Event.Events := (Epoll_Event.Events or EPOLLIN);
      end if;
      if Interested_In_Write_Events then
        Epoll_Event.Events := (Epoll_Event.Events or EPOLLOUT);
      end if;
      Epoll_Event.Data.FD := FD_Int;
      return Epoll_Event;
    end Create_Epoll_Event;

    procedure Add_FD_To_Epoll is
      Epoll_Event : aliased Epoll_Event_T := Create_Epoll_Event;
      Ret_Val : Interfaces.C.Int := -1;
    begin
      Ret_Val :=
        Epoll_Ctl(
          Epoll_FD => Epoll.Epoll_FD,
          Operation => EPOLL_CTL_ADD,
          FD => FD_Int,
          Event => Epoll_Event'Access);
      if (Ret_Val /= 0) then
        raise Epoll_Exception with
          ("Epoll_Ctl(EPOLL_CTL_ADD) failed errno" & Integer'Image(GNAT.OS_Lib.Errno));
      end if;
    end Add_FD_To_Epoll;

    procedure Update_FD_In_Epoll is
      Epoll_Event : aliased Epoll_Event_T := Create_Epoll_Event;
      Ret_Val : Interfaces.C.Int := -1;
    begin
      Ret_Val :=
        Epoll_Ctl(
          Epoll_FD => Epoll.Epoll_FD,
          Operation => EPOLL_CTL_MOD,
          FD => FD_Int,
          Event => Epoll_Event'Access);
      if (Ret_Val /= 0) then
        raise Epoll_Exception with
          ("Epoll_Ctl(EPOLL_CTL_MOD) failed errno" & Integer'Image(GNAT.OS_Lib.Errno));
      end if;
    end Update_FD_In_Epoll;

    Map_Cursor : FD_State_Maps.Cursor;
    New_FD_State : constant FD_State := (
      Interested_In_Read_Events => Interested_In_Read_Events,
      Interested_In_Write_Events => Interested_In_Write_Events);

  begin

    Map_Cursor := Epoll.FD_State_Map.Find(FD_Int);
    if (FD_State_Maps.Has_Element(Map_Cursor)) then
      -- FD is in the map

      if (New_FD_State /= FD_State_Maps.Element(Map_Cursor)) then
        -- New_FD_State is different from current FD_State
        Update_FD_In_Epoll;
        Epoll.FD_State_Map.Replace_Element(
          Position => Map_Cursor,
          New_Item => New_FD_State);
      end if;

    else
      -- FD is not in the map
      Add_FD_To_Epoll;
      Epoll.FD_State_Map.Insert(
        Key => FD_Int,
        New_Item => New_FD_State);
    end if;

  end Register_FD;


  procedure Unregister_FD(
    Epoll : in out Epoll_T;
    FD : in Integer) is

    FD_Int : constant Interfaces.C.Int := Interfaces.C.Int(FD);
    Map_Cursor : FD_State_Maps.Cursor;
    Ret_Val : Interfaces.C.Int := -1;

  begin

    Map_Cursor := Epoll.FD_State_Map.Find(FD_Int);
    if (FD_State_Maps.Has_Element(Map_Cursor)) then

      Ret_Val := Epoll_Ctl(
        Epoll_FD => Epoll.Epoll_FD,
        Operation => EPOLL_CTL_DEL,
        FD => FD_Int,
        Event => null);

      if (Ret_Val /= 0) then
        raise Epoll_Exception with
          ("Epoll_Ctl(EPOLL_CTL_DEL) failed errno" &
           Integer'Image(GNAT.OS_Lib.Errno));
      end if;

      Epoll.FD_State_Map.Delete(Map_Cursor);

    end if;

  end Unregister_FD;


  procedure Poll(
    Epoll : in out Epoll_T;
    Block : Boolean := True;
    Handle_Ready_FD : not null access procedure(
      Ready_FD : Integer;
      Read_Ready : Boolean;
      Write_Ready : Boolean;
      Error_Ready : Boolean)) is

    Epoll_Event_Array : Epoll_Event_Array_T(1 .. 128);
    Ret_Val : Interfaces.C.Int := -1;
    Num_Ready_FDs : Positive;
    Read_Ready : Boolean := False;
    Write_Ready : Boolean := False;
    Error_Ready : Boolean := False;

    function Get_Timeout
      return Interfaces.C.Int is
    begin
      if Block then
        return Infinite_Timeout;
      else
        return Zero_Timeout;
      end if;
    end Get_Timeout;

  begin
    Ret_Val :=
      Signal_Safe_Epoll_Wait(
        Epoll_FD => Epoll.Epoll_FD,
        Events => Epoll_Event_Array(Epoll_Event_Array'First)'Access,
        Max_Events => Interfaces.C.Int(Epoll_Event_Array'Last),
        Timeout => Get_Timeout);
    if (Ret_Val < 0) then

      raise Epoll_Exception with
        ("Epoll_Wait failed errno" &
         Integer'Image(GNAT.OS_Lib.Errno));

    elsif (Ret_Val > 0) then

      Num_Ready_FDs := Positive(Ret_Val);
      for I in Positive range Epoll_Event_Array'First .. Num_Ready_FDs loop
        Read_Ready := ((Epoll_Event_Array(I).Events and EPOLLIN) /= 0);
        Write_Ready := ((Epoll_Event_Array(I).Events and EPOLLOUT) /= 0);
        Error_Ready := (((Epoll_Event_Array(I).Events and EPOLLERR) /= 0) or
                        ((Epoll_Event_Array(I).Events and EPOLLHUP) /= 0));
        Handle_Ready_FD(
          Ready_Fd => Integer(Epoll_Event_Array(I).Data.FD),
          Read_Ready => Read_Ready,
          Write_Ready => Write_Ready,
          Error_Ready => Error_Ready);
      end loop;

    end if;

  end Poll;


  function Epoll_FD(
    Epoll : in Epoll_T)
    return Integer is
  begin
    return Integer(Epoll.Epoll_FD);
  end Epoll_FD;


  procedure Destroy(
    Epoll : in out Epoll_Ptr_T) is

    procedure Free is new Ada.Unchecked_Deallocation
      (Epoll_T, Epoll_Ptr_T);

  begin
    Free(Epoll);
  end Destroy;


  procedure Finalize(
    Epoll : in out Epoll_T) is
  begin
    if (Epoll.Epoll_FD /= Null_Epoll_FD) then
      GNAT.OS_Lib.Close(GNAT.OS_Lib.File_Descriptor(Epoll.Epoll_FD));
      Epoll.Epoll_FD := Null_Epoll_FD;
    end if;
  end Finalize;


end Epoll;
