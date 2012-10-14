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

package body Socket_Util is

  procedure Set_Socket_Non_Blocking(
    Socket : in GNAT.Sockets.Socket_Type) is

    Non_Blocking_Request :
      GNAT.Sockets.Request_Type :=
        (Name => GNAT.Sockets.Non_Blocking_IO,
         Enabled => True);

  begin
    GNAT.Sockets.Control_Socket(
      Socket => Socket,
      Request => Non_Blocking_Request);
  end Set_Socket_Non_Blocking;


  function Get_Socket_Error(
    Socket : in GNAT.Sockets.Socket_Type)
    return GNAT.Sockets.Error_Type is

    Error_Option : GNAT.Sockets.Option_Type(GNAT.Sockets.Error);

  begin
    Error_Option :=
      GNAT.Sockets.Get_Socket_Option(
        Socket => Socket,
        Level => GNAT.Sockets.Socket_Level,
        Name => GNAT.Sockets.Error);
    return Error_Option.Error;
  end Get_Socket_Error;


  procedure Accept_Socket(
    Server_Socket : in GNAT.Sockets.Socket_Type;
    Result : out Accept_Result_T) is

    use type GNAT.Sockets.Error_Type;

    Success : Boolean := False;
    Error : Boolean := False;
    Would_Block : Boolean := False;
    Client_Socket : GNAT.Sockets.Socket_Type;
    Client_Address : GNAT.Sockets.Sock_Addr_Type;
    Socket_Error_Type : GNAT.Sockets.Error_Type := GNAT.Sockets.Success;

  begin

    while ((not Success) and
           (not Error) and
           (not Would_Block)) loop
      begin
        GNAT.Sockets.Accept_Socket(
          Server => Server_Socket,
          Socket => Client_Socket,
          Address => Client_Address);
        Success := True;
      exception
        when Socket_Error : GNAT.Sockets.Socket_Error =>
          Socket_Error_Type := GNAT.Sockets.Resolve_Exception(Socket_Error);
          case Socket_Error_Type is
            when GNAT.Sockets.Interrupted_System_Call =>
              null;
            when GNAT.Sockets.Resource_Temporarily_Unavailable =>
              Would_Block := True;
            when others =>
              Error := True;
          end case;
      end;
    end loop;

    if Would_Block then
      Result := (Status => Accept_Would_Block);
    elsif Error then
      Result := (Status => Accept_Error,
                 Error_Type => Socket_Error_Type);
    else
      Result := (Status => Accept_Success,
                 Client_Socket => Client_Socket,
                 Client_Address => Client_Address);
    end if;

  end Accept_Socket;


  procedure Connect(
    Socket : in GNAT.Sockets.Socket_Type;
    Remote_Address : in GNAT.Sockets.Sock_Addr_Type;
    Result : out Connect_Result_T) is

    use type GNAT.Sockets.Error_Type;

    Success : Boolean := False;
    In_Progress : Boolean := False;
    Error : Boolean := False;
    Socket_Error_Type : GNAT.Sockets.Error_Type := GNAT.Sockets.Success;

  begin

    while ((not Success) and
           (not In_Progress) and
           (not Error)) loop
      begin
        GNAT.Sockets.Connect_Socket(
          Socket => Socket,
          Server => Remote_Address);
        Success := True;
      exception
        when Socket_Error : GNAT.Sockets.Socket_Error =>
          Socket_Error_Type := GNAT.Sockets.Resolve_Exception(Socket_Error);
          case Socket_Error_Type is
            when GNAT.Sockets.Interrupted_System_Call |
                 GNAT.Sockets.Operation_Now_In_Progress =>
              In_Progress := True;
            when others =>
              Error := True;
          end case;
      end;
    end loop;

    if In_Progress then
      Result := (Status => Connect_In_Progress);
    elsif Error then
      Result := (Status => Connect_Error,
                 Error_Type => Socket_Error_Type);
    else
      Result := (Status => Connect_Success);
    end if;
  end Connect;


  procedure Read(
    Socket : in GNAT.Sockets.Socket_Type;
    Buffer : out Ada.Streams.Stream_Element_Array;
    Result : out Read_Result_T) is

    use type Ada.Streams.Stream_Element_Offset;
    use type GNAT.Sockets.Error_Type;

    Success : Boolean := False;
    Receive_Would_Block : Boolean := False;
    Receive_Error : Boolean := False;
    Receive_EOF : Boolean := False;
    Last_Read_Index : Ada.Streams.Stream_Element_Offset := 0;
    Socket_Error_Type : GNAT.Sockets.Error_Type := GNAT.Sockets.Success;

  begin

    while ((not Success) and
           (not Receive_Would_Block) and
           (not Receive_Error) and
           (not Receive_EOF)) loop
      begin
        GNAT.Sockets.Receive_Socket(
          Socket => Socket,
          Item => Buffer,
          Last => Last_Read_Index);
        if (Last_Read_Index = (Buffer'First - 1)) then
          Receive_EOF := True;
        else
          Success := True;
        end if;
      exception
        when Socket_Error : GNAT.Sockets.Socket_Error =>
          Socket_Error_Type := GNAT.Sockets.Resolve_Exception(Socket_Error);
          case Socket_Error_Type is
            when GNAT.Sockets.Interrupted_System_Call =>
              null;
            when GNAT.Sockets.Resource_Temporarily_Unavailable =>
              Receive_Would_Block := True;
            when others =>
              Receive_Error := True;
          end case;
      end;
    end loop;

    if Receive_Would_Block then
      Result := (Status => Read_Would_Block);
    elsif Receive_Error then
      Result := (Status => Read_Error,
                 Error_Type => Socket_Error_Type);
    elsif Receive_EOF then
      Result := (Status => Read_EOF);
    else
      Result := (Status => Read_Success,
                 Num_Read => (Last_Read_Index - Buffer'First + 1));
    end if;

  end Read;


  procedure Write(
    Socket : in GNAT.Sockets.Socket_Type;
    Buffer : in Ada.Streams.Stream_Element_Array;
    Result : out Write_Result_T) is

    use type Ada.Streams.Stream_Element_Offset;
    use type GNAT.Sockets.Error_Type;

    Success : Boolean := False;
    Send_Would_Block : Boolean := False;
    Send_Error : Boolean := False;
    Send_EOF : Boolean := False;
    Last_Send_Index : Ada.Streams.Stream_Element_Offset := 0;
    Socket_Error_Type : GNAT.Sockets.Error_Type := GNAT.Sockets.Success;

  begin

    while ((not Success) and
           (not Send_Would_Block) and
           (not Send_Error) and
           (not Send_EOF)) loop
      begin
        GNAT.Sockets.Send_Socket(
          Socket => Socket,
          Item => Buffer,
          Last => Last_Send_Index);
        if (Last_Send_Index = (Buffer'First - 1)) then
          Send_EOF := True;
        else
          Success := True;
        end if;
      exception
        when Socket_Error : GNAT.Sockets.Socket_Error =>
          Socket_Error_Type := GNAT.Sockets.Resolve_Exception(Socket_Error);
          case Socket_Error_Type is
            when GNAT.Sockets.Interrupted_System_Call =>
              null;
            when GNAT.Sockets.Resource_Temporarily_Unavailable =>
              Send_Would_Block := True;
            when others =>
              Send_Error := True;
          end case;
      end;
    end loop;

    if Send_Would_Block then
      Result := (Status => Write_Would_Block);
    elsif Send_Error then
      Result := (Status => Write_Error,
                 Error_Type => Socket_Error_Type);
    elsif Send_EOF then
      Result := (Status => Write_EOF);
    else
      Result := (Status => Write_Success, 
                 Num_Written => (Last_Send_Index - Buffer'First + 1));
    end if;

  end Write;

end Socket_Util;
