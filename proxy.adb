with Ada.Unchecked_Deallocation;
with Epoll;
with Log;
with Socket_Util;

package body Proxy is

  Max_Operations_For_FD : constant := 100;


  function Create(
    Listen_Address_And_Ports : in Address_And_Port_Array;
    Remote_Address_And_Port : in Address_And_Port;
    Buffer_Size : in Ada.Streams.Stream_Element_Offset := Default_Buffer_Size)
    return Proxy_Ptr_T is

    Proxy_Ptr : Proxy_Ptr_T := null;

  begin
    GNAT.Sockets.Initialize;

    Proxy_Ptr := new Proxy_T;
    Proxy_Ptr.Proxy_Ptr := Proxy_Ptr;
    Proxy_Ptr.Buffer_Size := Buffer_Size;
    Proxy_Ptr.Epoll_Ptr := Epoll.Create;
    Proxy_Ptr.Remote_Address.Addr :=
      GNAT.Sockets.Addresses(
        GNAT.Sockets.Get_Host_By_Name(
          Ada.Strings.Unbounded.To_String(
            Remote_Address_And_Port.Address)));
    Proxy_Ptr.Remote_Address.Port := GNAT.Sockets.Port_Type(
      Remote_Address_And_Port.Port);
    Log.Log("remote address " &
            GNAT.Sockets.Image(Proxy_Ptr.Remote_Address));
    Log.Log("epoll fd" & Integer'Image(Proxy_Ptr.Epoll_Ptr.Epoll_FD));

    Proxy_Ptr.Create_Server_Sockets(Listen_Address_And_Ports);

    return Proxy_Ptr;
  end Create;


  procedure Run(
    Proxy : in out Proxy_T) is
  begin
    loop
      Proxy.Poll(Block => True);
    end loop;
  end Run;


  procedure Poll(
    Proxy : in out Proxy_T;
    Block : in Boolean := False) is

    procedure Handle_Ready_FD(
      Ready_FD : Integer;
      Read_Ready : Boolean;
      Write_Ready : Boolean;
      Error_Ready : Boolean) is
    begin
      Proxy.Dispatch_Ready_FD(
        Ready_FD => Ready_FD,
        Read_Ready => Read_Ready,
        Write_Ready => Write_Ready,
        Error_Ready => Error_Ready);
    end Handle_Ready_FD;

  begin
    Proxy.Epoll_Ptr.Poll(
      Block => Block,
      Handle_Ready_FD => Handle_Ready_FD'Access);

    Proxy.Free_Pending_Client_Socket_Info;
  end Poll;


  procedure Destroy(
    Proxy : in out Proxy_Ptr_T) is

    procedure Free is new Ada.Unchecked_Deallocation
      (Proxy_T, Proxy_Ptr_T);

  begin
    Free(Proxy);
  end Destroy;


  function Get_Epoll_Ptr(
    Proxy : in Proxy_T) return Epoll.Epoll_Ptr_T is
  begin
    return Proxy.Epoll_Ptr;
  end Get_Epoll_Ptr;


  procedure Create_Server_Sockets(
    Proxy : in out Proxy_T;
    Listen_Address_And_Ports : in Address_And_Port_Array) is

    Server_Socket_Info_Ptr : Server.Server_Socket_Info_Ptr_T := null;

  begin
    for I in Listen_Address_And_Ports'Range loop
      Server_Socket_Info_Ptr := Server.Create(
        Proxy_Ptr => Proxy.Proxy_Ptr,
        Listen_Address_And_Port => Listen_Address_And_Ports(I));

      Proxy.Server_Socket_Info_Map.Insert(
        Key => Server_Socket_Info_Ptr.Get_File_Descriptor,
        New_Item => Server_Socket_Info_Ptr);

      Log.Log("listen on " &
              GNAT.Sockets.Image(Server_Socket_Info_Ptr.Get_Listen_Address) &
              " (fd =" & Integer'Image(Server_Socket_Info_Ptr.Get_File_Descriptor) & ")");

    end loop;

  end Create_Server_Sockets;


  procedure Free_Pending_Client_Socket_Info(
    Proxy : in out Proxy_T) is

    procedure Process_Cursor(
      Position : in Client_Socket_Vectors.Cursor) is
    begin
      Proxy.Client_Socket_Info_To_Destroy.Update_Element(
        Position => Position,
        Process => Client.Destroy'Access);
    end Process_Cursor;

  begin

    Proxy.Client_Socket_Info_To_Destroy.Iterate(
      Process => Process_Cursor'Access);

    Proxy.Client_Socket_Info_To_Destroy.Clear;

  end Free_Pending_Client_Socket_Info;


  procedure Dispatch_Ready_FD(
    Proxy : in out Proxy_T;
    Ready_FD : in Integer;
    Read_Ready : Boolean;
    Write_Ready : Boolean;
    Error_Ready : Boolean) is

    Server_Socket_Cursor : Server_Socket_Maps.Cursor;
    Server_Socket_Info_Ptr : Server.Server_Socket_Info_Ptr_T := null;
    Client_Socket_Cursor : Client_Socket_Maps.Cursor;
    Client_Socket_Info_Ptr : Client.Client_Socket_Info_Ptr_T := null;

  begin

    --Log.Log("Dispatch_Ready_FD Ready_FD =" &
    --        Integer'Image(Ready_FD) &
    --        " Read_Ready " &
    --        Boolean'Image(Read_Ready) &
    --        " Write_Ready " &
    --        Boolean'Image(Write_Ready) &
    --        " Error_Ready " &
    --        Boolean'Image(Error_Ready));

    Server_Socket_Cursor := Proxy.Server_Socket_Info_Map.Find(Ready_FD);
    if (Server_Socket_Maps.Has_Element(Server_Socket_Cursor)) then
      Server_Socket_Info_Ptr := Server_Socket_Maps.Element(Server_Socket_Cursor);
      Server_Socket_Info_Ptr.Handle_Ready;
    end if;

    if Error_Ready then
      Client_Socket_Cursor := Proxy.Client_Socket_Info_Map.Find(Ready_FD);
      if (Client_Socket_Maps.Has_Element(Client_Socket_Cursor)) then
        Client_Socket_Info_Ptr := Client_Socket_Maps.Element(Client_Socket_Cursor);
        Client_Socket_Info_Ptr.Handle_Error_Ready;
      end if;
    end if;

    if Read_Ready then
      Client_Socket_Cursor := Proxy.Client_Socket_Info_Map.Find(Ready_FD);
      if (Client_Socket_Maps.Has_Element(Client_Socket_Cursor)) then
        Client_Socket_Info_Ptr := Client_Socket_Maps.Element(Client_Socket_Cursor);
        Client_Socket_Info_Ptr.Handle_Read_Ready;
      end if;
    end if;

    if Write_Ready then
      Client_Socket_Cursor := Proxy.Client_Socket_Info_Map.Find(Ready_FD);
      if (Client_Socket_Maps.Has_Element(Client_Socket_Cursor)) then
        Client_Socket_Info_Ptr := Client_Socket_Maps.Element(Client_Socket_Cursor);
        Client_Socket_Info_Ptr.Handle_Write_Ready;
      end if;
    end if;

  end Dispatch_Ready_FD;


  procedure Handle_Client_Socket_Accepted(
    Proxy : in out Proxy_T;
    Client_Socket : in GNAT.Sockets.Socket_Type;
    Client_Address : in GNAT.Sockets.Sock_Addr_Type) is

    Client_Socket_Info_Ptr : Client.Client_Socket_Info_Ptr_T := null;
    Remote_Socket_Info_Ptr : Client.Client_Socket_Info_Ptr_T := null;

  begin
    Socket_Util.Set_Socket_Non_Blocking(Socket => Client_Socket);

    Log.Log(
      "accepted connection " &
      GNAT.Sockets.Image(GNAT.Sockets.Get_Peer_Name(Client_Socket)) &
      " -> " &
      GNAT.Sockets.Image(GNAT.Sockets.Get_Socket_Name(Client_Socket)) &
      " fd" & Integer'Image(GNAT.Sockets.To_C(Client_Socket)));

    Remote_Socket_Info_Ptr := Proxy.Create_Remote_Socket;

    if (Remote_Socket_Info_Ptr = null) then

      GNAT.Sockets.Close_Socket(Socket => Client_Socket);

    else

      Client_Socket_Info_Ptr := Client.Create(
        Buffer_Size => Proxy.Buffer_Size,
        Proxy_Ptr => Proxy.Proxy_Ptr,
        Client_Socket => Client_Socket,
        Related_Client_Socket_Info_Ptr => Remote_Socket_Info_Ptr,
        Client_Address => GNAT.Sockets.Get_Peer_Name(Client_Socket),
        Server_Address => GNAT.Sockets.Get_Socket_Name(Client_Socket),
        Waiting_For_Read => (not Remote_Socket_Info_Ptr.Is_Waiting_For_Connect),
        Waiting_For_Connect => False);

      Remote_Socket_Info_Ptr.Set_Related_Info_Ptr(Client_Socket_Info_Ptr);

      Proxy.Client_Socket_Info_Map.Insert(
        Key => Client_Socket_Info_Ptr.Get_File_Descriptor,
        New_Item => Client_Socket_Info_Ptr);
      Proxy.Client_Socket_Info_Map.Insert(
        Key => Remote_Socket_Info_Ptr.Get_File_Descriptor,
        New_Item => Remote_Socket_Info_Ptr);


    end if;

  end Handle_Client_Socket_Accepted;


  function Create_Remote_Socket(
    Proxy : in Proxy_T) return Client.Client_Socket_Info_Ptr_T is

    use type GNAT.Sockets.Error_Type;
    use type Socket_Util.Connect_Status_T;

    Client_Socket_Info_Ptr : Client.Client_Socket_Info_Ptr_T := null;
    Remote_Socket : GNAT.Sockets.Socket_Type;
    Connect_Result : Socket_Util.Connect_Result_T;

  begin
    GNAT.Sockets.Create_Socket(Socket => Remote_Socket);

    Socket_Util.Set_Socket_Non_Blocking(Socket => Remote_Socket);

    Socket_Util.Connect(
      Socket => Remote_Socket,
      Remote_Address => Proxy.Remote_Address,
      Result => Connect_Result);

    case Connect_Result.Status is
      when Socket_Util.Connect_Error =>

        Log.Log("connect error " &
                GNAT.Sockets.Error_Type'Image(Connect_Result.Error_Type));
        GNAT.Sockets.Close_Socket(Socket => Remote_Socket);

      when Socket_Util.Connect_In_Progress | Socket_Util.Connect_Success =>

        Client_Socket_Info_Ptr := Client.Create(
          Buffer_Size => Proxy.Buffer_Size,
          Proxy_Ptr => Proxy.Proxy_Ptr,
          Client_Socket => Remote_Socket,
          Related_Client_Socket_Info_Ptr => null,
          Client_Address => GNAT.Sockets.Get_Socket_Name(Remote_Socket),
          Server_Address => Proxy.Remote_Address,
          Waiting_For_Read =>
            (Connect_Result.Status = Socket_Util.Connect_Success),
          Waiting_For_Connect =>
            (Connect_Result.Status = Socket_Util.Connect_In_Progress));

        if (Connect_Result.Status = Socket_Util.Connect_In_Progress) then

          Log.Log(
            "remote connection in progress " &
            GNAT.Sockets.Image(Client_Socket_Info_Ptr.Get_Client_Address) &
            " -> " &
            GNAT.Sockets.Image(Client_Socket_Info_Ptr.Get_Server_Address) &
            " fd" & Integer'Image(Client_Socket_Info_Ptr.Get_File_Descriptor));

        else

          Log.Log(
            "remote connection complete " &
            GNAT.Sockets.Image(Client_Socket_Info_Ptr.Get_Client_Address) &
            " -> " &
            GNAT.Sockets.Image(Client_Socket_Info_Ptr.Get_Server_Address) &
            " fd" & Integer'Image(Client_Socket_Info_Ptr.Get_File_Descriptor));

        end if;

    end case;

    return Client_Socket_Info_Ptr;
  end Create_Remote_Socket;


  procedure Destroy_Client_Socket_Info(
    Proxy : in out Proxy_T;
    Client_Socket_Info : in Client.Client_Socket_Info_T) is

    Related_Client_Socket_Info_Ptr : Client.Client_Socket_Info_Ptr_T :=
      Client_Socket_Info.Get_Related_Client_Socket_Info;
    Client_Socket_Cursor : Client_Socket_Maps.Cursor;

  begin
    Log.Log(
      "disconnect " &
      GNAT.Sockets.Image(Client_Socket_Info.Get_Client_Address) &
      " -> " &
      GNAT.Sockets.Image(Client_Socket_Info.Get_Server_Address) &
      " fd" & Integer'Image(Client_Socket_Info.Get_File_Descriptor));

    Client_Socket_Cursor := Proxy.Client_Socket_Info_Map.Find(
      Client_Socket_Info.Get_File_Descriptor);
    if (Client_Socket_Maps.Has_Element(Client_Socket_Cursor)) then
      Proxy.Client_Socket_Info_To_Destroy.Append(
        Client_Socket_Maps.Element(Client_Socket_Cursor));
      Proxy.Client_Socket_Info_Map.Delete(Client_Socket_Cursor);
    end if;

    if (Related_Client_Socket_Info_Ptr /= null) then
      Related_Client_Socket_Info_Ptr.Handle_Related_Client_Info_Destroyed;
    end if;
  end Destroy_Client_Socket_Info;


  procedure Finalize(
    Proxy : in out Proxy_T) is

    procedure Add_Client_Info_To_Destroy_Vector(
      Position : in Client_Socket_Maps.Cursor) is
    begin
      Proxy.Client_Socket_Info_To_Destroy.Append(
        Client_Socket_Maps.Element(Position));
    end Add_Client_Info_To_Destroy_Vector;

    procedure Free_Server_Socket_Info(
      Key : in Integer;
      Server_Socket_Info : in out Server.Server_Socket_Info_Ptr_T) is
    begin
      Server.Destroy(Server_Socket_Info);
   end Free_Server_Socket_Info;

    procedure Process_Server_Info_Cursor(
      Position : in Server_Socket_Maps.Cursor) is
    begin
      Proxy.Server_Socket_Info_Map.Update_Element(
        Position => Position,
        Process => Free_Server_Socket_Info'Access);
    end Process_Server_Info_Cursor;

  begin
    Proxy.Client_Socket_Info_Map.Iterate(
      Add_Client_Info_To_Destroy_Vector'Access);
    Proxy.Client_Socket_Info_Map.Clear;
    Proxy.Free_Pending_Client_Socket_Info;

    Proxy.Server_Socket_Info_Map.Iterate(
      Process_Server_Info_Cursor'Access);
    Proxy.Server_Socket_Info_Map.Clear;

    Epoll.Destroy(Proxy.Epoll_Ptr);
  end Finalize;


  package body Server is

    function Create(
      Proxy_Ptr : in Proxy_Ptr_T;
      Listen_Address_And_Port : in Address_And_Port) 
      return Server_Socket_Info_Ptr_T is

      Server_Socket : GNAT.Sockets.Socket_Type;
      File_Descriptor : Integer := Null_File_Descriptor;
      Server_Socket_Info_Ptr : Server_Socket_Info_Ptr_T := null;
      Listen_Address : GNAT.Sockets.Sock_Addr_Type;
      Listen_Address_String : constant String := 
        Ada.Strings.Unbounded.To_String(Listen_Address_And_Port.Address);

    begin
      GNAT.Sockets.Create_Socket(Server_Socket);

      File_Descriptor := GNAT.Sockets.To_C(Server_Socket);

      GNAT.Sockets.Set_Socket_Option(
        Socket => Server_Socket,
        Level => GNAT.Sockets.Socket_Level,
        Option => (Name => GNAT.Sockets.Reuse_Address,
                   Enabled => True));

      if ((Listen_Address_String = "0") or
          (Listen_Address_String = "0.0.0.0")) then
        Listen_Address.Addr := GNAT.Sockets.Any_Inet_Addr;
      else
        Listen_Address.Addr :=
          GNAT.Sockets.Addresses(
            GNAT.Sockets.Get_Host_By_Name(
              Listen_Address_String));
      end if;
      Listen_Address.Port := GNAT.Sockets.Port_Type(
        Listen_Address_And_Port.Port);

      GNAT.Sockets.Bind_Socket(
        Socket => Server_Socket,
        Address => Listen_Address);

      GNAT.Sockets.Listen_Socket(
        Socket => Server_Socket);

      Socket_Util.Set_Socket_Non_Blocking(Socket => Server_Socket);

      Server_Socket_Info_Ptr :=
        new Server_Socket_Info_T'(
          Ada.Finalization.Limited_Controlled with
            Proxy_Ptr => Proxy_Ptr,
            Server_Socket => Server_Socket,
            File_Descriptor => File_Descriptor,
            Listen_Address => Listen_Address);

      Proxy_Ptr.Get_Epoll_Ptr.Register_FD(
        FD => File_Descriptor,
        Interested_In_Read_Events => True);

      return Server_Socket_Info_Ptr;
    end Create;


    function Get_File_Descriptor(
      Server_Socket_Info : in Server_Socket_Info_T) 
      return Integer is
    begin
      return Server_Socket_Info.File_Descriptor;
    end Get_File_Descriptor;


    function Get_Listen_Address(
      Server_Socket_Info : in Server_Socket_Info_T) 
      return GNAT.Sockets.Sock_Addr_Type is
    begin
      return Server_Socket_Info.Listen_Address;
    end Get_Listen_Address;


    procedure Handle_Ready(
      Server_Socket_Info : in out Server_Socket_Info_T) is

      Accept_Failed : Boolean := False;
      Result : Socket_Util.Accept_Result_T;
      Num_Accepts : Natural := 0;

    begin
      while ((not Accept_Failed) and
             (Num_Accepts < Max_Operations_For_FD)) loop
        Socket_Util.Accept_Socket(
          Server_Socket => Server_Socket_Info.Server_Socket,
          Result => Result);
        Num_Accepts := Num_Accepts + 1;

        case Result.Status is

          when Socket_Util.Accept_Would_Block =>
            Accept_Failed := True;

          when Socket_Util.Accept_Error =>
            Accept_Failed := True;
            Log.Log(
              "accept error " &
              GNAT.Sockets.Error_Type'Image(Result.Error_Type));

          when Socket_Util.Accept_Success =>
            Server_Socket_Info.Proxy_Ptr.Handle_Client_Socket_Accepted(
              Client_Socket => Result.Client_Socket, 
              Client_Address => Result.Client_Address);

        end case;
      end loop;

    end Handle_Ready;


    procedure Destroy(
      Server_Socket_Info_Ptr : in out Server_Socket_Info_Ptr_T) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Server_Socket_Info_T, Server_Socket_Info_Ptr_T);

    begin
      Free(Server_Socket_Info_Ptr);
    end Destroy;


    procedure Finalize(
      Server_Socket_Info : in out Server_Socket_Info_T) is

      use type GNAT.Sockets.Socket_Type;

    begin
      if (Server_Socket_Info.File_Descriptor /= Null_File_Descriptor) then

        Server_Socket_Info.Proxy_Ptr.Get_Epoll_Ptr.Unregister_FD(
          FD => Server_Socket_Info.File_Descriptor);
        Server_Socket_Info.File_Descriptor := Null_File_Descriptor;

      end if;

      if (Server_Socket_Info.Server_Socket /= GNAT.Sockets.No_Socket) then

        GNAT.Sockets.Close_Socket(Socket => Server_Socket_Info.Server_Socket);
        Server_Socket_Info.Server_Socket := GNAT.Sockets.No_Socket;

      end if;
    end Finalize;

  end Server;


  package body Client is

    function Create(
      Buffer_Size : in Ada.Streams.Stream_Element_Offset;
      Proxy_Ptr : in Proxy_Ptr_T;
      Client_Socket : in GNAT.Sockets.Socket_Type;
      Related_Client_Socket_Info_Ptr : in Client_Socket_Info_Ptr_T;
      Client_Address : in GNAT.Sockets.Sock_Addr_Type;
      Server_Address : in GNAT.Sockets.Sock_Addr_Type;
      Waiting_For_Read : in Boolean;
      Waiting_For_Connect : in Boolean) 
      return Client_Socket_Info_Ptr_T is

      Client_Socket_Info_Ptr : Client_Socket_Info_Ptr_T := null;

    begin

      Client_Socket_Info_Ptr := new Client_Socket_Info_T(
        Buffer_Size => Buffer_Size);
      Client_Socket_Info_Ptr.Proxy_Ptr := Proxy_Ptr;
      Client_Socket_Info_Ptr.Client_Socket := Client_Socket;
      Client_Socket_Info_Ptr.File_Descriptor := GNAT.Sockets.To_C(Client_Socket);
      Client_Socket_Info_Ptr.Related_Client_Socket_Info_Ptr := Related_Client_Socket_Info_Ptr;
      Client_Socket_Info_Ptr.Client_Address := Client_Address;
      Client_Socket_Info_Ptr.Server_Address := Server_Address;
      Client_Socket_Info_Ptr.Set_Waiting_For_Read(Waiting_For_Read);
      Client_Socket_Info_Ptr.Set_Waiting_For_Connect(Waiting_For_Connect);

      return Client_Socket_Info_Ptr;

    end Create;


    function Is_Waiting_For_Connect(
      Client_Socket_Info : in Client_Socket_Info_T)
      return Boolean is
    begin
      return Client_Socket_Info.Waiting_For_Connect;
    end Is_Waiting_For_Connect;


    function Is_Waiting_For_Write(
      Client_Socket_Info : in Client_Socket_Info_T)
      return Boolean is
    begin
      return Client_Socket_Info.Waiting_For_Write;
    end Is_Waiting_For_Write;


    procedure Set_Related_Info_Ptr(
      Client_Socket_Info : in out Client_Socket_Info_T;
      Related_Client_Socket_Info_Ptr : in Client_Socket_Info_Ptr_T) is
    begin
      Client_Socket_Info.Related_Client_Socket_Info_Ptr := Related_Client_Socket_Info_Ptr;
    end Set_Related_Info_Ptr;


    function Get_File_Descriptor(
      Client_Socket_Info : in Client_Socket_Info_T)
      return Integer is
    begin
      return Client_Socket_Info.File_Descriptor;
    end Get_File_Descriptor;


    function Get_Client_Address(
      Client_Socket_Info : in Client_Socket_Info_T)
      return GNAT.Sockets.Sock_Addr_Type is
    begin
      return Client_Socket_Info.Client_Address;
    end Get_Client_Address;


    function Get_Server_Address(
      Client_Socket_Info : in Client_Socket_Info_T)
      return GNAT.Sockets.Sock_Addr_Type is
    begin
      return Client_Socket_Info.Server_Address;
    end Get_Server_Address;


    function Get_Related_Client_Socket_Info(
      Client_Socket_Info : in Client_Socket_Info_T)
      return Client_Socket_Info_Ptr_T is
    begin
      return Client_Socket_Info.Related_Client_Socket_Info_Ptr;
    end Get_Related_Client_Socket_Info;


    procedure Handle_Error_Ready(
      Client_Socket_Info : in out Client_Socket_Info_T) is

      use type GNAT.Sockets.Error_Type;

      Socket_Error_Type : GNAT.Sockets.Error_Type := GNAT.Sockets.Success;

    begin
      Socket_Error_Type := Socket_Util.Get_Socket_Error(Client_Socket_Info.Client_Socket);
      if (Socket_Error_Type /= GNAT.Sockets.Success) then
        Log.Log(
          "socket error " & 
          GNAT.Sockets.Error_Type'Image(Socket_Error_Type) &
          " fd" & Integer'Image(Client_Socket_Info.File_Descriptor));
        Client_Socket_Info.Proxy_Ptr.Destroy_Client_Socket_Info(Client_Socket_Info);
      end if;
    end Handle_Error_Ready;


    procedure Handle_Read_Ready(
      Client_Socket_Info : in out Client_Socket_Info_T) is

      use type Ada.Streams.Stream_Element_Offset;
      use type GNAT.Sockets.Error_Type;

      Last_Read_Successful : Boolean := True;
      Last_Write_Completed : Boolean := True;
      Num_Reads : Natural := 0;
      Related_Client_Socket_Info_Ptr : Client_Socket_Info_Ptr_T := null;
      Read_Result : Socket_Util.Read_Result_T;

    begin
      while (Client_Socket_Info.Waiting_For_Read and
             Last_Read_Successful and
             Last_Write_Completed and
             (Num_Reads < Max_Operations_For_FD)) loop

        Related_Client_Socket_Info_Ptr := 
          Client_Socket_Info.Related_Client_Socket_Info_Ptr;

        Socket_Util.Read(
          Socket => Client_Socket_Info.Client_Socket,
          Buffer => Related_Client_Socket_Info_Ptr.Write_Buffer,
          Result => Read_Result);
        Num_Reads := Num_Reads + 1;

        case Read_Result.Status is

          when Socket_Util.Read_Would_Block =>
            Last_Read_Successful := False;

          when Socket_Util.Read_Error | Socket_Util.Read_EOF =>
            Client_Socket_Info.Proxy_Ptr.Destroy_Client_Socket_Info(Client_Socket_Info);
            Last_Read_Successful := False;

          when Socket_Util.Read_Success =>
            Related_Client_Socket_Info_Ptr.First_Write_Buffer_Write_Index := 1;
            Related_Client_Socket_Info_Ptr.Last_Write_Buffer_Write_Index :=
              Read_Result.Num_Read;
            Last_Read_Successful := True;

            Write_Buffered_Data(
              Client_Socket_Info => Related_Client_Socket_Info_Ptr.all,
              Write_Completed => Last_Write_Completed);

        end case;
      end loop;
    end Handle_Read_Ready;


    procedure Handle_Related_Client_Info_Destroyed(
      Client_Socket_Info : in out Client_Socket_Info_T) is
    begin

      Client_Socket_Info.Related_Client_Socket_Info_Ptr := null;

      if (Client_Socket_Info.Waiting_For_Write) then
        Client_Socket_Info.Destroy_When_Write_Finishes := True;
        Client_Socket_Info.Set_Waiting_For_Read(False);
      else
        Client_Socket_Info.Proxy_Ptr.Destroy_Client_Socket_Info(Client_Socket_Info);
      end if;

    end Handle_Related_Client_Info_Destroyed;


    procedure Handle_Write_Ready(
      Client_Socket_Info : in out Client_Socket_Info_T) is

      use type GNAT.Sockets.Error_Type;

      Socket_Error : GNAT.Sockets.Error_Type := GNAT.Sockets.Success;
      Related_Client_Socket_Info_Ptr : Client_Socket_Info_Ptr_T := null;
      Write_Completed : Boolean := True;

    begin
      if (Client_Socket_Info.Waiting_For_Connect) then

        Related_Client_Socket_Info_Ptr := 
          Client_Socket_Info.Related_Client_Socket_Info_Ptr;

        Socket_Error := Socket_Util.Get_Socket_Error(Client_Socket_Info.Client_Socket);
        if (Socket_Error = GNAT.Sockets.Success) then

          Log.Log(
            "remote connection complete " &
            GNAT.Sockets.Image(Client_Socket_Info.Client_Address) &
            " -> " &
            GNAT.Sockets.Image(Client_Socket_Info.Server_Address) &
            " fd" & Integer'Image(Client_Socket_Info.File_Descriptor));
          Client_Socket_Info.Set_Waiting_For_Connect(False);
          Client_Socket_Info.Set_Waiting_For_Read(True);
          Related_Client_Socket_Info_Ptr.Set_Waiting_For_Read(True);

        elsif (Socket_Error /= GNAT.Sockets.Operation_Now_In_Progress) then

          Log.Log(
            "connect error " & 
            GNAT.Sockets.Error_Type'Image(Socket_Error));
          Client_Socket_Info.Proxy_Ptr.Destroy_Client_Socket_Info(Client_Socket_Info);

        end if;

      elsif (Client_Socket_Info.Waiting_For_Write) then

        Client_Socket_Info.Write_Buffered_Data(
          Write_Completed => Write_Completed);

      end if;
    end Handle_Write_Ready;


    procedure Destroy(
      Client_Socket_Info_Ptr : in out Client_Socket_Info_Ptr_T) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Client_Socket_Info_T, Client_Socket_Info_Ptr_T);

    begin
      Free(Client_Socket_Info_Ptr);
    end Destroy;


    procedure Update_Epoll(
      Client_Socket_Info : in out Client_Socket_Info_T) is
    begin
      Client_Socket_Info.Proxy_Ptr.Get_Epoll_Ptr.Register_FD(
        FD => Client_Socket_Info.File_Descriptor,
        Interested_In_Read_Events =>
          (Client_Socket_Info.Waiting_For_Read),
        Interested_In_Write_Events =>
          (Client_Socket_Info.Waiting_For_Write or
           Client_Socket_Info.Waiting_For_Connect));
    end Update_Epoll;


    procedure Set_Waiting_For_Connect(
      Client_Socket_Info : in out Client_Socket_Info_T;
      Waiting_For_Connect : Boolean) is
    begin
      if (Waiting_For_Connect /= Client_Socket_Info.Waiting_For_Connect) then
        Client_Socket_Info.Waiting_For_Connect := Waiting_For_Connect;
        Client_Socket_Info.Update_Epoll;
      end if;
    end Set_Waiting_For_Connect;


    procedure Set_Waiting_For_Read(
      Client_Socket_Info : in out Client_Socket_Info_T;
      Waiting_For_Read : Boolean) is
    begin
      if (Waiting_For_Read /= Client_Socket_Info.Waiting_For_Read) then
        Client_Socket_Info.Waiting_For_Read := Waiting_For_Read;
        Client_Socket_Info.Update_Epoll;
      end if;
    end Set_Waiting_For_Read;


    procedure Set_Waiting_For_Write(
      Client_Socket_Info : in out Client_Socket_Info_T;
      Waiting_For_Write : Boolean) is
    begin
      if (Waiting_For_Write /= Client_Socket_Info.Waiting_For_Write) then
        Client_Socket_Info.Waiting_For_Write := Waiting_For_Write;
        Client_Socket_Info.Update_Epoll;
      end if;
    end Set_Waiting_For_Write;


    procedure Write_Buffered_Data(
      Client_Socket_Info : in out Client_Socket_Info_T;
      Write_Completed : out Boolean) is

      use type Ada.Streams.Stream_Element_Offset;
      use type GNAT.Sockets.Error_Type;

      Last_Write_Successful : Boolean := True;
      Related_Client_Socket_Info_Ptr : Client_Socket_Info_Ptr_T := null;
      Write_Result : Socket_Util.Write_Result_T;

    begin
      Write_Completed := False;
      while ((not Write_Completed) and
             Last_Write_Successful) loop

        Related_Client_Socket_Info_Ptr := 
          Client_Socket_Info.Related_Client_Socket_Info_Ptr;

        Socket_Util.Write(
          Socket => Client_Socket_Info.Client_Socket,
          Buffer => Client_Socket_Info.Write_Buffer(
            Client_Socket_Info.First_Write_Buffer_Write_Index ..
            Client_Socket_Info.Last_Write_Buffer_Write_Index),
          Result => Write_Result);

        case Write_Result.Status is

          when Socket_Util.Write_Would_Block =>
            Client_Socket_Info.Set_Waiting_For_Write(True);
            if (Related_Client_Socket_Info_Ptr /= null) then
              Related_Client_Socket_Info_Ptr.Set_Waiting_For_Read(False);
            end if;
            Last_Write_Successful := False;

          when Socket_Util.Write_Error | Socket_Util.Write_EOF =>
            Client_Socket_Info.Proxy_Ptr.Destroy_Client_Socket_Info(Client_Socket_Info);
            Last_Write_Successful := False;

          when Socket_Util.Write_Success =>
            Last_Write_Successful := True;
            Client_Socket_Info.First_Write_Buffer_Write_Index :=
              Client_Socket_Info.First_Write_Buffer_Write_Index +
              Write_Result.Num_Written;
            if (Client_Socket_Info.First_Write_Buffer_Write_Index >
                Client_Socket_Info.Last_Write_Buffer_Write_Index) then
              Write_Completed := True;
              Client_Socket_Info.First_Write_Buffer_Write_Index := 1;
              Client_Socket_Info.Last_Write_Buffer_Write_Index := 0;
              Client_Socket_Info.Set_Waiting_For_Write(False);

              if Client_Socket_Info.Destroy_When_Write_Finishes then
                Client_Socket_Info.Proxy_Ptr.Destroy_Client_Socket_Info(Client_Socket_Info);
              else
                Related_Client_Socket_Info_Ptr.Set_Waiting_For_Read(True);
              end if;
            end if;

        end case;
      end loop;

    end Write_Buffered_Data;


    procedure Finalize(
      Client_Socket_Info : in out Client_Socket_Info_T) is

      use type GNAT.Sockets.Socket_Type;

    begin
      if (Client_Socket_Info.File_Descriptor /= Null_File_Descriptor) then

        Client_Socket_Info.Proxy_Ptr.Get_Epoll_Ptr.Unregister_FD(
          FD => Client_Socket_Info.File_Descriptor);
        Client_Socket_Info.File_Descriptor := Null_File_Descriptor;

      end if;

      if (Client_Socket_Info.Client_Socket /= GNAT.Sockets.No_Socket) then

        GNAT.Sockets.Close_Socket(Socket => Client_Socket_Info.Client_Socket);
        Client_Socket_Info.Client_Socket := GNAT.Sockets.No_Socket;

      end if;

    end Finalize;

  end Client; 


end Proxy;
