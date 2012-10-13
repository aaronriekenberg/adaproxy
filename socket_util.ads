with Ada.Streams;
with GNAT.Sockets;

package Socket_Util is

  -- This package shouldn't need to exist.  The GNAT.Sockets package is so ridiculously hard
  -- to use that I needed to put all the ugliness in one place.

  procedure Set_Socket_Non_Blocking(
    Socket : in GNAT.Sockets.Socket_Type);

  function Get_Socket_Error(
    Socket : in GNAT.Sockets.Socket_Type)
    return GNAT.Sockets.Error_Type;

  type Accept_Status_T is (
    Accept_Would_Block,
    Accept_Error,
    Accept_Success);

  type Accept_Result_T(Status : Accept_Status_T := Accept_Would_Block) is
    record
      case Status is

        when Accept_Would_Block =>
          null;

        when Accept_Error =>
          Error_Type : GNAT.Sockets.Error_Type;

        when Accept_Success =>
          Client_Socket : GNAT.Sockets.Socket_Type;
          Client_Address : GNAT.Sockets.Sock_Addr_Type;

      end case;

    end record;

  procedure Accept_Socket(
    Server_Socket : in GNAT.Sockets.Socket_Type;
    Result : out Accept_Result_T);

  type Connect_Status_T is (
    Connect_In_Progress,
    Connect_Error,
    Connect_Success);

  type Connect_Result_T(Status : Connect_Status_T := Connect_In_Progress) is
    record
      case Status is

        when Connect_In_Progress =>
          null;

        when Connect_Error =>
          Error_Type : GNAT.Sockets.Error_Type;

        when Connect_Success =>
          null;

      end case;

    end record;

  procedure Connect(
    Socket : in GNAT.Sockets.Socket_Type;
    Remote_Address : in GNAT.Sockets.Sock_Addr_Type;
    Result : out Connect_Result_T);

  type Read_Status_T is (
    Read_Would_Block,
    Read_Error,
    Read_EOF,
    Read_Success);

  type Read_Result_T(Status : Read_Status_T := Read_Would_Block) is
    record
      case Status is

        when Read_Would_Block =>
          null;

        when Read_Error =>
          Error_Type : GNAT.Sockets.Error_Type;

        when Read_EOF =>
          null;

        when Read_Success =>
          Num_Read : Ada.Streams.Stream_Element_Offset;

      end case;

    end record;

  procedure Read(
    Socket : in GNAT.Sockets.Socket_Type;
    Buffer : out Ada.Streams.Stream_Element_Array;
    Result : out Read_Result_T);

  type Write_Status_T is (
    Write_Would_Block,
    Write_Error,
    Write_EOF,
    Write_Success);

  type Write_Result_T(Status : Write_Status_T := Write_Would_Block) is
    record
      case Status is

        when Write_Would_Block =>
          null;

        when Write_Error =>
          Error_Type : GNAT.Sockets.Error_Type;

        when Write_EOF =>
          null;

        when Write_Success =>
          Num_Written : Ada.Streams.Stream_Element_Offset;

      end case;

    end record;

  procedure Write(
    Socket : in GNAT.Sockets.Socket_Type;
    Buffer : in Ada.Streams.Stream_Element_Array;
    Result : out Write_Result_T);

end Socket_Util;
