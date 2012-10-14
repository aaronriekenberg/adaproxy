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

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Proxy;

procedure Main is

  Proxy_Command_Line_Exception : exception;

  function Parse_Address_Argument(
    Argument : in String) 
    return Proxy.Address_And_Port is
  begin
    for I in Argument'Range loop
      if (Argument(I) = ':') then
        return (Address => Ada.Strings.Unbounded.To_Unbounded_String(
                             Argument(Argument'First .. (I - 1))),
                Port => Natural'Value(
                          Argument((I + 1) .. Argument'Last)));
      end if;
    end loop;
    raise Proxy_Command_Line_Exception with ("Invalid Address '" & Argument & "'");
  exception
    when PCLE : Proxy_Command_Line_Exception =>
      raise;
    when others =>
      raise Proxy_Command_Line_Exception with ("Invalid Address '" & Argument & "'");
  end Parse_Address_Argument;

  function Get_Remote_Address_And_Port
    return Proxy.Address_And_Port is
  begin
    return Parse_Address_Argument(Ada.Command_Line.Argument(
             Ada.Command_Line.Argument_Count));
  end Get_Remote_Address_And_Port;

  function Get_Listen_Address_And_Ports
    return Proxy.Address_And_Port_Array is
    Listen_Address_And_Ports : Proxy.Address_And_Port_Array(
      1 .. (Ada.Command_Line.Argument_Count - 1));
  begin
    for I in Listen_Address_And_Ports'Range loop
      Listen_Address_And_Ports(I) :=
        Parse_Address_Argument(Ada.Command_Line.Argument(I));
    end loop;
    return Listen_Address_And_Ports;
  end Get_Listen_Address_And_Ports;

  Proxy_Ptr : Proxy.Proxy_Ptr_T := null;

begin

  if (Ada.Command_Line.Argument_Count < 2) then

    Ada.Text_IO.Put_Line("Usage: " & Ada.Command_Line.Command_Name &
                         " <listen addr> [<listen addr> ...] <remote addr>");
    Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);

  else

    Proxy_Ptr :=
      Proxy.Create(
        Listen_Address_And_Ports => Get_Listen_Address_And_Ports,
        Remote_Address_And_Port => Get_Remote_Address_And_Port);

    Proxy_Ptr.Run;

  end if;

end Main;
