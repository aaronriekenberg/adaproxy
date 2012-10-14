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

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Log is

  Microseconds_In_Second : constant := 1_000_000;


  function Time_String return String is

    Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
    Date_And_Time_Image : constant String :=
      Ada.Calendar.Formatting.Image(
        Date => Now,
        Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset(Date => Now));
    Micro_Image : constant String :=
      Ada.Strings.Fixed.Tail(
        Source => Ada.Strings.Fixed.Trim(
          Source => Natural'Image(
                      Natural(Ada.Calendar.Formatting.Sub_Second(Now) *
                              Microseconds_In_Second)),
          Side => Ada.Strings.Left),
        Count => 6,
        Pad => '0');

  begin
    return (Date_And_Time_Image & "." & Micro_Image);
  end Time_String;


  procedure Log(Message : in String) is
  begin
    Ada.Text_IO.Put_Line(Time_String & " " & Message);
  end Log;

end Log;
