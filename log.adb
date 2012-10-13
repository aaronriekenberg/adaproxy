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
