with Ada.Text_Io; use Ada.Text_Io;
with AdMPFR;      use AdMPFR;

procedure Main is
   N : Mpfr_Float;
begin
   Set (N, "0.1");
   Put_Line (To_String (N));
end Main;
