with Ada.Text_IO; use Ada.Text_IO;
with AdMPFR;      use AdMPFR;

procedure Main is
   N : Mpfr_Float;
begin
   Set (N, "0.1");
   Put_Line (To_String (N));
end Main;
