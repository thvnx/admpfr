with Ada.Text_Io; use Ada.Text_Io;
with AdMPFR;

procedure Main is
   N, M, O : Admpfr.Mpfr_Float;
begin
   Admpfr.Set (N, "-0.1");
   Put_Line (Admpfr.To_String (N));
   Admpfr.Set (M, "1");
   Put_Line (Admpfr.To_String (M));
   Admpfr.Set (O, "-10");
   Put_Line (Admpfr.To_String (O));
end Main;
