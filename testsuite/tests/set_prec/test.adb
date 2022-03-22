with Ada.Text_IO; use Ada.Text_IO;
with AdMPFR;      use AdMPFR;

procedure Test is
   M : Mpfr_Float;
begin
   Set_Prec (M, 42);
   Set (M, "1");
   Put_Line (Get_Prec (M)'Image);
end Test;
