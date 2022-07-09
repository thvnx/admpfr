with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   M : Mpfloat;
begin
   Set_Prec (M, 42);
   Set (M, "1");
   Put_Line (Get_Prec (M)'Image);
end Test;
