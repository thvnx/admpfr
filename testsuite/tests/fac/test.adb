with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   M : Mpfloat;
begin
   M.Fac (10);
   Put_Line (M'Image);
end Test;
