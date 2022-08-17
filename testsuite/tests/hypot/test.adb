with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   M, N : Mpfloat;
begin
   M.Set (Float (3.3));
   N.Set (Float (1.0));

   M.Hypot (M, N);
   Put_Line (M'Image);
end Test;
