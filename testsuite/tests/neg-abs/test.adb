with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   M, N : Mpfloat;
begin
   N.Set (Float (1.0));

   M.Neg (N);
   Put_Line (M'Image);

   M.Absolute (M);
   Put_Line (M'Image);
end Test;
