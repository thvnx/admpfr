with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   M, N : Mpfloat;
begin
   N.Set (Float (1.0));

   M.Sqr (N);
   Put_Line (M'Image);

   N.Set (Float (2.0));

   M.Sqr (N);
   Put_Line (M'Image);
end Test;
