with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   M, N, O : Mpfloat;
begin
   N.Set (Float (1.5));
   O.Set (Float (4.0));

   M.Mul (N, O);
   Put_Line (M'Image);

   M.Mul (N, 5);
   Put_Line (M'Image);

   M.Mul (N, 6.0);
   Put_Line (M'Image);

   M.Mul_2 (N, 8);
   Put_Line (M'Image);
end Test;
