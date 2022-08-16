with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   M, N : Mpfloat;
begin
   N.Set (Float (1.4));

   M.Cbrt (N);
   Put_Line (M'Image);

   M.Rootn (N, 3);
   Put_Line (M'Image);
end Test;
