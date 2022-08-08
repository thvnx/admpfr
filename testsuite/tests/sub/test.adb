with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   M, N, O : Mpfloat;
begin
   N.Set (Float (1.0));
   O.Set (Float (4.0));

   M.Sub (N, O);
   Put_Line (M'Image);

   M.Sub (N, 5);
   Put_Line (M'Image);

   M.Sub (N, 6.0);
   Put_Line (M'Image);

   M.Sub (5, N);
   Put_Line (M'Image);

   M.Sub (6.0, N);
   Put_Line (M'Image);
end Test;
