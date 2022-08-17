with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   M, N, O : Mpfloat;
begin
   N.Set (Float (1.0));
   O.Set (Float (4.0));

   M.Div (N, O);
   Put_Line (M'Image);

   M.Div (N, 5);
   Put_Line (M'Image);

   M.Div (N, 6.0);
   Put_Line (M'Image);

   M.Div (5, N);
   Put_Line (M'Image);

   M.Div (6.0, N);
   Put_Line (M'Image);

   M.Div_2 (N, 8);
   Put_Line (M'Image);
end Test;
