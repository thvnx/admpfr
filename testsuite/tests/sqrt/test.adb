with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   M, N : Mpfloat;
begin
   N.Set (Float (1.5));

   M.Sqrt (N);
   Put_Line (M'Image);

   M.Sqrt (4);
   Put_Line (M'Image);

   M.Rec_Sqrt (N);
   Put_Line (M'Image);
end Test;
