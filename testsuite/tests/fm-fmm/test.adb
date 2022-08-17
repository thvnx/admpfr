with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   M, N, O, P, Q : Mpfloat;
begin
   N.Set ("1.1");
   O.Set ("2.2");
   P.Set ("3.3");
   Q.Set ("4.4");

   M.Fma (N, O, P);
   Put_Line (M'Image);

   M.Fms (N, O, P);
   Put_Line (M'Image);

   M.Fmma (N, O, P, Q);
   Put_Line (M'Image);

   M.Fmms (N, O, P, Q);
   Put_Line (M'Image);

end Test;
