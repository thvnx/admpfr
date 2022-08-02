with Ada.Text_IO; use Ada.Text_IO;
with Admpfr; use Admpfr;

procedure Test is
   A : Mpfloat;
begin
   for R in Rounding'Range loop
      for B in 2 .. 62 loop
         Set (A, "-0.1", Base => Base (B), Rnd => R);
         Put_Line (A'Image);
      end loop;
   end loop;
end Test;
