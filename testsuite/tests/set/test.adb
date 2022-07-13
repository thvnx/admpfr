with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   A, B, C : Mpfloat;
begin
   Set (A, "-0.1");
   Set (B, "0");
   Set (C, "0.1");

   for R in Rounding'Range loop
      for B in 0 .. 62 loop
         begin
            Set (A, "-0.1", Base => Base (B), Rnd => R);
            Set (Test.B, "0", Base => Base (B), Rnd => R);
            Set (C, "0.1", Base => Base (B), Rnd => R);
         exception
            when others => Put_Line ("Base not in range?: " & B'Image);
         end;
      end loop;
   end loop;
end Test;
