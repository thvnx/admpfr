with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   A : Mpfloat;
begin
   for R in Rounding'Range loop
      for B in 0 .. 62 loop
         begin
            Set (A, "-0.1", Base => Base (B), Rnd => R);
            Printf ("%.R*e" & ASCII.LF, A, R);
         exception
            when
              others => Put_Line ("Base not in range?: " & B'Image);
         end;
      end loop;
   end loop;

   Set (A, "0.1");
   Printf ("%.RUe" & ASCII.LF, A);
   Printf ("%.RDe" & ASCII.LF, A);
   Printf ("%.RYe" & ASCII.LF, A);
   Printf ("%.RZe" & ASCII.LF, A);
   Printf ("%.RNe" & ASCII.LF, A);
   Printf ("%.R*e" & ASCII.LF, A, RNDN);
end Test;
