with Admpfr; use Admpfr;

procedure Test is
   A : Mpfloat;
begin
   for R in Rounding'Range loop
      for B in Base'Range loop
         Set (A, "-0.1", Base => B, Rnd => R);
         Mpfr_Printf ("%.R*e" & ASCII.LF, A, R);
      end loop;
   end loop;
end Test;
