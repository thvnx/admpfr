with AdMPFR;      use AdMPFR;

procedure Test is
   A : Mpfr_Float;
begin
   for R in Rnd_T'Range loop
      for B in Base_T'Range loop
         Set (A, "-0.1", Base => B, Rnd => R);
         Mpfr_Printf ("%.R*e" & ASCII.LF, R, A);
      end loop;
   end loop;
end Test;
