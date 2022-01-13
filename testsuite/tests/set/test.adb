with Admpfr; use Admpfr;

procedure Test is
   A, B, C : Mpfr_Float;
begin
   Set (A, "-0.1");
   Set (B, "0");
   Set (C, "0.1");

   for R in Rnd_T'Range loop
      for B in Base_T'Range loop
         Set (A, "-0.1", Base => B, Rnd => R);
         Set (Test.B, "0", Base => B, Rnd => R);
         Set (C, "0.1", Base => B, Rnd => R);
      end loop;
   end loop;
end Test;
