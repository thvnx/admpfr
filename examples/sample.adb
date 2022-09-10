--  #include <stdio.h>
with Ada.Text_IO; use Ada.Text_IO;

--  #include <gmp.h>
--  #include <mpfr.h>
with Admpfr; use Admpfr;

--  int main (void)
--  {
procedure Sample is
   --  unsigned int i;
   --  mpfr_t s, t, u;
   S, T, U : Mpfloat (200);
begin
   --  mpfr_init2 (t, 200);
   --  mpfr_set_d (t, 1.0, MPFR_RNDD);
   --  mpfr_init2 (s, 200);
   --  mpfr_set_d (s, 1.0, MPFR_RNDD);
   --  mpfr_init2 (u, 200);
   T.Set (Long_Float (1.0), RNDD);
   S.Set (Long_Float (1.0), RNDD);

   --  for (i = 1; i <= 100; i++)
   --    {
   --      mpfr_mul_ui (t, t, i, MPFR_RNDU);
   --      mpfr_set_d (u, 1.0, MPFR_RNDD);
   --      mpfr_div (u, u, t, MPFR_RNDD);
   --      mpfr_add (s, s, u, MPFR_RNDD);
   --    }
   for I in 1 .. 100 loop
      T.Mul (T, Long_Integer (I), RNDU);
      U.Set (Long_Float (1.0), RNDD);
      U.Div (U, T, RNDD);
      S.Add (S, U, RNDD);
   end loop;

   --  printf ("Sum is ");
   Put ("Sum is ");

   --  mpfr_out_str (stdout, 10, 0, s, MPFR_RNDD);
   Out_Str (Standard_Output, S, 10, RNDD);

--    putchar ('\n');
   Put (ASCII.LF);

--    mpfr_clear (s);
--    mpfr_clear (t);
--    mpfr_clear (u);
--    mpfr_free_cache ();
--    return 0;
--  }
end Sample;
