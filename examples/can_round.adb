--  #include <stdio.h>
--  #include <mpfr.h>
with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

--  int
--  main (void)
--  {
procedure Can_Round is
   --  mpfr_t x, y;
   --  mpfr_prec_t px = 53, py = 50;
   --  mpfr_rnd_t r1, r2;
   --  int ok;
   X, Y : Mpfloat;
   Px   : constant Precision := 53;
   Py   : constant Precision := 50;
   OK   : Boolean;
   Err  : Exponent;
begin
   --  Given an approximation of Pi to px bits computed with rounding mode r1,
   --  we call mpfr_can_round() to see if we can deduced the correct rounding
   --  of Pi to py bits with rounding mode r2.
   --  The error is at most 1 = 2^0 ulp. This translates into err = prec(x).
   --  mpfr_init2 (x, px);
   --  mpfr_init2 (y, py);
   X.Set_Prec (Px);
   Y.Set_Prec (Py);

   --  for (r1 = 0; r1 < 4; r1++)
   --    {
   for R1 in RNDN .. RNDD loop

   --  mpfr_const_pi (x, r1);
   --  printf ("r1=%s approx=", mpfr_print_rnd_mode (r1));
   --  mpfr_out_str (stdout, 2, 0, x, MPFR_RNDN);
   --  printf ("\n");
      X.Const_Pi (R1);
      Put ("R1=" & R1'Image & " approx=");
      Out_Str (Standard_Output, X, 2);
      Put (ASCII.LF);

   --  for (r2 = 0; r2 < 4; r2++)
   --    {
      for R2 in RNDN .. RNDD loop
      --  ok = mpfr_can_round (x, mpfr_get_prec (x), r1, r2, py);
      --  printf ("r2=%s ok=%d", mpfr_print_rnd_mode (r2), ok);
         Err := Exponent (X.Get_Prec);
         OK := X.Can_Round (Err, R1, R2, Py);
         Put ("R2=" & R2'Image & " OK=" & OK'Image);
      --  if (ok)
      --    {
         if OK then
         --  mpfr_set (y, x, r2);
         --  printf ("   ");
         --  mpfr_out_str (stdout, 2, 0, y, MPFR_RNDN);
            Y.Set (X, R2);
            Put ("   ");
            Out_Str (Standard_Output, Y, 2);

         --  }
         end if;

         --  printf ("\n");
         Put (ASCII.LF);

      --  }
      end loop;

   --  }
   end loop;
   --  mpfr_clear (x);
   --  mpfr_clear (y);
   --  mpfr_free_cache ();
   --  return 0;
   --  }
end Can_Round;