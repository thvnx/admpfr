with Ada.Text_IO; use Ada.Text_IO;
with Admpfr; use Admpfr;

procedure Test is
   S, T, U : Mpfloat (200);
begin
   T.Set (Long_Float (1.0), RNDD);
   S.Set (Long_Float (1.0), RNDD);

   for I in 1 .. 100 loop
      T.Mul (T, Long_Integer (I), RNDU);
      U.Set (Long_Float (1.0), RNDD);
      U.Div (U, T, RNDD);
      S.Add (S, U, RNDD);
   end loop;

   Put ("Sum is ");

   Out_Str (Standard_Output, S, 10, RNDD);
   Put (ASCII.LF);
end Test;
