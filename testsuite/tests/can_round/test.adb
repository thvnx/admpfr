with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   X, Y : Mpfloat;
   Px   : constant Precision := 53;
   Py   : constant Precision := 50;
   OK   : Boolean;
   Err  : Exponent;
begin
   X.Set_Prec (Px);
   Y.Set_Prec (Py);

   for R1 in RNDN .. RNDD loop

      X.Const_Pi (R1);
      Put ("R1=" & R1'Image & " approx=");
      Out_Str (Standard_Output, X, 2);
      Put (ASCII.LF);

      for R2 in RNDN .. RNDD loop
         Err := Exponent (X.Get_Prec);
         OK := X.Can_Round (Err, R1, R2, Py);
         Put ("R2=" & R2'Image & " OK=" & OK'Image);
         if OK then
            Y.Set (X, R2);
            Put ("   ");
            Out_Str (Standard_Output, Y, 2);
         end if;
         Put (ASCII.LF);
      end loop;
   end loop;
end Test;
