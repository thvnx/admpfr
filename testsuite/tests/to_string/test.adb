with Ada.Text_Io; use Ada.Text_Io;
with Admpfr;      use Admpfr;

procedure Test is
   A, B, C, D, E, F : Mpfr_Float;
begin
   Set (A, "-0.1");
   Put_Line (To_String (A));
   Set (B, "0");
   Put_Line (To_String (B));
   Set (C, "-0");
   Put_Line (To_String (C));
   Set (D, "0.1");
   Put_Line (To_String (D));
   Set (E, "1");
   Put_Line (To_String (E));
   Set (F, "999e999");
   Put_Line (To_String (F));

   for R in Rnd_T'Range loop
      for B in Base_T'Range loop
         Set (A, "-0.1", Base => B, Rnd => R);
         Put_Line (To_String (A, Base => B, Rnd => R));
      end loop;
   end loop;
end Test;
