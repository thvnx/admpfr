with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   A, B, C, D, E, F : Mpfloat;
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

   for R in Rounding'Range loop
      Set (A, "-0.1", Rnd => R);
      for B in -36 .. 62 loop
         begin
            Put_Line (To_String (A, Base => Base (B), Rnd => R));
         exception
            when
              others => Put_Line ("Base not in range?: " & B'Image);
         end;
      end loop;
   end loop;

   for B in 2 .. 62 loop
      begin
         Set (A, "0.1", Base => Base (B));
         if Sprintf ("%.R*e", A, RNDN) /= A.To_String then
            Put_Line ("diff: " &
                        Sprintf ("%.R*e", A, RNDN) &
                        " /= " & A.To_String);
         end if;
      exception
         when
           others => Put_Line ("Base not in range?: " & B'Image);
      end;
   end loop;
end Test;
