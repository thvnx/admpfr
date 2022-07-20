with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   A, B, C : Mpfloat;
begin
   Set (A, "-0.1");
   Set (B, "0");
   Set (C, "0.1");

   for R in Rounding'Range loop
      for B in 0 .. 62 loop
         begin
            Set (A, "-0.1", Base => Base (B), Rnd => R);
            Set (Test.B, "0", Base => Base (B), Rnd => R);
            Set (C, "0.1", Base => Base (B), Rnd => R);
         exception
            when others => Put_Line ("Base not in range?: " & B'Image);
         end;
      end loop;
   end loop;

   declare
      D, E : Mpfloat (12);
   begin
      Put_Line (A.Get_Ternary_Value'Image);
      Put_Line (B.Get_Ternary_Value'Image);
      Put_Line (C.Get_Ternary_Value'Image);
      Set (C, E);
      Put_Line (C.Get_Ternary_Value'Image);
      Set (D, A);
      Put_Line (D.Get_Ternary_Value'Image);
      Set (E, B);
      Put_Line (E.Get_Ternary_Value'Image);
   end;

   declare
      F, G, H : Mpfloat (2);
   begin
      Set (F, 0);
      Put_Line (F.Get_Ternary_Value'Image);
      Set (G, -567);
      Put_Line (G.Get_Ternary_Value'Image);
      Set (H, 567);
      Put_Line (H.Get_Ternary_Value'Image);
   end;

   declare
      A : constant Float := Float'Last;
      B : constant Long_Float := Long_Float'First;
      C : constant Float := 0.0;
      I, J, K : Mpfloat;
   begin
      Set (I, A);
      Put_Line (I.Get_Ternary_Value'Image);
      Set (J, B);
      Put_Line (J.Get_Ternary_Value'Image);
      Set (K, C);
      Put_Line (K.Get_Ternary_Value'Image);
   end;

   declare
      A, B : Mpfloat;
   begin
      Set (A, 1, 10);
      Put_Line (A.Get_Ternary_Value'Image);
      Set (B, 1, 10_000);
      Put_Line (B.Get_Ternary_Value'Image);
   end;
end Test;
