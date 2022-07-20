with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   M, N : Mpfloat;
begin
   Set (M, "1");
   Set_Default_Prec (42);
   Set (N, "1");
   Put_Line (Get_Prec (M)'Image);
   Put_Line (Get_Prec (N)'Image);

   declare
      O : Mpfloat;
   begin
      Put_Line (Get_Prec (O)'Image);
   end;
end Test;
