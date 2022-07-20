with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   M : Mpfloat;
begin
   Set_Default_Prec (1337);
   Set (M, "1");
   Put_Line (Get_Default_Prec'Image);
end Test;
