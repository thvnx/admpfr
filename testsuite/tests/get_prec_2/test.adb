with Ada.Text_IO;    use Ada.Text_IO;
with Admpfr;         use Admpfr;

procedure Test is
   M : Mpfloat;
   N : Mpfloat (1);
begin
   Set (M, "1");
   Put_Line (Get_Prec (M)'Image);

   Put_Line (Get_Prec (N)'Image);
end Test;
