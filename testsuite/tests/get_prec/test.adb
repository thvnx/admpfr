with Ada.Text_Io; use Ada.Text_Io;
with Admpfr;      use Admpfr;

procedure Test is
   M : Mpfr_Float;
begin
   Set (M, "1");
   Put_Line (Get_Prec (M)'Image);
end Test;
