with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Main is
   N : Mpfloat;
begin
   Set (N, "0.1");
   Put_Line (To_String (N));
end Main;
