pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Main is
   N : Mpfloat;
begin
   N.Set ("0.1");
   Put_Line (N'Image);
end Main;
