with Ada.Text_IO;    use Ada.Text_IO;
with Admpfr;         use Admpfr;

procedure Test is
   M   : Mpfloat;
   I   : Long_Integer;
begin
   M.Set (2);
   I := M.Get_Long_Integer;
   Put_Line (I'Image);

   M.Set (Float (0.1));
   I := M.Get_Long_Integer;
   Put_Line (I'Image);
   --  TODO: test inexact flag

   M.Set_Nan;
   I := M.Get_Long_Integer;
   Put_Line (I'Image);
   --  TODO: test erange flag

   M.Set ("1e999999");
   I := M.Get_Long_Integer;
   Put_Line (I'Image);
   Put_Line (Long_Integer'Last'Image);
   --  TODO: test erange flag
end Test;
