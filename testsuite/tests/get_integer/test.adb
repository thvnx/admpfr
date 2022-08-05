with Ada.Text_IO;    use Ada.Text_IO;
with Admpfr;         use Admpfr;

procedure Test is
   M   : Mpfloat;
   I   : Long_Integer;
begin
   M.Set (2);
   I := M.Get_Long_Integer;
   Put_Line (I'Image);
   Put_Line (M.Fits_Long_Integer'Image);

   M.Set (Float (0.1));
   I := M.Get_Long_Integer;
   Put_Line (I'Image);
   Put_Line (M.Fits_Long_Integer'Image);
   --  TODO: test inexact flag

   M.Set_Nan;
   I := M.Get_Long_Integer;
   Put_Line (I'Image);
   Put_Line (M.Fits_Long_Integer'Image);
   --  TODO: test erange flag

   M.Set ("1e999999");
   I := M.Get_Long_Integer;
   Put_Line (I'Image);
   Put_Line (Long_Integer'Last'Image);
   Put_Line (M.Fits_Integer'Image);
   Put_Line (M.Fits_Long_Integer'Image);
   --  TODO: test erange flag

   M.Set (Long_Integer'Last, RNDD);
   I := M.Get_Long_Integer;
   Put_Line (I'Image);
   Put_Line (M.Fits_Integer'Image);
   Put_Line (M.Fits_Long_Integer'Image);
end Test;
