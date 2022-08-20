with Ada.Text_IO;    use Ada.Text_IO;
with Admpfr;         use Admpfr;

procedure Test is
   M : Mpfloat;
   NaN, Inf, Zero : Mpfloat;
begin
   M.Set (12);
   NaN.Set_Nan;
   Inf.Set_Inf (Pos);
   Zero.Set_Zero (Pos);

   Put_Line (M.Is_Nan'Image);
   Put_Line (M.Is_Inf'Image);
   Put_Line (M.Is_Number'Image);
   Put_Line (M.Is_Zero'Image);
   Put_Line (M.Is_Regular'Image);

   Put_Line (NaN.Is_Nan'Image);
   Put_Line (NaN.Is_Inf'Image);
   Put_Line (NaN.Is_Number'Image);
   Put_Line (NaN.Is_Zero'Image);
   Put_Line (NaN.Is_Regular'Image);

   Put_Line (Inf.Is_Nan'Image);
   Put_Line (Inf.Is_Inf'Image);
   Put_Line (Inf.Is_Number'Image);
   Put_Line (Inf.Is_Zero'Image);
   Put_Line (Inf.Is_Regular'Image);

   Put_Line (Zero.Is_Nan'Image);
   Put_Line (Zero.Is_Inf'Image);
   Put_Line (Zero.Is_Number'Image);
   Put_Line (Zero.Is_Zero'Image);
   Put_Line (Zero.Is_Regular'Image);
end Test;
