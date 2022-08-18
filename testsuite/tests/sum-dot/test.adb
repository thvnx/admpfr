with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   M : Mpfloat;
   A : Mpfloat_Array (1 .. 1000);
   E : Mpfloat_Array (0 .. -1);
begin
   for I in A'Range loop
      A (I).Set (Long_Integer (I));
      Put_Line (A (I)'Image);
   end loop;

   M.Sum (A);
   Put_Line (M'Image);

   M.Dot (A, A);
   Put_Line (M'Image);

   M.Dot (A (1 .. 2), A);
   Put_Line (M'Image);

   M.Sum (E);
   Put_Line (M'Image);

   M.Dot (E, A);
   Put_Line (M'Image);
end Test;
