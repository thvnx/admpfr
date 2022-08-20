with Ada.Text_IO;    use Ada.Text_IO;
with Admpfr;         use Admpfr;

procedure Test is
   M, N, O, P : Mpfloat;
   B : Boolean;
begin
   M.Set ("0.1");
   N.Set ("-0.1");
   O.Set_Nan;
   P.Set_Inf (Pos);

   Put_Line (M.Cmp (N)'Image);
   Put_Line (N.Cmp (M)'Image);
   Put_Line (M.Cmp (M)'Image);

   Put_Line (M.Cmp (-1)'Image);
   Put_Line (N.Cmp (1)'Image);

   Put_Line (M.Cmp (Long_Float (0.1))'Image);
   Put_Line (N.Cmp (Long_Float (0.1))'Image);

   Put_Line (M.Cmp (Long_Long_Float (0.1))'Image);
   Put_Line (N.Cmp (Long_Long_Float (0.1))'Image);

   Put_Line (M.Cmp (-1, 3)'Image);
   Put_Line (N.Cmp (1, -3)'Image);

   Put_Line (M.Cmp_Abs (-1)'Image);
   Put_Line (N.Cmp_Abs (1)'Image);

   Put_Line (M.Cmp_Abs (N)'Image);
   Put_Line (N.Cmp_Abs (M)'Image);
   Put_Line (M.Cmp_Abs (M)'Image);

   B := M = N; Put_Line (B'Image);
   B := M /= N; Put_Line (B'Image);
   B := M > N; Put_Line (B'Image);
   B := M < N; Put_Line (B'Image);
   B := M >= N; Put_Line (B'Image);
   B := M <= N; Put_Line (B'Image);

   B := M = P; Put_Line (B'Image);
   B := M /= P; Put_Line (B'Image);
   B := M > P; Put_Line (B'Image);
   B := M < P; Put_Line (B'Image);
   B := M >= P; Put_Line (B'Image);
   B := M <= P; Put_Line (B'Image);

   B := M = O; Put_Line (B'Image);
   B := M > O; Put_Line (B'Image);
   B := M < O; Put_Line (B'Image);
   B := M >= O; Put_Line (B'Image);
   B := M <= O; Put_Line (B'Image);

   Put_Line (M.Greater (N)'Image);
   Put_Line (M.Greaterequal (N)'Image);
   Put_Line (M.Less (N)'Image);
   Put_Line (M.Lessequal (N)'Image);
   Put_Line (M.Equal (N)'Image);
   Put_Line (M.Lessgreater (N)'Image);
   Put_Line (M.Unordered (N)'Image);
   Put_Line (M.Total_Order (N)'Image);
end Test;
