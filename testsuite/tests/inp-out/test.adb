with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   F, I  : File_Type;
   M, N : Mpfloat;
begin
   Create (F, Out_File, "tt.txt");
   Create (I, Out_File, "fpif.txt");
   M.Const_Catalan;
   Out_Str (F, M);
   Fpif_Export (I, M);
   Put_Line (F, "");
   M.Const_Euler;
   Out_Str (F, M);
   Fpif_Export (I, M);
   Put_Line (F, "");
   M.Const_Log2;
   Out_Str (F, M);
   Fpif_Export (I, M);
   Put_Line (F, "");
   Close (F);
   Close (I);

   Open (F, In_File, "tt.txt");
   Open (I, In_File, "fpif.txt");
   Inp_Str (N, F);
   Put_Line (N'Image);
   Fpif_Import (N, I);
   Put_Line (N'Image);
   N.Dump;
   Inp_Str (N, F);
   Put_Line (N'Image);
   Fpif_Import (N, I);
   Put_Line (N'Image);
   N.Dump;
   Inp_Str (N, F);
   Put_Line (N'Image);
   Fpif_Import (N, I);
   Put_Line (N'Image);
   N.Dump;
   Close (F);

   Out_Str (Standard_Output, N);

end Test;
