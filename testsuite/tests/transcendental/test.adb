with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Test is
   type Fn_1 is access
     procedure (Rop : in out Mpfloat; Op : Mpfloat; Rnd : Rounding);
   type Fn_2 is access
     procedure (Rop : in out Mpfloat; Op1, Op2 : Mpfloat; Rnd : Rounding);

   procedure Test_Fn_1
     (Mpfr_Fn : Fn_1;
      Op      : Mpfloat);
   procedure Test_Fn_2
     (Mpfr_Fn  : Fn_2;
      Op1, Op2 : Mpfloat);

   procedure Test_Fn_1
     (Mpfr_Fn : Fn_1;
      Op      : Mpfloat)
   is
      Rop : Mpfloat;
   begin
      Put_Line ("Fn1");
      for Rnd in Rounding'Range loop
         Mpfr_Fn (Rop, Op, Rnd);
         Put_Line (Rop'Image);
      end loop;
      Put (ASCII.CR);
   end Test_Fn_1;

   procedure Test_Fn_2
     (Mpfr_Fn  : Fn_2;
      Op1, Op2 : Mpfloat)
   is
      Rop : Mpfloat;
   begin
      Put_Line ("Fn2");
      for Rnd in Rounding'Range loop
         Mpfr_Fn (Rop, Op1, Op2, Rnd);
         Put_Line (Rop'Image);
      end loop;
      Put (ASCII.CR);
   end Test_Fn_2;

   N, M : Mpfloat;
begin
   N.Set (1);
   Test_Fn_1 (Log'Access, N);
   Test_Fn_1 (Log2'Access, N);
   Test_Fn_1 (Log10'Access, N);
   Test_Fn_1 (Log1p'Access, N);
   Test_Fn_1 (Log2p1'Access, N);
   Test_Fn_1 (Log10p1'Access, N);
   Test_Fn_1 (Exp'Access, N);
   Test_Fn_1 (Exp2'Access, N);
   Test_Fn_1 (Exp10'Access, N);
   Test_Fn_1 (Expm1'Access, N);
   Test_Fn_1 (Cos'Access, N);
   Test_Fn_1 (Sin'Access, N);
   Test_Fn_1 (Tan'Access, N);
   Test_Fn_1 (Sec'Access, N);
   Test_Fn_1 (Csc'Access, N);
   Test_Fn_1 (Cot'Access, N);
   Test_Fn_1 (Acos'Access, N);
   Test_Fn_1 (Asin'Access, N);
   Test_Fn_1 (Atan'Access, N);
   Test_Fn_1 (Cosh'Access, N);
   Test_Fn_1 (Sinh'Access, N);
   Test_Fn_1 (Tanh'Access, N);
   Test_Fn_1 (Sech'Access, N);
   Test_Fn_1 (Csch'Access, N);
   Test_Fn_1 (Coth'Access, N);
   Test_Fn_1 (Acosh'Access, N);
   Test_Fn_1 (Asinh'Access, N);
   Test_Fn_1 (Atanh'Access, N);
   Test_Fn_1 (Eint'Access, N);
   Test_Fn_1 (Li2'Access, N);
   Test_Fn_1 (Gamma'Access, N);
   Test_Fn_1 (Lngamma'Access, N);
   Test_Fn_1 (Digamma'Access, N);
   Test_Fn_1 (Zeta'Access, N);
   Test_Fn_1 (Erf'Access, N);
   Test_Fn_1 (Erfc'Access, N);
   Test_Fn_1 (J0'Access, N);
   Test_Fn_1 (J1'Access, N);
   Test_Fn_1 (Y0'Access, N);
   Test_Fn_1 (Y1'Access, N);
   Test_Fn_1 (Ai'Access, N);

   M.Set (2);
   Test_Fn_2 (Pow'Access, N, M);
   Test_Fn_2 (Atan2'Access, N, M);
   Test_Fn_2 (Gamma_Inc'Access, N, M);
   Test_Fn_2 (Beta'Access, N, M);
   Test_Fn_2 (Agm'Access, N, M);
end Test;
