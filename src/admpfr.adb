--  This file is part of Admpfr.
--
--  Admpfr is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  Admpfr is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Admpfr.  If not, see <https://www.gnu.org/licenses/>.

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

with Ada.Text_IO.C_Streams;

with Admpfr.Bindings;        use Admpfr.Bindings;
with Admpfr.Custom_Bindings; use Admpfr.Custom_Bindings;

package body Admpfr is

   --------------------------
   -- Reformat_Printf_Args --
   --------------------------

   procedure Reformat_Printf_Args (T : in out String; R : in out Rounding) is
   begin
      for I in 1 .. T'Length loop
         if T (I) = 'R' and then R = RNDN then
            case T (I + 1) is
               when 'N' => T (I + 1) := '*';
               when 'U' => T (I + 1) := '*'; R := RNDU;
               when 'D' => T (I + 1) := '*'; R := RNDD;
               when 'Y' => T (I + 1) := '*'; R := RNDA;
               when 'Z' => T (I + 1) := '*'; R := RNDZ;
               when '*' => null;
               when others => raise Failure;
            end case;
         end if;
         exit when T (I) = '*';
      end loop;
   end Reformat_Printf_Args;

   ------------
   -- Printf --
   ------------

   procedure Printf (Template : String;
                     X        : Mpfloat;
                     R        : Rounding := RNDEF) is
      Res   : int := -1;
      Input : chars_ptr := Null_Ptr;
      Tpl   : String := Template;
      Rnd   : Rounding := R;
   begin
      begin
         Reformat_Printf_Args (Tpl, Rnd);
         Input := New_String (Tpl);
         Res := printf_stub (Input, Rounding'Pos (Rnd), X.Value'Access);
         Free (Input);

         if Res < 0 then
            raise Failure with "mpfr_printf failure";
         end if;

      exception
         when Storage_Error =>
            raise Failure with "mpfr_printf malformation";
      end;
   end Printf;

   -------------
   -- Sprintf --
   -------------

   function Sprintf (Template : String;
                     X        : Mpfloat;
                     R        : Rounding := RNDEF) return String is
      Res : int := -1;
      Buf : String (1 .. 256);
      --  TODO: find a way to compute a safe upper bound for Buf. mpfr_sprintf
      --  will overflow if the result is bigger than that.

      Input : chars_ptr := Null_Ptr;
      Tpl   : String := Template;
      Rnd   : Rounding := R;
   begin
      begin
         Reformat_Printf_Args (Tpl, Rnd);
         Input := New_String (Tpl);

         Res := sprintf_stub (Buf'Address,
                              Input,
                              Rounding'Pos (Rnd),
                              X.Value'Access);
         Free (Input);

         if Res > 0 then
            return Buf (1 .. Integer (Res));
         else
            raise Failure with "mpfr_sprintf failure";
         end if;

      exception
         when Storage_Error =>
            raise Failure with "mpfr_sprintf malformation";
      end;
   end Sprintf;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (X : in out Mpfloat) is
   begin
      mpfr_init2 (X.Value'Access, mpfr_prec_t (X.Prec));
   exception
      when Program_Error =>
         raise Failure with "error during Mpfloat (Prec :=" &
           X.Prec'Image & ") initialization";
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Mpfloat) is
   begin
      mpfr_clear (X.Value'Access);
   end Finalize;

   -----------------------
   -- Get_Ternary_Value --
   -----------------------

   function Get_Ternary_Value (X : Mpfloat) return Ternary_Value is
     (X.Ternary);

   ----------------------
   -- To_Ternary_Value --
   ----------------------

   function To_Ternary_Value (T : int) return Ternary_Value is
   begin
      if T > 0 then
         return GREATER;
      elsif T < 0 then
         return LOWER;
      else
         return EXACT;
      end if;
   end To_Ternary_Value;

   ---------
   -- Set --
   ---------

   procedure Set (Rop : out Mpfloat; Op : Mpfloat; Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_set (Rop.Value'Access,
                                    Op.Value'Access,
                                    Rounding'Pos (Rnd)));
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Rop : out Mpfloat;
      Op  : Long_Integer;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_set_si (Rop.Value'Access,
                                       long (Op),
                                       Rounding'Pos (Rnd)));
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Rop : out Mpfloat; Op : Float; Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_set_flt (Rop.Value'Access,
                                        C_float (Op),
                                        Rounding'Pos (Rnd)));
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Rop : out Mpfloat;
      Op  : Long_Float;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_set_d (Rop.Value'Access,
                                      Interfaces.C.double (Op),
                                      Rounding'Pos (Rnd)));
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Rop : out Mpfloat;
      Op  : Long_Long_Float;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_set_ld (Rop.Value'Access,
                                       Interfaces.C.long_double (Op),
                                       Rounding'Pos (Rnd)));
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Rop : out Mpfloat;
      Op  : Long_Integer;
      E   : Exponent;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_set_si_2exp (Rop.Value'Access,
                                            long (Op),
                                            mpfr_exp_t (E),
                                            Rounding'Pos (Rnd)));
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Rop  : out Mpfloat;
      S    : String;
      Base : Admpfr.Base := 10;
      Rnd  : Rounding    := RNDEF)
   is
      Input  : chars_ptr := New_String (S);
      Endptr : aliased chars_ptr := Null_Ptr;
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_strtofr (Rop.Value'Access,
                                        Input,
                                        Endptr'Access,
                                        int (Base),
                                        Rounding'Pos (Rnd)));

      declare
         Invalid_Data : constant String := Value (Endptr);
      begin
         --  Free Input only after having took the Value of Endptr. If not the
         --  memory content pointed by Endptr can sligthly change and contains
         --  corrupted data. This is not clear to me why though.

         Free (Input);

         if Invalid_Data /= "" then
            raise Failure with "invalid data: '" & Invalid_Data &
              "' to set number from string: '" & S & "'";
         end if;
      end;

   end Set;

   -------------
   -- Set_Nan --
   -------------

   procedure Set_Nan (X : out Mpfloat) is
   begin
      mpfr_set_nan (X.Value'Access);
   end Set_Nan;

   -------------
   -- Set_Inf --
   -------------

   procedure Set_Inf (X : out Mpfloat; S : Sign) is
   begin
      mpfr_set_inf (X.Value'Access, S'Enum_Rep);
   end Set_Inf;

   --------------
   -- Set_Zero --
   --------------

   procedure Set_Zero (X : out Mpfloat; S : Sign) is
   begin
      mpfr_set_zero (X.Value'Access, S'Enum_Rep);
   end Set_Zero;

   ----------
   -- Swap --
   ----------

   procedure Swap (X : in out Mpfloat; Y : in out Mpfloat) is
      T : constant Ternary_Value := X.Ternary;
   begin
      mpfr_swap (X.Value'Access, Y.Value'Access);
      X.Ternary := Y.Ternary;
      Y.Ternary := T;
   end Swap;

   ---------------
   -- Get_Float --
   ---------------

   function Get_Float (Op : Mpfloat; Rnd : Rounding := RNDEF) return Float is
   begin
      return Float (mpfr_get_flt (Op.Value'Access, Rounding'Pos (Rnd)));
   exception
      when Constraint_Error =>
         raise Failure with "invalid data, can't convert: " & Op.To_String
            & " to Float";
   end Get_Float;

   --------------------
   -- Get_Long_Float --
   --------------------

   function Get_Long_Float
     (Op  : Mpfloat;
      Rnd : Rounding := RNDEF) return Long_Float is
   begin
      return Long_Float (mpfr_get_d (Op.Value'Access, Rounding'Pos (Rnd)));
   exception
      when Constraint_Error =>
         raise Failure with "invalid data, can't convert: " & Op.To_String
            & " to Long_Float";
   end Get_Long_Float;

   -------------------------
   -- Get_Long_Long_Float --
   -------------------------

   function Get_Long_Long_Float
     (Op  : Mpfloat;
      Rnd : Rounding := RNDEF) return Long_Long_Float is
   begin
      return Long_Long_Float
        (mpfr_get_ld (Op.Value'Access, Rounding'Pos (Rnd)));
   exception
      when Constraint_Error =>
         raise Failure with "invalid data, can't convert: " & Op.To_String
            & " to Long_Long_Float";
   end Get_Long_Long_Float;

   ----------------------
   -- Get_Long_Integer --
   ----------------------

   function Get_Long_Integer
     (Op  : Mpfloat;
      Rnd : Rounding := RNDEF) return Long_Integer is
   begin
      return Long_Integer (mpfr_get_si (Op.Value'Access, Rounding'Pos (Rnd)));
   end Get_Long_Integer;

   --------------------
   -- Get_Long_Float --
   --------------------

   function Get_Long_Float
     (Op  : Mpfloat;
      Exp : out Long_Integer;
      Rnd : Rounding := RNDEF) return Long_Float
   is
      L : aliased long;
   begin
      return LF : constant Long_Float := Long_Float
        (mpfr_get_d_2exp (L'Access,
                          Op.Value'Access,
                          Rounding'Pos (Rnd))) do
         Exp := Long_Integer (L);
      end return;
   exception
      when Constraint_Error =>
         raise Failure with "invalid data, can't convert: " & Op.To_String
            & " to Long_Float";
   end Get_Long_Float;

   -------------------------
   -- Get_Long_Long_Float --
   -------------------------

   function Get_Long_Long_Float
     (Op  : Mpfloat;
      Exp : out Long_Integer;
      Rnd : Rounding := RNDEF) return Long_Long_Float
   is
      L : aliased long;
   begin
      return LLF : constant Long_Long_Float := Long_Long_Float
        (mpfr_get_ld_2exp (L'Access,
                           Op.Value'Access,
                           Rounding'Pos (Rnd))) do
         Exp := Long_Integer (L);
      end return;
   exception
      when Constraint_Error =>
         raise Failure with "invalid data, can't convert: " & Op.To_String
            & " to Long_Long_Float";
   end Get_Long_Long_Float;

   ---------
   -- Set --
   ---------

   procedure Set
     (Rop : out Mpfloat;
      Exp : out Long_Integer;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF)
   is
      L : aliased mpfr_exp_t;
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_frexp (L'Access,
                                      Rop.Value'Access,
                                      Op.Value'Access,
                                      Rounding'Pos (Rnd)));
      Exp := Long_Integer (L);
   end Set;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (X    : Mpfloat;
      Base : Admpfr.Base := 10;
      Rnd  : Rounding    := RNDEF) return String
   is
      --  TODO: Rely on mpfr_get_str_ndigits for now but allows the user to set
      --  the number of digits to print by adding a parameter to this function.

      --  Default behavior mimics mpfr_printf("%.RNe", X),
      --  at least for base 10!

      Number_Digits : constant size_t :=
        size_t'Max (mpfr_get_str_ndigits (abs int (Base),
                                          mpfr_get_prec (X.Value'Access)),
                    7);

      Significand_Buffer : String (1 .. Integer (Number_Digits + 2));
      Exponent           : mpfr_exp_t;

      Exponent_S, Number : Unbounded_String;
      Significand        : chars_ptr;
   begin
      Significand := mpfr_get_str (Significand_Buffer'Address,
                                   Exponent'Address,
                                   int (Base),
                                   Number_Digits,
                                   X.Value'Access,
                                   Rounding'Pos (Rnd));

      --  Special treatment for NaN and inf values

      for I in 1 .. 2 loop
         if Significand_Buffer (I) = '@' then
            return Significand_Buffer (1 .. I + 5);
         end if;
      end loop;

      --  Remove 1 if first digit is not zero as we'll insert the implicit
      --  radix point in the significand.

      if Significand_Buffer (1) /= '-' then
         if Significand_Buffer (1) /= '0' then
            Exponent := Exponent - 1;
         end if;
      else
         if Significand_Buffer (2) /= '0' then
            Exponent := Exponent - 1;
         end if;
      end if;

      --  Convert Exponent to a string

      Exponent_S := To_Unbounded_String (mpfr_exp_t'Image (Exponent));

      --  Format exponent as mpfr_printf("%RNe")

      if Length (Exponent_S) = 2 then
         Insert (Exponent_S, 2, "0");
      end if;

      --  Add '+' sign if exponent is zero or positive.

      if Exponent >= 0 then
         Overwrite (Exponent_S, 1, "+");
      end if;

      --  Concat significand and exponent

      Number := Value (Significand) & "e" & Exponent_S;

      --  Insert the radix point

      Insert (Number, (if Significand_Buffer (1) = '-' then 3 else 2), ".");

      return To_String (Number);
   end To_String;

   --------------------
   --  Mpfloat_Image --
   --------------------

   procedure Mpfloat_Image
     (Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    :        Mpfloat)
   is
   begin
      Buffer.Put (Arg.To_String);
   end Mpfloat_Image;

   -----------------------
   -- Fits_Long_Integer --
   -----------------------

   function Fits_Long_Integer
     (Op  : Mpfloat;
      Rnd : Rounding := RNDEF) return Boolean
   is
      Fits : constant int := mpfr_fits_slong_p (Op.Value'Access,
                                                Rounding'Pos (Rnd));
   begin
      return (if Fits /= 0 then True else False);
   end Fits_Long_Integer;

   ------------------
   -- Fits_Integer --
   ------------------

   function Fits_Integer
     (Op  : Mpfloat;
      Rnd : Rounding := RNDEF) return Boolean
   is
      Fits : constant int := mpfr_fits_sint_p (Op.Value'Access,
                                               Rounding'Pos (Rnd));
   begin
      return (if Fits /= 0 then True else False);
   end Fits_Integer;

   ---------------
   -- Mpfr_Fn_1 --
   ---------------

   procedure Mpfr_Fn_1
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_fn (Rop.Value'Access,
                                   Op.Value'Access,
                                   Rounding'Pos (Rnd)));
   end Mpfr_Fn_1;

   procedure Mpfr_Sqr is new Mpfr_Fn_1 (mpfr_sqr);
   procedure Mpfr_Sqrt is new Mpfr_Fn_1 (mpfr_sqrt);
   procedure Mpfr_Rec_Sqrt is new Mpfr_Fn_1 (mpfr_rec_sqrt);
   procedure Mpfr_Cbrt is new Mpfr_Fn_1 (mpfr_cbrt);
   procedure Mpfr_Neg is new Mpfr_Fn_1 (mpfr_neg);
   procedure Mpfr_Abs is new Mpfr_Fn_1 (mpfr_abs);
   procedure Mpfr_Log is new Mpfr_Fn_1 (mpfr_log);
   procedure Mpfr_Log2 is new Mpfr_Fn_1 (mpfr_log2);
   procedure Mpfr_Log10 is new Mpfr_Fn_1 (mpfr_log10);
   procedure Mpfr_Log1p is new Mpfr_Fn_1 (mpfr_log1p);
   procedure Mpfr_Exp is new Mpfr_Fn_1 (mpfr_exp);
   procedure Mpfr_Exp2 is new Mpfr_Fn_1 (mpfr_exp2);
   procedure Mpfr_Exp10 is new Mpfr_Fn_1 (mpfr_exp10);
   procedure Mpfr_Expm1 is new Mpfr_Fn_1 (mpfr_expm1);
   procedure Mpfr_Cos is new Mpfr_Fn_1 (mpfr_cos);
   procedure Mpfr_Sin is new Mpfr_Fn_1 (mpfr_sin);
   procedure Mpfr_Tan is new Mpfr_Fn_1 (mpfr_tan);
   procedure Mpfr_Sec is new Mpfr_Fn_1 (mpfr_sec);
   procedure Mpfr_Csc is new Mpfr_Fn_1 (mpfr_csc);
   procedure Mpfr_Cot is new Mpfr_Fn_1 (mpfr_cot);
   procedure Mpfr_Acos is new Mpfr_Fn_1 (mpfr_acos);
   procedure Mpfr_Asin is new Mpfr_Fn_1 (mpfr_asin);
   procedure Mpfr_Atan is new Mpfr_Fn_1 (mpfr_atan);
   procedure Mpfr_Cosh is new Mpfr_Fn_1 (mpfr_cosh);
   procedure Mpfr_Sinh is new Mpfr_Fn_1 (mpfr_sinh);
   procedure Mpfr_Tanh is new Mpfr_Fn_1 (mpfr_tanh);
   procedure Mpfr_Sech is new Mpfr_Fn_1 (mpfr_sech);
   procedure Mpfr_Csch is new Mpfr_Fn_1 (mpfr_csch);
   procedure Mpfr_Coth is new Mpfr_Fn_1 (mpfr_coth);
   procedure Mpfr_Acosh is new Mpfr_Fn_1 (mpfr_acosh);
   procedure Mpfr_Asinh is new Mpfr_Fn_1 (mpfr_asinh);
   procedure Mpfr_Atanh is new Mpfr_Fn_1 (mpfr_atanh);
   procedure Mpfr_Eint is new Mpfr_Fn_1 (mpfr_eint);
   procedure Mpfr_Li2 is new Mpfr_Fn_1 (mpfr_li2);
   procedure Mpfr_Gamma is new Mpfr_Fn_1 (mpfr_gamma);
   procedure Mpfr_Lngamma is new Mpfr_Fn_1 (mpfr_lngamma);
   procedure Mpfr_Digamma is new Mpfr_Fn_1 (mpfr_digamma);
   procedure Mpfr_Zeta is new Mpfr_Fn_1 (mpfr_zeta);
   procedure Mpfr_Erf is new Mpfr_Fn_1 (mpfr_erf);
   procedure Mpfr_Erfc is new Mpfr_Fn_1 (mpfr_erfc);
   procedure Mpfr_J0 is new Mpfr_Fn_1 (mpfr_j0);
   procedure Mpfr_J1 is new Mpfr_Fn_1 (mpfr_j1);
   procedure Mpfr_Y0 is new Mpfr_Fn_1 (mpfr_y0);
   procedure Mpfr_Y1 is new Mpfr_Fn_1 (mpfr_y1);
   procedure Mpfr_Ai is new Mpfr_Fn_1 (mpfr_ai);
   procedure Mpfr_Rint is new Mpfr_Fn_1 (mpfr_rint);
   procedure Mpfr_Rint_Ceil is new Mpfr_Fn_1 (mpfr_rint_ceil);
   procedure Mpfr_Rint_Floor is new Mpfr_Fn_1 (mpfr_rint_floor);
   procedure Mpfr_Rint_Round is new Mpfr_Fn_1 (mpfr_rint_round);
   procedure Mpfr_Rint_Roundeven is new Mpfr_Fn_1 (mpfr_rint_roundeven);
   procedure Mpfr_Rint_Trunc is new Mpfr_Fn_1 (mpfr_rint_trunc);
   procedure Mpfr_Frac is new Mpfr_Fn_1 (mpfr_frac);

   ---------------
   -- Mpfr_Fn_2 --
   ---------------

   procedure Mpfr_Fn_2
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_fn (Rop.Value'Access,
                                   Op1.Value'Access,
                                   Op2.Value'Access,
                                   Rounding'Pos (Rnd)));
   end Mpfr_Fn_2;

   procedure Mpfr_Add is new Mpfr_Fn_2 (mpfr_add);
   procedure Mpfr_Sub is new Mpfr_Fn_2 (mpfr_sub);
   procedure Mpfr_Mul is new Mpfr_Fn_2 (mpfr_mul);
   procedure Mpfr_Div is new Mpfr_Fn_2 (mpfr_div);
   procedure Mpfr_Dim is new Mpfr_Fn_2 (mpfr_dim);
   procedure Mpfr_Hypot is new Mpfr_Fn_2 (mpfr_hypot);
   procedure Mpfr_Pow is new Mpfr_Fn_2 (mpfr_pow);
   procedure Mpfr_Atan2 is new Mpfr_Fn_2 (mpfr_atan2);
   procedure Mpfr_Gamma_Inc is new Mpfr_Fn_2 (mpfr_gamma_inc);
   procedure Mpfr_Beta is new Mpfr_Fn_2 (mpfr_beta);
   procedure Mpfr_Agm is new Mpfr_Fn_2 (mpfr_agm);

   -----------------
   -- Mpfr_Fn_2_I --
   -----------------

   procedure Mpfr_Fn_2_I
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_fn (Rop.Value'Access,
                                   Op1.Value'Access,
                                   long (Op2),
                                   Rounding'Pos (Rnd)));
   end Mpfr_Fn_2_I;

   procedure Mpfr_Add is new Mpfr_Fn_2_I (mpfr_add_si);
   procedure Mpfr_Sub is new Mpfr_Fn_2_I (mpfr_sub_si);
   procedure Mpfr_Mul is new Mpfr_Fn_2_I (mpfr_mul_si);
   procedure Mpfr_Div is new Mpfr_Fn_2_I (mpfr_div_si);
   procedure Mpfr_Mul_2 is new Mpfr_Fn_2_I (mpfr_mul_2si);
   procedure Mpfr_Div_2 is new Mpfr_Fn_2_I (mpfr_div_2si);

   ------------------
   -- Mpfr_Fn_2_Ib --
   ------------------

   procedure Mpfr_Fn_2_Ib
     (Rop : in out Mpfloat;
      Op1 : Long_Integer;
      Op2 : Mpfloat;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_fn (Rop.Value'Access,
                                   long (Op1),
                                   Op2.Value'Access,
                                   Rounding'Pos (Rnd)));
   end Mpfr_Fn_2_Ib;

   procedure Mpfr_Sub is new Mpfr_Fn_2_Ib (mpfr_si_sub);
   procedure Mpfr_Div is new Mpfr_Fn_2_Ib (mpfr_si_div);

   -----------------
   -- Mpfr_Fn_2_F --
   -----------------

   procedure Mpfr_Fn_2_F
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Float;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_fn (Rop.Value'Access,
                                   Op1.Value'Access,
                                   double (Op2),
                                   Rounding'Pos (Rnd)));
   end Mpfr_Fn_2_F;

   procedure Mpfr_Add is new Mpfr_Fn_2_F (mpfr_add_d);
   procedure Mpfr_Sub is new Mpfr_Fn_2_F (mpfr_sub_d);
   procedure Mpfr_Mul is new Mpfr_Fn_2_F (mpfr_mul_d);
   procedure Mpfr_Div is new Mpfr_Fn_2_F (mpfr_div_d);

   ------------------
   -- Mpfr_Fn_2_Fb --
   ------------------

   procedure Mpfr_Fn_2_Fb
     (Rop : in out Mpfloat;
      Op1 : Long_Float;
      Op2 : Mpfloat;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_fn (Rop.Value'Access,
                                   double (Op1),
                                   Op2.Value'Access,
                                   Rounding'Pos (Rnd)));
   end Mpfr_Fn_2_Fb;

   procedure Mpfr_Sub is new Mpfr_Fn_2_Fb (mpfr_d_sub);
   procedure Mpfr_Div is new Mpfr_Fn_2_Fb (mpfr_d_div);

   ---------
   -- Add --
   ---------

   procedure Add
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF) renames Mpfr_Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF) renames Mpfr_Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Float;
      Rnd : Rounding := RNDEF) renames Mpfr_Add;

   ---------
   -- Sub --
   ---------

   procedure Sub
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF) renames Mpfr_Sub;

   ---------
   -- Sub --
   ---------

   procedure Sub
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF) renames Mpfr_Sub;

   ---------
   -- Sub --
   ---------

   procedure Sub
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Float;
      Rnd : Rounding := RNDEF) renames Mpfr_Sub;

   ---------
   -- Sub --
   ---------

   procedure Sub
     (Rop : in out Mpfloat;
      Op1 : Long_Integer;
      Op2 : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Sub;

   ---------
   -- Sub --
   ---------

   procedure Sub
     (Rop : in out Mpfloat;
      Op1 : Long_Float;
      Op2 : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Sub;

   ---------
   -- Mul --
   ---------

   procedure Mul
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF) renames Mpfr_Mul;

   ---------
   -- Mul --
   ---------

   procedure Mul
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF) renames Mpfr_Mul;

   ---------
   -- Mul --
   ---------

   procedure Mul
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Float;
      Rnd : Rounding := RNDEF) renames Mpfr_Mul;

   ---------
   -- Sqr --
   ---------

   procedure Sqr
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Sqr;

   ---------
   -- Div --
   ---------

   procedure Div
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF) renames Mpfr_Div;

   ---------
   -- Div --
   ---------

   procedure Div
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF) renames Mpfr_Div;

   ---------
   -- Div --
   ---------

   procedure Div
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Float;
      Rnd : Rounding := RNDEF) renames Mpfr_Div;

   ---------
   -- Div --
   ---------

   procedure Div
     (Rop : in out Mpfloat;
      Op1 : Long_Integer;
      Op2 : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Div;

   ---------
   -- Div --
   ---------

   procedure Div
     (Rop : in out Mpfloat;
      Op1 : Long_Float;
      Op2 : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Div;

   ----------
   -- Sqrt --
   ----------

   procedure Sqrt
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Sqrt;

   ----------
   -- Sqrt --
   ----------

   procedure Sqrt
     (Rop : in out Mpfloat;
      Op  : Long_Integer;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_sqrt_ui (Rop.Value'Access,
                                        unsigned_long (Op),
                                        Rounding'Pos (Rnd)));
   end Sqrt;

   --------------
   -- Rec_Sqrt --
   --------------

   procedure Rec_Sqrt
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Rec_Sqrt;

   ----------
   -- Cbrt --
   ----------

   procedure Cbrt
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Cbrt;

   -----------
   -- Rootn --
   -----------

   procedure Rootn
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      N   : Long_Integer;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_rootn_ui (Rop.Value'Access,
                                         Op.Value'Access,
                                         unsigned_long (N),
                                         Rounding'Pos (Rnd)));
   end Rootn;

   ---------
   -- Neg --
   ---------

   procedure Neg
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Neg;

   --------------
   -- Absolute --
   --------------

   procedure Absolute
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Abs;

   ---------
   -- Dim --
   ---------

   procedure Dim
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF) renames Mpfr_Dim;

   -----------
   -- Mul_2 --
   -----------

   procedure Mul_2
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF) renames Mpfr_Mul_2;

   -----------
   -- Div_2 --
   -----------

   procedure Div_2
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF) renames Mpfr_Div_2;

   ---------
   -- Fac --
   ---------

   procedure Fac
     (Rop : in out Mpfloat;
      Op  : Long_Integer;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_fac_ui (Rop.Value'Access,
                                       unsigned_long (Op),
                                       Rounding'Pos (Rnd)));
   end Fac;

   ---------
   -- Fma --
   ---------

   procedure Fma
     (Rop           : in out Mpfloat;
      Op1, Op2, Op3 : Mpfloat;
      Rnd           : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_fma (Rop.Value'Access,
                                    Op1.Value'Access,
                                    Op2.Value'Access,
                                    Op3.Value'Access,
                                    Rounding'Pos (Rnd)));
   end Fma;

   ---------
   -- Fms --
   ---------

   procedure Fms
     (Rop           : in out Mpfloat;
      Op1, Op2, Op3 : Mpfloat;
      Rnd           : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_fms (Rop.Value'Access,
                                    Op1.Value'Access,
                                    Op2.Value'Access,
                                    Op3.Value'Access,
                                    Rounding'Pos (Rnd)));
   end Fms;

   ----------
   -- Fmma --
   ----------

   procedure Fmma
     (Rop                : in out Mpfloat;
      Op1, Op2, Op3, Op4 : Mpfloat;
      Rnd                : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_fmma (Rop.Value'Access,
                                     Op1.Value'Access,
                                     Op2.Value'Access,
                                     Op3.Value'Access,
                                     Op4.Value'Access,
                                     Rounding'Pos (Rnd)));
   end Fmma;

   ----------
   -- Fmms --
   ----------

   procedure Fmms
     (Rop                : in out Mpfloat;
      Op1, Op2, Op3, Op4 : Mpfloat;
      Rnd                : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_fmms (Rop.Value'Access,
                                     Op1.Value'Access,
                                     Op2.Value'Access,
                                     Op3.Value'Access,
                                     Op4.Value'Access,
                                     Rounding'Pos (Rnd)));
   end Fmms;

   -----------
   -- Hypot --
   -----------

   procedure Hypot
     (Rop  : in out Mpfloat;
      X, Y : Mpfloat;
      Rnd  : Rounding := RNDEF) renames Mpfr_Hypot;

   ---------
   -- Sum --
   ---------

   procedure Sum
     (Rop : in out Mpfloat;
      Arr : Mpfloat_Array;
      Rnd : Rounding := RNDEF)
   is
      N : constant Integer := Arr'Length;

      type mpfr_ptr_array is array (Integer range 1 .. N)
        of access constant mpfr_t with Convention => C;
      Tab : mpfr_ptr_array;
   begin
      for I in 1 .. N loop
         Tab (I) := Arr (I).Value'Access;
      end loop;

      Rop.Ternary :=
        To_Ternary_Value (mpfr_sum (Rop.Value'Access,
                                    Tab'Address,
                                    unsigned_long (N),
                                    Rounding'Pos (Rnd)));
   end Sum;

   ---------
   -- Dot --
   ---------

   procedure Dot
     (Rop  : in out Mpfloat;
      Arr1 : Mpfloat_Array;
      Arr2 : Mpfloat_Array;
      Rnd  : Rounding := RNDEF)
   is
      N : constant Integer :=
         (if Arr1'Length <= Arr2'Length then Arr1'Length else Arr2'Length);

      type mpfr_ptr_array is array (Integer range 1 .. N)
        of access constant mpfr_t with Convention => C;
      A, B : mpfr_ptr_array;
   begin
      for I in 1 .. N loop
         A (I) := Arr1 (I).Value'Access;
         B (I) := Arr2 (I).Value'Access;
      end loop;

      Rop.Ternary :=
        To_Ternary_Value (mpfr_dot (Rop.Value'Access,
                                    A'Address,
                                    B'Address,
                                    unsigned_long (N),
                                    Rounding'Pos (Rnd)));
   end Dot;

   ----------------
   -- To_Compare --
   ----------------

   function To_Compare (C : int) return Compare is
   begin
      if C > 0 then
         return Greater;
      elsif C < 0 then
         return Less;
      else
         return Equal;
      end if;
   end To_Compare;

   ---------
   -- Cmp --
   ---------

   function Cmp (Op1, Op2 : Mpfloat) return Compare is
     (To_Compare (mpfr_cmp (Op1.Value'Access, Op2.Value'Access)));

   ---------
   -- Cmp --
   ---------

   function Cmp (Op1 : Mpfloat; Op2 : Long_Integer) return Compare is
   begin
      return C : Compare do
         if Op2 = 0 then
            C := (To_Compare (mpfr_sgn (Op1.Value'Access)));
         else
            C := (To_Compare (mpfr_cmp_si (Op1.Value'Access, long (Op2))));
         end if;
      end return;
   end Cmp;

   ---------
   -- Cmp --
   ---------

   function Cmp (Op1 : Mpfloat; Op2 : Long_Float) return Compare is
     (To_Compare (mpfr_cmp_d (Op1.Value'Access, double (Op2))));

   ---------
   -- Cmp --
   ---------

   function Cmp (Op1 : Mpfloat; Op2 : Long_Long_Float) return Compare is
     (To_Compare (mpfr_cmp_ld (Op1.Value'Access, long_double (Op2))));

   ---------
   -- Cmp --
   ---------

   function Cmp
     (Op1 : Mpfloat;
      Op2 : Long_Integer;
      E   : Exponent) return Compare is
     (To_Compare
        (mpfr_cmp_si_2exp (Op1.Value'Access, long (Op2), mpfr_exp_t (E))));

   -------------
   -- Cmp_Abs --
   -------------

   function Cmp_Abs (Op1, Op2 : Mpfloat) return Compare is
     (To_Compare (mpfr_cmpabs (Op1.Value'Access, Op2.Value'Access)));

   -------------
   -- Cmp_Abs --
   -------------

   function Cmp_Abs (Op1 : Mpfloat; Op2 : Long_Integer) return Compare is
     (To_Compare
        (mpfr_cmpabs_ui (Op1.Value'Access, (unsigned_long (abs Op2)))));

   ------------
   -- Is_Nan --
   ------------

   function Is_Nan (Op : Mpfloat) return Boolean is
     (if mpfr_nan_p (Op.Value'Access) /= 0 then True else False);

   ------------
   -- Is_Inf --
   ------------

   function Is_Inf (Op : Mpfloat) return Boolean is
     (if mpfr_inf_p (Op.Value'Access) /= 0 then True else False);

   ---------------
   -- Is_Number --
   ---------------

   function Is_Number (Op : Mpfloat) return Boolean is
     (if mpfr_number_p (Op.Value'Access) /= 0 then True else False);

   -------------
   -- Is_Zero --
   -------------

   function Is_Zero (Op : Mpfloat) return Boolean is
     (if mpfr_zero_p (Op.Value'Access) /= 0 then True else False);

   ----------------
   -- Is_Regular --
   ----------------

   function Is_Regular (Op : Mpfloat) return Boolean is
     (if mpfr_regular_p (Op.Value'Access) /= 0 then True else False);

   -------------
   -- Greater --
   -------------

   function Greater (Op1, Op2 : Mpfloat) return Boolean is
     (if mpfr_greater_p (Op1.Value'Access, Op2.Value'Access) /= 0
      then True else False);

   ------------------
   -- Greaterequal --
   ------------------

   function Greaterequal (Op1, Op2 : Mpfloat) return Boolean is
     (if mpfr_greaterequal_p (Op1.Value'Access, Op2.Value'Access) /= 0
      then True else False);

   ----------
   -- Less --
   ----------

   function Less (Op1, Op2 : Mpfloat) return Boolean is
     (if mpfr_less_p (Op1.Value'Access, Op2.Value'Access) /= 0
      then True else False);

   ---------------
   -- Lessequal --
   ---------------

   function Lessequal (Op1, Op2 : Mpfloat) return Boolean is
     (if mpfr_lessequal_p (Op1.Value'Access, Op2.Value'Access) /= 0
      then True else False);

   -----------
   -- Equal --
   -----------

   function Equal (Op1, Op2 : Mpfloat) return Boolean is
     (if mpfr_equal_p (Op1.Value'Access, Op2.Value'Access) /= 0
      then True else False);

   -----------------
   -- Lessgreater --
   -----------------

   function Lessgreater (Op1, Op2 : Mpfloat) return Boolean is
     (if mpfr_lessgreater_p (Op1.Value'Access, Op2.Value'Access) /= 0
      then True else False);

   ---------------
   -- Unordered --
   ---------------

   function Unordered (Op1, Op2 : Mpfloat) return Boolean is
     (if mpfr_unordered_p (Op1.Value'Access, Op2.Value'Access) /= 0
      then True else False);

   -----------------
   -- Total_Order --
   -----------------

   function Total_Order (Op1, Op2 : Mpfloat) return Boolean is
     (if mpfr_total_order_p (Op1.Value'Access, Op2.Value'Access) /= 0
      then True else False);

   ---------
   -- Log --
   ---------

   procedure Log
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Log;

   ---------
   -- Log --
   ---------

   procedure Log
     (Rop : in out Mpfloat;
      Op  : Long_Integer;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_log_ui (Rop.Value'Access,
                                       unsigned_long (Op),
                                       Rounding'Pos (Rnd)));
   end Log;

   ----------
   -- Log2 --
   ----------

   procedure Log2
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Log2;

   -----------
   -- Log10 --
   -----------

   procedure Log10
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Log10;

   -----------
   -- Log1p --
   -----------

   procedure Log1p
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Log1p;

   ---------
   -- Exp --
   ---------

   procedure Exp
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Exp;

   ----------
   -- Exp2 --
   ----------

   procedure Exp2
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Exp2;

   -----------
   -- Exp10 --
   -----------

   procedure Exp10
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Exp10;

   -----------
   -- Expm1 --
   -----------

   procedure Expm1
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Expm1;

   ---------
   -- Pow --
   ---------

   procedure Pow
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF) renames Mpfr_Pow;

   ---------
   -- Pow --
   ---------

   procedure Pow
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_pow_ui (Rop.Value'Access,
                                       Op1.Value'Access,
                                       unsigned_long (Op2),
                                       Rounding'Pos (Rnd)));
   end Pow;

   ---------
   -- Pow --
   ---------

   procedure Pow
     (Rop      : in out Mpfloat;
      Op1, Op2 : Long_Integer;
      Rnd      : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_ui_pow_ui (Rop.Value'Access,
                                          unsigned_long (Op1),
                                          unsigned_long (Op2),
                                          Rounding'Pos (Rnd)));
   end Pow;

   ---------
   -- Pow --
   ---------

   procedure Pow
     (Rop : in out Mpfloat;
      Op1 : Long_Integer;
      Op2 : Mpfloat;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_ui_pow (Rop.Value'Access,
                                       unsigned_long (Op1),
                                       Op2.Value'Access,
                                       Rounding'Pos (Rnd)));
   end Pow;

   ---------
   -- Cos --
   ---------

   procedure Cos
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Cos;

   ---------
   -- Sin --
   ---------

   procedure Sin
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Sin;

   ---------
   -- Tan --
   ---------

   procedure Tan
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Tan;

   -------------
   -- Sin_Cos --
   -------------

   procedure Sin_Cos
     (Sop, Cop : in out Mpfloat;
      Op       : Mpfloat;
      Rnd      : Rounding := RNDEF)
   is
      Ret, Sop_T, Cop_T : int;
      type mpfr_t_a is access all mpfr_t;
   begin
      if mpfr_t_a'(Sop.Value'Access) = Cop.Value'Access then
         raise Failure with "Sop and Cop must be different variables";
      end if;

      Ret := mpfr_sin_cos (Sop.Value'Access,
                           Cop.Value'Access,
                           Op.Value'Access,
                           Rounding'Pos (Rnd));

      Sop_T := Ret mod 4;
      Cop_T := Ret / 4;

      if Sop_T = 2 then
         Sop_T := -Sop_T;
      end if;

      if Cop_T = 2 then
         Cop_T := -Cop_T;
      end if;

      Sop.Ternary := To_Ternary_Value (Sop_T);
      Cop.Ternary := To_Ternary_Value (Cop_T);
   end Sin_Cos;

   ---------
   -- Sec --
   ---------

   procedure Sec
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Sec;

   ---------
   -- Csc --
   ---------

   procedure Csc
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Csc;

   ---------
   -- Cot --
   ---------

   procedure Cot
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Cot;

   ----------
   -- Acos --
   ----------

   procedure Acos
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Acos;

   ----------
   -- Asin --
   ----------

   procedure Asin
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Asin;

   ----------
   -- Atan --
   ----------

   procedure Atan
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Atan;

   -----------
   -- Atan2 --
   -----------

   procedure Atan2
     (Rop   : in out Mpfloat;
      X, Y  : Mpfloat;
      Rnd   : Rounding := RNDEF) renames Mpfr_Atan2;

   ----------
   -- Cosh --
   ----------

   procedure Cosh
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Cosh;

   ----------
   -- Sinh --
   ----------

   procedure Sinh
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Sinh;

   ----------
   -- Tanh --
   ----------

   procedure Tanh
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Tanh;

   ---------------
   -- Sinh_Cosh --
   ---------------

   procedure Sinh_Cosh
     (Sop, Cop : in out Mpfloat;
      Op       : Mpfloat;
      Rnd      : Rounding := RNDEF)
   is
      Ret, Sop_T, Cop_T : int;
      type mpfr_t_a is access all mpfr_t;
   begin
      if mpfr_t_a'(Sop.Value'Access) = Cop.Value'Access then
         raise Failure with "Sop and Cop must be different variables";
      end if;

      Ret := mpfr_sinh_cosh (Sop.Value'Access,
                             Cop.Value'Access,
                             Op.Value'Access,
                             Rounding'Pos (Rnd));

      Sop_T := Ret mod 4;
      Cop_T := Ret / 4;

      if Sop_T = 2 then
         Sop_T := -Sop_T;
      end if;

      if Cop_T = 2 then
         Cop_T := -Cop_T;
      end if;

      Sop.Ternary := To_Ternary_Value (Sop_T);
      Cop.Ternary := To_Ternary_Value (Cop_T);
   end Sinh_Cosh;

   ----------
   -- Sech --
   ----------

   procedure Sech
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Sech;

   ----------
   -- Csch --
   ----------

   procedure Csch
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Csch;

   ----------
   -- Coth --
   ----------

   procedure Coth
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Coth;

   -----------
   -- Acosh --
   -----------

   procedure Acosh
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Acosh;

   -----------
   -- Asinh --
   -----------

   procedure Asinh
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Asinh;

   -----------
   -- Atanh --
   -----------

   procedure Atanh
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Atanh;

   ----------
   -- Eint --
   ----------

   procedure Eint
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Eint;

   ---------
   -- Li2 --
   ---------

   procedure Li2
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Li2;

   -----------
   -- Gamma --
   -----------

   procedure Gamma
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Gamma;

   ---------------
   -- Gamma_Inc --
   ---------------

   procedure Gamma_Inc
     (Rop     : in out Mpfloat;
      Op, Op2 : Mpfloat;
      Rnd     : Rounding := RNDEF) renames Mpfr_Gamma_Inc;

   -------------
   -- Lngamma --
   -------------

   procedure Lngamma
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Lngamma;

   ------------
   -- Lgamma --
   ------------

   procedure Lgamma
     (Rop   : in out Mpfloat;
      Signp : in out Sign;
      Op    : Mpfloat;
      Rnd   : Rounding := RNDEF)
   is
      S : aliased int;
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_lgamma (Rop.Value'Access,
                                       S'Access,
                                       Op.Value'Access,
                                       Rounding'Pos (Rnd)));
      if S > 0 then
         Signp := Pos;
      else
         Signp := Neg;
      end if;
   end Lgamma;

   -------------
   -- Digamma --
   -------------

   procedure Digamma
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Digamma;

   ----------
   -- Beta --
   ----------

   procedure Beta
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF) renames Mpfr_Beta;

   ----------
   -- Zeta --
   ----------

   procedure Zeta
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Zeta;

   ----------
   -- Zeta --
   ----------

   procedure Zeta
     (Rop : in out Mpfloat;
      Op  : Long_Integer;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_zeta_ui (Rop.Value'Access,
                                        unsigned_long (Op),
                                        Rounding'Pos (Rnd)));
   end Zeta;

   ---------
   -- Erf --
   ---------

   procedure Erf
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Erf;

   ----------
   -- Erfc --
   ----------

   procedure Erfc
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Erfc;

   --------
   -- J0 --
   --------

   procedure J0
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_J0;

   --------
   -- J1 --
   --------

   procedure J1
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_J1;

   --------
   -- Jn --
   --------

   procedure Jn
     (Rop : in out Mpfloat;
      N   : Long_Integer;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_jn (Rop.Value'Access,
                                   long (N),
                                   Op.Value'Access,
                                   Rounding'Pos (Rnd)));
   end Jn;

   --------
   -- Y0 --
   --------

   procedure Y0
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Y0;

   --------
   -- Y1 --
   --------

   procedure Y1
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Y1;

   --------
   -- Yn --
   --------

   procedure Yn
     (Rop : in out Mpfloat;
      N   : Long_Integer;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_yn (Rop.Value'Access,
                                   long (N),
                                   Op.Value'Access,
                                   Rounding'Pos (Rnd)));
   end Yn;

   ---------
   -- Agm --
   ---------

   procedure Agm
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF) renames Mpfr_Agm;

   --------
   -- Ai --
   --------

   procedure Ai
     (Rop : in out Mpfloat;
      X   : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Ai;

   ----------------
   -- Const_LogZ --
   ----------------

   procedure Const_Log2
     (Rop : in out Mpfloat;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_const_log2 (Rop.Value'Access,
                                           Rounding'Pos (Rnd)));
   end Const_Log2;

   --------------
   -- Const_Pi --
   --------------

   procedure Const_Pi
     (Rop : in out Mpfloat;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_const_pi (Rop.Value'Access,
                                         Rounding'Pos (Rnd)));
   end Const_Pi;

   -----------------
   -- Const_Euler --
   -----------------

   procedure Const_Euler
     (Rop : in out Mpfloat;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_const_euler (Rop.Value'Access,
                                            Rounding'Pos (Rnd)));
   end Const_Euler;

   -------------------
   -- Const_Catalan --
   -------------------

   procedure Const_Catalan
     (Rop : in out Mpfloat;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_const_catalan (Rop.Value'Access,
                                              Rounding'Pos (Rnd)));
   end Const_Catalan;

   -------------
   -- Out_Str --
   -------------

   procedure Out_Str
     (Stream : File_Type;
      Op     : Mpfloat;
      Base   : Admpfr.Base := 10;
      Rnd    : Rounding := RNDEF)
   is
      N : constant size_t :=  mpfr_out_str (C_Streams.C_Stream (Stream),
                                            int (Base),
                                            0,
                                            Op.Value'Access,
                                            Rounding'Pos (Rnd));
   begin
      if N = 0 then
         raise Failure with "error with mpfr_out_str";
      end if;
   end Out_Str;

   -------------
   -- Inp_Str --
   -------------

   procedure Inp_Str
     (Op     : in out Mpfloat;
      Stream : File_Type;
      Base   : Admpfr.Base := 10;
      Rnd    : Rounding := RNDEF)
   is
      N : constant size_t := mpfr_inp_str (Op.Value'Access,
                                           C_Streams.C_Stream (Stream),
                                           int (abs (Base)),
                                           Rounding'Pos (Rnd));
   begin
      Op.Ternary := NOT_SET;
      if N = 0 then
         raise Failure with "error with mpfr_inp_str";
      end if;
   end Inp_Str;

   -----------------
   -- Fpif_Export --
   -----------------

   procedure Fpif_Export (Stream : File_Type; Op : Mpfloat)
   is
      N : constant int := mpfr_fpif_export (C_Streams.C_Stream (Stream),
                                            Op.Value'Access);
   begin
      if N /= 0 then
         raise Failure with "error with mpfr_fpif_export";
      end if;
   end Fpif_Export;

   -----------------
   -- Fpif_Import --
   -----------------

   procedure Fpif_Import (Op : in out Mpfloat; Stream : File_Type)
   is
      N : constant int := mpfr_fpif_import (Op.Value'Access,
                                            C_Streams.C_Stream (Stream));
   begin
      Op.Ternary := NOT_SET;
      if N /= 0 then
         raise Failure with "error with mpfr_fpif_import";
      end if;
   end Fpif_Import;

   ----------
   -- Dump --
   ----------

   procedure Dump (Op : Mpfloat) is
   begin
      mpfr_dump (Op.Value'Access);
   end Dump;

   ----------
   -- Rint --
   ----------

   procedure Rint
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Rint;

   ----------
   -- Ceil --
   ----------

   procedure Ceil (Rop : in out Mpfloat; Op : Mpfloat) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_ceil (Rop.Value'Access,
                                     Op.Value'Access));
   end Ceil;

   -----------
   -- Floor --
   -----------

   procedure Floor (Rop : in out Mpfloat; Op : Mpfloat) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_floor (Rop.Value'Access,
                                      Op.Value'Access));
   end Floor;

   -----------
   -- Round --
   -----------

   procedure Round (Rop : in out Mpfloat; Op : Mpfloat) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_round (Rop.Value'Access,
                                      Op.Value'Access));
   end Round;

   ---------------
   -- Roundeven --
   ---------------

   procedure Roundeven (Rop : in out Mpfloat; Op : Mpfloat) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_roundeven (Rop.Value'Access,
                                          Op.Value'Access));
   end Roundeven;

   -----------
   -- Trunc --
   -----------

   procedure Trunc (Rop : in out Mpfloat; Op : Mpfloat) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_trunc (Rop.Value'Access,
                                      Op.Value'Access));
   end Trunc;

   ----------------
   -- Rint_Floor --
   ----------------

   procedure Rint_Ceil
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Rint_Ceil;

   ----------------
   -- Rint_Floor --
   ----------------

   procedure Rint_Floor
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Rint_Floor;

   ----------------
   -- Rint_Round --
   ----------------

   procedure Rint_Round
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Rint_Round;

   --------------------
   -- Rint_Roundeven --
   --------------------

   procedure Rint_Roundeven
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Rint_Roundeven;

   ----------------
   -- Rint_Trunc --
   ----------------

   procedure Rint_Trunc
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Rint_Trunc;

   ----------
   -- Frac --
   ----------

   procedure Frac
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF) renames Mpfr_Frac;

   ----------
   -- Modf --
   ----------

   procedure Modf
     (Iop, Fop : in out Mpfloat;
      Op       : Mpfloat;
      Rnd      : Rounding := RNDEF)
   is
      Ret, Iop_T, Fop_T : int;
      type mpfr_t_a is access all mpfr_t;
   begin
      if mpfr_t_a'(Iop.Value'Access) = Fop.Value'Access then
         raise Failure with "Iop and Fop must be different variables";
      end if;

      Ret := mpfr_modf (Iop.Value'Access,
                        Fop.Value'Access,
                        Op.Value'Access,
                        Rounding'Pos (Rnd));

      Iop_T := Ret mod 4;
      Fop_T := Ret / 4;

      if Iop_T = 2 then
         Iop_T := -Iop_T;
      end if;

      if Fop_T = 2 then
         Fop_T := -Fop_T;
      end if;

      Iop.Ternary := To_Ternary_Value (Iop_T);
      Fop.Ternary := To_Ternary_Value (Fop_T);
   end Modf;

   ----------
   -- Fmod --
   ----------

   procedure Fmod
     (R    : in out Mpfloat;
      X, Y : Mpfloat;
      Rnd  : Rounding := RNDEF) is
   begin
      R.Ternary :=
        To_Ternary_Value (mpfr_fmod (R.Value'Access,
                                     X.Value'Access,
                                     Y.Value'Access,
                                     Rounding'Pos (Rnd)));
   end Fmod;

   -------------
   -- Fmodquo --
   -------------

   procedure Fmodquo
     (R    : in out Mpfloat;
      Q    : in out Long_Integer;
      X, Y : Mpfloat;
      Rnd  : Rounding := RNDEF)
   is
      lq : aliased long;
   begin
      R.Ternary :=
        To_Ternary_Value (mpfr_fmodquo (R.Value'Access,
                                        lq'Access,
                                        X.Value'Access,
                                        Y.Value'Access,
                                        Rounding'Pos (Rnd)));
      Q := Long_Integer (lq);
   end Fmodquo;

   ---------------
   -- Remainder --
   ---------------

   procedure Remainder
     (R    : in out Mpfloat;
      X, Y : Mpfloat;
      Rnd  : Rounding := RNDEF) is
   begin
      R.Ternary :=
        To_Ternary_Value (mpfr_remainder (R.Value'Access,
                                          X.Value'Access,
                                          Y.Value'Access,
                                          Rounding'Pos (Rnd)));
   end Remainder;

   ------------
   -- Remquo --
   ------------

   procedure Remquo
     (R    : in out Mpfloat;
      Q    : in out Long_Integer;
      X, Y : Mpfloat;
      Rnd  : Rounding := RNDEF)
   is
      lq : aliased long;
   begin
      R.Ternary :=
        To_Ternary_Value (mpfr_remquo (R.Value'Access,
                                       lq'Access,
                                       X.Value'Access,
                                       Y.Value'Access,
                                       Rounding'Pos (Rnd)));
      Q := Long_Integer (lq);
   end Remquo;

   ----------------
   -- Is_Integer --
   ----------------

   function Is_Integer (Op : Mpfloat) return Boolean is
     (if mpfr_integer_p (Op.Value'Access) /= 0 then True else False);

   -------------------------------
   -- Set_Default_Rounding_Mode --
   -------------------------------

   procedure Set_Default_Rounding_Mode (Rnd : Rounding) is
   begin
      mpfr_set_default_rounding_mode (Rounding'Pos (Rnd));
   end Set_Default_Rounding_Mode;

   -------------------------------
   -- Get_Default_Rounding_Mode --
   -------------------------------

   function Get_Default_Rounding_Mode return Rounding is
     (Rounding'Enum_Val (mpfr_get_default_rounding_mode));

   ----------------
   -- Prec_Round --
   ----------------

   procedure Prec_Round
     (X    : in out Mpfloat;
      Prec : Precision;
      Rnd  : Rounding := RNDEF) is
   begin
      X.Ternary :=
        To_Ternary_Value (mpfr_prec_round (X.Value'Access,
                                           mpfr_prec_t (Prec),
                                           Rounding'Pos (Rnd)));
   end Prec_Round;

   ---------------
   -- Can_Round --
   ---------------

   function Can_Round
     (B          : Mpfloat;
      Err        : in out Exponent;
      Rnd1, Rnd2 : Rounding;
      Prec       : Precision) return Boolean
   is
      R : constant int := mpfr_can_round (B.Value'Access,
                                          mpfr_exp_t (Err),
                                          Rounding'Pos (Rnd1),
                                          Rounding'Pos (Rnd2),
                                          mpfr_prec_t (Prec));
   begin
      if R /= 0 then
         return True;
      else
         return False;
      end if;
   end Can_Round;

   --------------
   -- Min_Prec --
   --------------

   function Min_Prec (X : Mpfloat) return Precision is
      P : constant mpfr_prec_t := mpfr_min_prec (X.Value'Access);
   begin
      if P = 0 then
         raise Empty_Prec with "Min_Prec is 0";
      end if;

      return Precision (P);
   end Min_Prec;

   ----------------
   -- Nexttoward --
   ----------------

   procedure Nexttoward (X : in out Mpfloat; Y : Mpfloat) is
   begin
      mpfr_nexttoward (X.Value'Access, Y.Value'Access);
   end Nexttoward;

   ---------------
   -- Nextabove --
   ---------------

   procedure Nextabove (X : in out Mpfloat) is
   begin
      mpfr_nextabove (X.Value'Access);
   end Nextabove;

   ---------------
   -- Nextbelow --
   ---------------

   procedure Nextbelow (X : in out Mpfloat) is
   begin
      mpfr_nextbelow (X.Value'Access);
   end Nextbelow;

   ---------
   -- Min --
   ---------

   procedure Min
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_min (Rop.Value'Access,
                                    Op1.Value'Access,
                                    Op2.Value'Access,
                                    Rounding'Pos (Rnd)));
   end Min;

   ---------
   -- Max --
   ---------

   procedure Max
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_max (Rop.Value'Access,
                                    Op1.Value'Access,
                                    Op2.Value'Access,
                                    Rounding'Pos (Rnd)));
   end Max;

   -------------
   -- Get_Exp --
   -------------

   function Get_Exp (X : Mpfloat) return Exponent is
     (Exponent (mpfr_get_exp (X.Value'Access)));
   --  TODO: raise an exception if X is NaN, Inf, or Zero.

   -------------
   -- Set_Exp --
   -------------

   procedure Set_Exp (X : in out Mpfloat; E : Exponent) is
      R : constant int := mpfr_set_exp (X.Value'Access, mpfr_exp_t (E));
   begin
      if R /= 0 then
         raise Failure with "cannot Set_Exp of " & X.To_String;
      end if;
   end Set_Exp;

   -------------
   -- Signbit --
   -------------

   function Signbit (Op : Mpfloat) return Sign is
      S : constant int := mpfr_signbit (Op.Value'Access);
   begin
      return (if S = 0 then Pos else Neg);
   end Signbit;

   -------------
   -- Setsign --
   -------------

   procedure Setsign
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      S   : Sign;
      Rnd : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_setsign (Rop.Value'Access,
                                        Op.Value'Access,
                                        S'Enum_Rep,
                                        Rounding'Pos (Rnd)));
   end Setsign;

   --------------
   -- Copysign --
   --------------

   procedure Copysign
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_copysign (Rop.Value'Access,
                                         Op1.Value'Access,
                                         Op2.Value'Access,
                                         Rounding'Pos (Rnd)));
   end Copysign;

   -----------------
   -- Get_Version --
   -----------------

   function Get_Version return String is
      V : constant chars_ptr := mpfr_get_version;
   begin
      return Value (V);
   end Get_Version;

   --------------
   -- Get_Emin --
   --------------

   function Get_Emin return Exponent is
     (Exponent (mpfr_get_emin));

   --------------
   -- Get_Emax --
   --------------

   function Get_Emax return Exponent is
     (Exponent (mpfr_get_emax));

   --------------
   -- Set_Emin --
   --------------

   procedure Set_Emin (Exp : Exponent) is
      R : constant int := mpfr_set_emin (mpfr_exp_t (Exp));
   begin
      if R = 0 then
         raise Failure with "error with mpfr_set_emin, exponent not in range";
      elsif Exp > Get_Emax then
         raise Failure with "Set_Emin: emin > emax";
      end if;
   end Set_Emin;

   --------------
   -- Set_Emax --
   --------------

   procedure Set_Emax (Exp : Exponent) is
      R : constant int := mpfr_set_emax (mpfr_exp_t (Exp));
   begin
      if R = 0 then
         raise Failure with "error with mpfr_set_emax, exponent not in range";
      elsif Get_Emin > Exp then
         raise Failure with "Set_Emax: emin > emax";
      end if;
   end Set_Emax;

   ------------------
   -- Get_Emin_Min --
   ------------------

   function Get_Emin_Min return Exponent is
     (Exponent (mpfr_get_emin_min));

   ------------------
   -- Get_Emin_Max --
   ------------------

   function Get_Emin_Max return Exponent is
     (Exponent (mpfr_get_emin_max));

   ------------------
   -- Get_Emax_Min --
   ------------------

   function Get_Emax_Min return Exponent is
     (Exponent (mpfr_get_emax_min));

   ------------------
   -- Get_Emax_Max --
   ------------------

   function Get_Emax_Max return Exponent is
     (Exponent (mpfr_get_emax_max));

   -----------------
   -- Check_Range --
   -----------------

   procedure Check_Range (X : in out Mpfloat; Rnd : Rounding := RNDEF) is
   begin
      X.Ternary :=
        To_Ternary_Value (mpfr_check_range (X.Value'Access,
                                            X.Ternary'Enum_Rep,
                                            Rounding'Pos (Rnd)));
   end Check_Range;

   ------------------
   -- Subnormalize --
   ------------------

   procedure Subnormalize (X : in out Mpfloat; Rnd : Rounding := RNDEF) is
   begin
      X.Ternary :=
        To_Ternary_Value (mpfr_subnormalize (X.Value'Access,
                                             X.Ternary'Enum_Rep,
                                             Rounding'Pos (Rnd)));
   end Subnormalize;

   ---------------------
   -- Clear_Underflow --
   ---------------------

   procedure Clear_Underflow is
   begin
      mpfr_clear_underflow;
   end Clear_Underflow;

   --------------------
   -- Clear_Overflow --
   --------------------

   procedure Clear_Overflow is
   begin
      mpfr_clear_overflow;
   end Clear_Overflow;

   ------------------
   -- Clear_Divby0 --
   ------------------

   procedure Clear_Divby0 is
   begin
      mpfr_clear_divby0;
   end Clear_Divby0;

   -------------------
   -- Clear_Nanflag --
   -------------------

   procedure Clear_Nanflag is
   begin
      mpfr_clear_nanflag;
   end Clear_Nanflag;

   --------------------
   -- Clear_Inexflag --
   --------------------

   procedure Clear_Inexflag is
   begin
      mpfr_clear_inexflag;
   end Clear_Inexflag;

   ----------------------
   -- Clear_Erangeflag --
   ----------------------

   procedure Clear_Erangeflag is
   begin
      mpfr_clear_erangeflag;
   end Clear_Erangeflag;

   -----------------
   -- Clear_Flags --
   -----------------

   procedure Clear_flags is
   begin
      mpfr_clear_flags;
   end Clear_flags;

   -------------------
   -- Set_Underflow --
   -------------------

   procedure Set_Underflow is
   begin
      mpfr_set_underflow;
   end Set_Underflow;

   ------------------
   -- Set_Overflow --
   ------------------

   procedure Set_Overflow is
   begin
      mpfr_set_overflow;
   end Set_Overflow;

   ----------------
   -- Set_Divby0 --
   ----------------

   procedure Set_Divby0 is
   begin
      mpfr_set_divby0;
   end Set_Divby0;

   -----------------
   -- Set_Nanflag --
   -----------------

   procedure Set_Nanflag is
   begin
      mpfr_set_nanflag;
   end Set_Nanflag;

   ------------------
   -- Set_Inexflag --
   ------------------

   procedure Set_Inexflag is
   begin
      mpfr_set_inexflag;
   end Set_Inexflag;

   --------------------
   -- Set_Erangeflag --
   --------------------

   procedure Set_Erangeflag is
   begin
      mpfr_set_erangeflag;
   end Set_Erangeflag;

   ---------------
   -- Underflow --
   ---------------

   function Underflow return Boolean is
     (if mpfr_underflow_p /= 0 then True else False);

   --------------
   -- Overflow --
   --------------

   function Overflow return Boolean is
     (if mpfr_overflow_p /= 0 then True else False);

   ------------
   -- Divby0 --
   ------------

   function Divby0 return Boolean is
     (if mpfr_divby0_p /= 0 then True else False);

   -------------
   -- Nanflag --
   -------------

   function Nanflag return Boolean is
     (if mpfr_nanflag_p /= 0 then True else False);

   --------------
   -- Inexflag --
   --------------

   function Inexflag return Boolean is
     (if mpfr_inexflag_p /= 0 then True else False);

   ----------------
   -- Erangeflag --
   ----------------

   function Erangeflag return Boolean is
     (if mpfr_erangeflag_p /= 0 then True else False);

   ---------------------
   -- To_Mpfr_Flags_T --
   ---------------------

   function To_Mpfr_Flags_T (M : Flags_Mask) return mpfr_flags_t is
      Flags : mpfr_flags_t := 0;
   begin
      for F of M loop
         Flags := @ + F'Enum_Rep;
      end loop;
      return Flags;
   end To_Mpfr_Flags_T;

   -------------------
   -- To_Flags_Mask --
   -------------------

   function To_Flags_Mask (M : mpfr_flags_t) return Flags_Mask is
      Flags : Flags_Mask (1 .. 6);
      I : Integer := 0;
      procedure Check (F : Flag);

      procedure Check (F : Flag) is
      begin
         if M >= Flag'Enum_Rep (F) then
            I := @ + 1;
            Flags (I) := F;
         end if;
      end Check;
   begin
      for F of Flags_All loop
         Check (F);
      end loop;

      return Flags (1 .. I);
   end To_Flags_Mask;

   -----------------
   -- Flags_Clear --
   -----------------

   procedure Flags_Clear (Mask : Flags_Mask) is
   begin
      mpfr_flags_clear (To_Mpfr_Flags_T (Mask));
   end Flags_Clear;

   ---------------
   -- Flags_Set --
   ---------------

   procedure Flags_Set (Mask : Flags_Mask) is
   begin
      mpfr_flags_set (To_Mpfr_Flags_T (Mask));
   end Flags_Set;

   ----------------
   -- Flags_Test --
   ----------------

   function Flags_Test (Mask : Flags_Mask) return Flags_Mask is
     (To_Flags_Mask (mpfr_flags_test (To_Mpfr_Flags_T (Mask))));

   ----------------
   -- Flags_Save --
   ----------------

   function Flags_Save return Flags_Mask is
     (To_Flags_Mask (mpfr_flags_save));

   -------------------
   -- Flags_Restore --
   -------------------

   procedure Flags_Restore (Flags, Mask : Flags_Mask) is
   begin
      mpfr_flags_restore (To_Mpfr_Flags_T (Flags), To_Mpfr_Flags_T (Mask));
   end Flags_Restore;

   --------------
   -- Get_Prec --
   --------------

   function Get_Prec (X : Mpfloat) return Precision is
   begin
      return Precision (mpfr_get_prec (X.Value'Access));
   end Get_Prec;

   --------------
   -- Set_Prec --
   --------------

   procedure Set_Prec (X : Mpfloat; Prec : Precision) is
   begin
      mpfr_set_prec (X.Value'Access, mpfr_prec_t (Prec));
   end Set_Prec;

   ----------------------
   -- Set_Default_Prec --
   ----------------------

   procedure Set_Default_Prec (Prec : Precision) is
   begin
      mpfr_set_default_prec (mpfr_prec_t (Prec));
   end Set_Default_Prec;

   ----------------------
   -- Get_Default_Prec --
   ----------------------

   function Get_Default_Prec return Precision is
   begin
      return Precision (mpfr_get_default_prec);
   end Get_Default_Prec;

end Admpfr;
