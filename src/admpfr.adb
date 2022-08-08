--  This file is part of AdMPFR.
--
--  AdMPFR is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  AdMPFR is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Foobar.  If not, see <https://www.gnu.org/licenses/>.

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

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

   -----------------
   -- Mpfr_Printf --
   -----------------

   procedure Mpfr_Printf (Template : String;
                          X        : Mpfloat;
                          R        : Rounding := RNDN) is
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
   end Mpfr_Printf;

   ------------------
   -- Mpfr_Sprintf --
   ------------------

   function Mpfr_Sprintf (Template : String;
                          X        : Mpfloat;
                          R        : Rounding := RNDN) return String is
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
   end Mpfr_Sprintf;

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

   procedure Set (Rop : out Mpfloat; Op : Mpfloat; Rnd : Rounding := RNDN) is
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
      Rnd : Rounding := RNDN) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_set_si (Rop.Value'Access,
                                       long (Op),
                                       Rounding'Pos (Rnd)));
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Rop : out Mpfloat; Op : Float; Rnd : Rounding := RNDN) is
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
      Rnd : Rounding := RNDN) is
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
      Rnd : Rounding := RNDN) is
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
      Rnd : Rounding := RNDN) is
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
      Rnd  : Rounding    := RNDN)
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

   function Get_Float (Op : Mpfloat; Rnd : Rounding := RNDN) return Float is
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
      Rnd : Rounding := RNDN) return Long_Float is
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
      Rnd : Rounding := RNDN) return Long_Long_Float is
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
      Rnd : Rounding := RNDN) return Long_Integer is
   begin
      return Long_Integer (mpfr_get_si (Op.Value'Access, Rounding'Pos (Rnd)));
   end Get_Long_Integer;

   --------------------
   -- Get_Long_Float --
   --------------------

   function Get_Long_Float
     (Op  : Mpfloat;
      Exp : out Long_Integer;
      Rnd : Rounding := RNDN) return Long_Float
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
      Rnd : Rounding := RNDN) return Long_Long_Float
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
      Rnd : Rounding := RNDN)
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
      Rnd  : Rounding    := RNDN) return String
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
      Rnd : Rounding := RNDN) return Boolean
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
      Rnd : Rounding := RNDN) return Boolean
   is
      Fits : constant int := mpfr_fits_sint_p (Op.Value'Access,
                                               Rounding'Pos (Rnd));
   begin
      return (if Fits /= 0 then True else False);
   end Fits_Integer;

   ---------
   -- Add --
   ---------

   procedure Add
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDN) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_add (Rop.Value'Access,
                                    Op1.Value'Access,
                                    Op2.Value'Access,
                                    Rounding'Pos (Rnd)));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd  : Rounding := RNDN) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_add_si (Rop.Value'Access,
                                       Op1.Value'Access,
                                       long (Op2),
                                       Rounding'Pos (Rnd)));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Float;
      Rnd : Rounding := RNDN) is
   begin
      Rop.Ternary :=
        To_Ternary_Value (mpfr_add_d (Rop.Value'Access,
                                      Op1.Value'Access,
                                      double (Op2),
                                      Rounding'Pos (Rnd)));
   end Add;

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
