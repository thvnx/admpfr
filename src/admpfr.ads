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

pragma Ada_2022;

with Ada.Finalization; use Ada.Finalization;
with Ada.Text_IO;      use Ada.Text_IO;
with Interfaces.C;     use Interfaces.C;

with Ada.Strings.Text_Buffers;
with System;

package Admpfr is

   --  NOTE: documentation here is heavily copy/pasted from
   --  https://www.mpfr.org/mpfr-current/mpfr.html.

   type Base is range -36 .. 62 with
     Dynamic_Predicate => abs Base /= 1;
   --  Base is in range [-36 .. -2; 0; 2 .. 62]. Admpfr uses Dynamic_Predicate
   --  and preconditions (depending on the procedure or function using it) to
   --  ensure the base is actually in the range of supported values.

   type Rounding is (RNDN, RNDZ, RNDU, RNDD, RNDA, RNDF);
   for Rounding'Size use int'Size;
   --  Rounding represents the C mpfr_rnd_t enumeration.
   --
   --  RNDN: round to nearest, with ties to even
   --  RNDZ: round toward zero
   --  RNDU: round toward +Inf
   --  RNDD: round toward -Inf
   --  RNDA: round away from zero
   --  RNDF: faithful rounding
   --
   --  CAUTION: stick to the order declared in the mpfr.h header in order to
   --  ensure reproductible results between C and Ada mpfr-based applications.

   type Precision is range 1 .. Long_Integer'Last - 256;
   --  Precision reprensents the C mpfr_prec_t type. The MPFR library defines
   --  the lower bound to 1 and the higher bound to Long_Integer'Last - 256.

   function Get_Default_Prec return Precision;
   --  Return the current default MPFR precision in bits. See the documentation
   --  of `Set_default_prec`.

   type Mpfloat (Prec : Precision := Get_Default_Prec) is
     tagged limited private;
   --  `Mpfloat` is the Ada counterpart of the mpfr_t C type. The `Prec`
   --  constraint can be used to set the precision of the `Mpfloat` number (it
   --  uses mpfr_init2 under the hood), which is set to the default MPFR
   --  precision.

   type Mpfloat_Array is array (Integer range <>) of Mpfloat;

   procedure Mpfloat_Image
     (Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : Mpfloat);

   for Mpfloat'Put_Image use Mpfloat_Image;

   type Ternary_Value is (EXACT, GREATER, LOWER, NOT_SET);
   --  Represents the C returning value of type int, called the ternary value
   --  (see MPFR documentation for more details). If the ternary value is
   --  `EXACT`, it means that the value stored in the destination variable is
   --  the exact result of the corresponding mathematical function. If the
   --  ternary value is `GREATER` (resp. `LOWER`), it means the value stored in
   --  the destination variable is greater (resp. lower) than the exact result.
   --  If the ternary value is `NOT_SET`, it means that the destination
   --  variable has not been set yet.

   subtype Exponent is Long_Integer;
   --  Represents the exponent mpfr_exp_t C type

   type Sign is (Neg, Pos);
   for Sign use (Neg => -1, Pos => 0);
   --  Mpfloat sign

   type Compare is (Less, Equal, Greater);
   for Compare use (Less => -1, Equal => 0, Greater => 1);
   --  Type for the return value of the `Cmp*` functions

   type Flag is (Underflow, Overflow, Divby0, Nanflag, Inexflag, Erangeflag);
   for Flag use (Underflow => 1,
                 Overflow => 2,
                 Divby0 => 4,
                 Nanflag => 8,
                 Inexflag => 16,
                 Erangeflag => 32);

   type Flags_Mask is array (Integer range <>) of Flag;

   Flags_All : Flags_Mask :=
     [Underflow, Overflow, Divby0, Nanflag, Inexflag, Erangeflag];

   function Get_Default_Rounding_Mode return Rounding;
   function RNDEF return Rounding renames Get_Default_Rounding_Mode;
   --  Get the default rounding mode.

   procedure Set (Rop : out Mpfloat; Op : Mpfloat; Rnd : Rounding := RNDEF);
   --  Set the value of `Rop` from `Op`, rounded toward the given direction
   --  `Rnd`. The sign of a NaN is propagated in order to mimic the IEEE 754
   --  copy operation. But contrary to IEEE 754, the NaN flag is set as usual.

   procedure Set
     (Rop : out Mpfloat;
      Op  : Long_Integer;
      Rnd : Rounding := RNDEF);
   --  Set the value of `Rop` from `Op`, rounded toward the given direction
   --  `Rnd`. The sign of a NaN is propagated in order to mimic the IEEE 754
   --  copy operation. But contrary to IEEE 754, the NaN flag is set as usual.
   --  The input 0 is converted to +0.

   procedure Set (Rop : out Mpfloat; Op : Float; Rnd : Rounding := RNDEF);
   procedure Set (Rop : out Mpfloat; Op : Long_Float; Rnd : Rounding := RNDEF);
   procedure Set
     (Rop : out Mpfloat;
      Op  : Long_Long_Float;
      Rnd : Rounding := RNDEF);
   --  Set the value of `Rop` from `Op`, rounded toward the given direction
   --  `Rnd`. The sign of a NaN is propagated in order to mimic the IEEE 754
   --  copy operation. But contrary to IEEE 754, the NaN flag is set as usual.

   procedure Set
     (Rop : out Mpfloat;
      Op  : Long_Integer;
      E   : Exponent;
      Rnd : Rounding := RNDEF);
   --  Set the value of `Rop` from `Op` multiplied by two to the power `E`,
   --  rounded toward the given direction `Rnd`. Note that the input 0 is
   --  converted to +0.

   procedure Set
     (Rop  : out Mpfloat;
      S    : String;
      Base : Admpfr.Base := 10;
      Rnd  : Rounding    := RNDEF)
   with
     Pre => Base = 0 or Base > 1;
   --  Set `Rop` to the value of the string `S` in base `Base`, rounded in the
   --  direction `Rnd`. `Base` and `Rnd` are optionals, their default values
   --  are 10 and RNDN, respectively.
   --
   --  Parsing follows the standard C strtod function with some extensions.
   --  After optional leading whitespace, one has a subject sequence consisting
   --  of an optional sign ('+' or '-'), and either numeric data or special
   --  data. The subject sequence is defined as the longest initial subsequence
   --  of the input string, starting with the first non-whitespace character,
   --  that is of the expected form.

   --  The form of numeric data is a non-empty sequence of significand digits
   --  with an optional decimal-point character, and an optional exponent
   --  consisting of an exponent prefix followed by an optional sign and a
   --  non-empty sequence of decimal digits. A significand digit is either a
   --  decimal digit or a Latin letter (62 possible characters), with 'A' = 10,
   --  'B' = 11, ..., 'Z' = 35; case is ignored in bases less than or equal to
   --  36, in bases larger than 36, 'a' = 36, 'b' = 37, ..., 'z' = 61. The
   --  value of a significand digit must be strictly less than the base. The
   --  decimal-point character can be either the one defined by the current
   --  locale or the period (the first one is accepted for consistency with the
   --  C standard and the practice, the second one is accepted to allow the
   --  programmer to provide MPFR numbers from strings in a way that does not
   --  depend on the current locale). The exponent prefix can be 'e' or 'E' for
   --  bases up to 10, or '@' in any base; it indicates a multiplication by a
   --  power of the base. In bases 2 and 16, the exponent prefix can also be
   --  'p' or 'P', in which case the exponent, called binary exponent,
   --  indicates a multiplication by a power of 2 instead of the base (there is
   --  a difference only for base 16); in base 16 for example '1p2' represents
   --  4 whereas '1@2' represents 256. The value of an exponent is always
   --  written in base 10.

   --  If the argument base is 0, then the base is automatically detected as
   --  follows. If the significand starts with '0b' or '0B', base 2 is assumed.
   --  If the significand starts with '0x' or '0X', base 16 is assumed.
   --  Otherwise base 10 is assumed.

   --  Note: The exponent (if present) must contain at least a digit. Otherwise
   --  the possible exponent prefix and sign are not part of the number (which
   --  ends with the significand). Similarly, if '0b', '0B', '0x' or '0X' is
   --  not followed by a binary/hexadecimal digit, then the subject sequence
   --  stops at the character '0', thus 0 is read.

   --  Special data (for infinities and NaN) can be '@inf@' or
   --  '@nan@(n-char-sequence-opt)', and if base <= 16, it can also be
   --  'infinity', 'inf', 'nan' or 'nan(n-char-sequence-opt)', all case
   --  insensitive. A 'n-char-sequence-opt' is a possibly empty string
   --  containing only digits, Latin letters and the underscore (0, 1, 2, ...,
   --  9, a, b, ..., z, A, B, ..., Z, _). Note: one has an optional sign for
   --  all data, even NaN. For example, '-@nAn@(This_Is_Not_17)' is a valid
   --  representation for NaN in base 17.

   procedure Set_Nan (X : out Mpfloat);
   procedure Set_Inf (X : out Mpfloat; S : Sign);
   procedure Set_Zero (X : out Mpfloat; S : Sign);
   --  Set the variable `X` to NaN (Not-a-Number), infinity or zero
   --  respectively. In `Set_Nan`, the sign bit of the result is unspecified.
   --
   --  TODO: ternary value of `X` won't change here, add another one for that
   --  case?

   procedure Swap (X : in out Mpfloat; Y : in out Mpfloat);
   --  Swap `X` and `Y`. In particular, the values are exchanged without
   --  rounding (this may be different from three `Set` calls using a third
   --  auxiliary variable).

   function Get_Ternary_Value (X : Mpfloat) return Ternary_Value;
   --  Return the ternary value of `X`

   function Get_Float (Op : Mpfloat; Rnd : Rounding := RNDEF) return Float;
   function Get_Long_Float
     (Op  : Mpfloat;
      Rnd : Rounding := RNDEF) return Long_Float;
   function Get_Long_Long_Float
     (Op  : Mpfloat;
      Rnd : Rounding := RNDEF) return Long_Long_Float;
   --  Convert `Op` to a Float (respectively Long_Float, Long_Long_Float) using
   --  the rounding mode `Rnd`. If `Op` is NaN or an Inf, a Failure exception
   --  is raised. If `Op` is zero, these functions return a zero, trying to
   --  preserve its sign, if possible.

   function Get_Long_Integer
     (Op  : Mpfloat;
      Rnd : Rounding := RNDEF) return Long_Integer;
   --  Convert `op` to a Long_Integer after rounding it to an integer with
   --  respect to `Rnd`. If `Op` is NaN, 0 is returned and the erange flag
   --  is set. If `Op` is too big for the return type, the function returns
   --  the maximum or the minimum of the corresponding C type, depending on
   --  the direction of the overflow; the erange flag is set too. When there
   --  is no such range error, if the return value differs from `Op`, i.e.,
   --  if `Op` is not an integer, the inexact flag is set. See also
   --  `Fits_Long_Integer`.

   function Get_Long_Float
     (Op  : Mpfloat;
      Exp : out Long_Integer;
      Rnd : Rounding := RNDEF) return Long_Float;
   function Get_Long_Long_Float
     (Op  : Mpfloat;
      Exp : out Long_Integer;
      Rnd : Rounding := RNDEF) return Long_Long_Float;
   --  Return d and set `Exp` such that 0.5<=abs(d)<1 and d times 2 raised to
   --  `Exp` equals `Op` rounded to Long_Float (resp. Long_Long_Float)
   --  precision, using the given rounding mode. If `Op` is zero, then a zero
   --  of the same sign is returned, and `Exp` is set to 0. If `Op` is NaN or
   --  an infinity, then a Failure exception is raised.

   procedure Set
     (Rop : out Mpfloat;
      Exp : out Long_Integer;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Exp` and `Rop` such that 0.5<=abs(`Rop`)<1 and `Rop` times 2 raised
   --  to `Exp` equals `Op` rounded to the precision of `Rop`, using the given
   --  rounding mode. If `Op` is zero, then `Rop` is set to a zero of the same
   --  sign and `Exp` is set to 0. If `Op` is NaN or an infinity, then `Rop` is
   --  set to the same value and `Exp` is undefined.

   function To_String
     (X    : Mpfloat;
      Base : Admpfr.Base := 10;
      Rnd  : Rounding    := RNDEF) return String
   with
     Pre => Base /= 0;
   --  Convert `X` to a String in base `Base` rounded in the direction `Rnd`.
   --  This function is based on mpfr_get_str and has no real equivalent in the
   --  original C library.
   --
   --  Default behavior mimics mpfr_printf("%.RNe", X), (at least for base 10)!

   function Fits_Long_Integer
     (Op  : Mpfloat;
      Rnd : Rounding := RNDEF) return Boolean;
   function Fits_Integer
     (Op  : Mpfloat;
      Rnd : Rounding := RNDEF) return Boolean;
   --  Return whether `Op` would fit in the respective Ada data type,
   --  respectively Long_Integer and Integer, when rounded to an integer in
   --  the direction `Rnd`.

   procedure Add
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF);
   procedure Add
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF);
   procedure Add
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Float;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to `Op1 + Op2` rounded in the direction `Rnd`. The IEEE 754
   --  rules are used, in particular for signed zeros.

   procedure Sub
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF);
   procedure Sub
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF);
   procedure Sub
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Float;
      Rnd : Rounding := RNDEF);
   procedure Sub
     (Rop : in out Mpfloat;
      Op1 : Long_Integer;
      Op2 : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Sub
     (Rop : in out Mpfloat;
      Op1 : Long_Float;
      Op2 : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to `Op1 - Op2` rounded in the direction `Rnd`. The IEEE 754
   --  rules are used, in particular for signed zeros.

   procedure Mul
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF);
   procedure Mul
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF);
   procedure Mul
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Float;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to `Op1` times `Op2` rounded in the direction `Rnd`. The
   --  IEEE 754 rules are used, in particular for signed zeros.

   procedure Sqr (Rop : in out Mpfloat; Op : Mpfloat; Rnd : Rounding := RNDEF);
   --  Set `Rop` to the square of `Op` rounded in the direction `Rnd`.

   procedure Div
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF);
   procedure Div
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF);
   procedure Div
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Float;
      Rnd : Rounding := RNDEF);
   procedure Div
     (Rop : in out Mpfloat;
      Op1 : Long_Integer;
      Op2 : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Div
     (Rop : in out Mpfloat;
      Op1 : Long_Float;
      Op2 : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to `Op1 / Op2` rounded in the direction `Rnd`. The IEEE 754
   --  rules are used, in particular for signed zeros.

   procedure Sqrt
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Sqrt
     (Rop : in out Mpfloat;
      Op  : Long_Integer;
      Rnd : Rounding := RNDEF)
   with
     Pre => Op >= 0;
   --  Set `Rop` to the square root of `Op` rounded in the direction `Rnd`. Set
   --  `Rop` to -0 if `Op` is -0, to be consistent with the IEEE 754 standard.
   --  Set `Rop` to NaN if `Op` is negative.

   procedure Rec_Sqrt
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the reciprocal square root of `Op` rounded in the direction
   --  `Rnd`. Set `Rop` to +Inf if `Op` is +/-0, +0 if `Op` is +Inf, and NaN if
   --  `Op` is negative. Warning! Therefore the result on -0 is different from
   --  the one of the rSqrt function recommended by the IEEE 754-2008 standard
   --  (Section 9.2.1), which is -Inf instead of +Inf.

   procedure Cbrt
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Rootn
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      N   : Long_Integer;
      Rnd : Rounding := RNDEF)
   with
     Pre => N >= 0;
   --  Set `Rop` to the nth root (with n = 3, the cubic root, for `Cbrt`) of
   --  `Op` rounded in the direction `Rnd`. For n = 0, set `Rop` to NaN. For n
   --  odd (resp. even) and `Op` negative (including -Inf), set `Rop` to a
   --  negative number (resp. NaN). If `Op` is zero, set `Rop` to zero with the
   --  sign obtained by the usual limit rules, i.e., the same sign as `Op` if n
   --  is odd, and positive if n is even.

   --  mpfr_root is not implemented since deprecated, use `Rootn` instead.

   procedure Neg
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to `-Op`, rounded in the direction `Rnd`. Just changes or
   --  adjusts the sign if `Rop` and `Op` are the same variable, otherwise a
   --  rounding might occur if the precision of `Rop` is less than that of
   --  `Op`.

   procedure Absolute
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the absolute value of `Op`, rounded in the direction `Rnd`.
   --  Just changes or adjusts the sign if `Rop` and `Op` are the same
   --  variable, otherwise a rounding might occur if the precision of `Rop`
   --  is less than that of `Op`.

   procedure Dim
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF);
   --  Set `Rop` to the positive difference of `Op1` and `Op2`, i.e.,
   --  `Op1 - Op2` rounded in the direction `Rnd` if `Op1 > op2`, +0 if
   --  `Op1 <= Op2`, and NaN if `Op1` or `Op2` is NaN.

   procedure Mul_2
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to `Op1` times 2 raised to `Op2` rounded in the direction
   --  `Rnd`. Just increases the exponent by `Op2` when `Rop` and `Op1` are
   --  identical.

   procedure Div_2
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to `Op1` divided by 2 raised to `Op2` rounded in the direction
   --  `Rnd`. Just decreases the exponent by `Op2` when `Rop` and `Op1` are
   --  identical.

   procedure Fac
     (Rop : in out Mpfloat;
      Op  : Long_Integer;
      Rnd : Rounding := RNDEF)
   with
     Pre => Op >= 0;
   --  Set `Rop` to the factorial of `Op`, rounded in the direction `Rnd`.

   procedure Fma
     (Rop           : in out Mpfloat;
      Op1, Op2, Op3 : Mpfloat;
      Rnd           : Rounding := RNDEF);
   procedure Fms
     (Rop           : in out Mpfloat;
      Op1, Op2, Op3 : Mpfloat;
      Rnd           : Rounding := RNDEF);
   --  Set `Rop` to `(Op1 times Op2) + Op3` (resp. `(Op1 times Op2) - Op3)`
   --  rounded in the direction `Rnd`. Concerning special values (signed
   --  zeros, infinities, NaN), these functions behave like a multiplication
   --  followed by a separate addition or subtraction. That is, the fused
   --  operation matters only for rounding.

   procedure Fmma
     (Rop                : in out Mpfloat;
      Op1, Op2, Op3, Op4 : Mpfloat;
      Rnd                : Rounding := RNDEF);
   procedure Fmms
     (Rop                : in out Mpfloat;
      Op1, Op2, Op3, Op4 : Mpfloat;
      Rnd                : Rounding := RNDEF);
   --  Set `Rop` to `(Op1 times Op2) + (Op3 times Op4)` (resp.
   --  `(Op1 times Op2) - (Op3 times Op4)`) rounded in the direction `Rnd`.
   --  In case the computation of `Op1` times `Op2` overflows or underflows
   --  (or that of `Op3` times `Op4`), the result `Rop` is computed as if the
   --  two intermediate products were computed with rounding toward zero.

   procedure Hypot
     (Rop  : in out Mpfloat;
      X, Y : Mpfloat;
      Rnd  : Rounding := RNDEF);
   --  Set `Rop` to the Euclidean norm of `X` and `Y`, i.e., the square root
   --  of the sum of the squares of `X` and `Y`, rounded in the direction
   --  `Rnd`.

   procedure Sum
     (Rop : in out Mpfloat;
      Arr : Mpfloat_Array;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the sum of all elements of `Arr`, correctly rounded in the
   --  direction `Rnd`. If the array is empty, then the result is +0, and if
   --  `Arr'Length = 1`, then the function is equivalent to `Set`. For the
   --  special exact cases, the result is the same as the one obtained with
   --  a succession of additions (`Add`) in infinite precision. In particular,
   --  if the result is an exact zero and `Arr'Length >= 1`:
   --  - if all the inputs have the same sign (i.e., all +0 or all -0), then
   --    the result has the same sign as the inputs;
   --  - otherwise, either because all inputs are zeros with at least a +0 and
   --    a -0, or because some inputs are non-zero (but they globally cancel),
   --    the result is +0, except for the `RNDD` rounding mode, where it is -0.

   procedure Dot
     (Rop  : in out Mpfloat;
      Arr1 : Mpfloat_Array;
      Arr2 : Mpfloat_Array;
      Rnd  : Rounding := RNDEF);
   --  Set `Rop` to the dot product of elements of `Arr1` by those of `Arr2`,
   --  correctly rounded in the direction `Rnd`. The product size is defined
   --  by min (Arr1'Length, Arr2`Length). This function is experimental, and
   --  does not yet handle intermediate overflows and underflows.

   function Cmp (Op1, Op2 : Mpfloat) return Compare;
   function Cmp (Op1 : Mpfloat; Op2 : Long_Integer) return Compare;
   function Cmp (Op1 : Mpfloat; Op2 : Long_Float) return Compare;
   function Cmp (Op1 : Mpfloat; Op2 : Long_Long_Float) return Compare;
   --  Compare `Op1` and `Op2`. Return `Greater` if `Op1 > op2`, `Equal` if
   --  `Op1 = Op2`, and `Less` value if `Op1 < Op2`. Both `Op1` and `Op2` are
   --  considered to their full own precision, which may differ. If one of the
   --  operands is NaN, set the erange flag and return `Equal`.

   --  Note: These functions may be useful to distinguish the three possible
   --  cases. If you need to distinguish two cases only, it is recommended to
   --  use the operators (e.g., "=" for the equality) described below; they
   --  behave like the IEEE 754 comparisons, in particular when one or both
   --  arguments are NaN. But only floating-point numbers can be compared
   --  (you may need to do a conversion first).

   function Cmp
     (Op1 : Mpfloat;
      Op2 : Long_Integer;
      E   : Exponent) return Compare;
   --  Compare `Op1` and `Op2` multiplied by two to the power `E`. Similar as
   --  above.

   function Cmp_Abs (Op1, Op2 : Mpfloat) return Compare;
   function Cmp_Abs (Op1 : Mpfloat; Op2 : Long_Integer) return Compare;
   --  Compare `|Op1| and |Op2|`. Return `Greater` value if `|Op1| > |Op2|`,
   --  `Equal` if `|Op1| = |Op2|`, and `Less` if `|Op1| < |Op2|`. If one of
   --  the operands is NaN, set the erange flag and return `Equal`.

   function Is_Nan (Op : Mpfloat) return Boolean;
   function Is_Inf (Op : Mpfloat) return Boolean;
   function Is_Number (Op : Mpfloat) return Boolean;
   function Is_Zero (Op : Mpfloat) return Boolean;
   function Is_Regular (Op : Mpfloat) return Boolean;
   --  Return whether `Op` is respectively NaN, an infinity, an ordinary number
   --  (i.e., neither NaN nor an infinity), zero, or a regular number
   --  (i.e., neither NaN, nor an infinity nor zero).

   function Greater (Op1, Op2 : Mpfloat) return Boolean;
   function Greaterequal (Op1, Op2 : Mpfloat) return Boolean;
   function Less (Op1, Op2 : Mpfloat) return Boolean;
   function Lessequal (Op1, Op2 : Mpfloat) return Boolean;
   function Equal (Op1, Op2 : Mpfloat) return Boolean;
   --  Return whether `Op1 > Op2`, `Op1 >= Op2`, `Op1 < Op2`, `Op1 <= Op2`,
   --  `Op1 = Op2` respectively. Those functions return False whenever
   --  `Op1` and/or `Op2` is NaN.

   function Lessgreater (Op1, Op2 : Mpfloat) return Boolean;
   --  Return whether `Op1 < Op2` or `Op1 > Op2` (i.e., neither `Op1`,
   --  nor `Op2` is NaN, and `Op1 <> Op2`) or not (i.e., `Op1` and/or `Op2`
   --  is NaN, or `Op1 = Op2`).

   function Unordered (Op1, Op2 : Mpfloat) return Boolean;
   --  Return whether `Op1` or `Op2` is a NaN (i.e., they cannot be compared).

   function Total_Order (Op1, Op2 : Mpfloat) return Boolean;
   --  This function implements the totalOrder predicate from IEEE 754-2008,
   --  where -NaN < -Inf < negative finite numbers < -0 < +0 < positive finite
   --  numbers < +Inf < +NaN.

   function "=" (Op1, Op2 : Mpfloat) return Boolean is (Equal (Op1, Op2));
   --  Note: due to how Ada set the "/=" operator being the negation of "=",
   --  its use is discouraged since it returns the wrong result if `Op1` or
   --  `Op2` is NaN.
   function ">" (Op1, Op2 : Mpfloat) return Boolean is (Greater (Op1, Op2));
   function "<" (Op1, Op2 : Mpfloat) return Boolean is (Less (Op1, Op2));
   function ">=" (Op1, Op2 : Mpfloat) return Boolean is
     (Greaterequal (Op1, Op2));
   function "<=" (Op1, Op2 : Mpfloat) return Boolean is (Lessequal (Op1, Op2));

   procedure Log
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Log
     (Rop : in out Mpfloat;
      Op  : Long_Integer;
      Rnd : Rounding := RNDEF)
   with
     Pre => Op >= 0;
   procedure Log2
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Log10
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the natural logarithm of `Op`, log2(`Op`) or log10(`Op`),
   --  respectively, rounded in the direction `Rnd`. Set `Rop` to +0 if `Op`
   --  is 1 (in all rounding modes), for consistency with the ISO C99 and
   --  IEEE 754-2008 standards. Set `Rop` to -Inf if `Op` is +/-0 (i.e., the
   --  sign of the zero has no influence on the result).

   procedure Log1p
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the logarithm of one plus `Op`, rounded in the direction
   --  `Rnd`. Set `Rop` to -Inf if `Op` is -1.

   procedure Exp
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Exp2
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Exp10
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the exponential of `Op`, to 2 power of `Op` or to 10 power
   --  of `Op`, respectively, rounded in the direction `Rnd`.

   procedure Expm1
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the exponential of `Op` followed by a subtraction by one,
   --  rounded in the direction `Rnd`.

   procedure Pow
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF);
   procedure Pow
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF);
   procedure Pow
     (Rop      : in out Mpfloat;
      Op1, Op2 : Long_Integer;
      Rnd      : Rounding := RNDEF);
   procedure Pow
     (Rop : in out Mpfloat;
      Op1 : Long_Integer;
      Op2 : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to `Op1` raised to `Op2`, rounded in the direction `Rnd`. See
   --  official MPFR documentation for special values handling details.

   procedure Cos
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Sin
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Tan
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the cosine of `Op`, sine of `Op`, tangent of `Op`, rounded
   --  in the direction `Rnd`.

   procedure Sin_Cos
     (Sop, Cop : in out Mpfloat;
      Op       : Mpfloat;
      Rnd      : Rounding := RNDEF);
   --  Set simultaneously `Sop` to the sine of `Op` and `Cop` to the cosine of
   --  `Op`, rounded in the direction `Rnd` with the corresponding precisions
   --  of `Sop` and `Cop`, which must be different variables.

   procedure Sec
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Csc
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Cot
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Ro`p to the secant of `Op`, cosecant of `Op`, cotangent of `Op`,
   --  rounded in the direction `Rnd`.

   procedure Acos
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Asin
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Atan
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the arc-cosine, arc-sine or arc-tangent of `Op`,
   --  rounded in the direction `Rnd`.

   procedure Atan2
     (Rop   : in out Mpfloat;
      X, Y  : Mpfloat;
      Rnd   : Rounding := RNDEF);
   --  Set `Rop` to the arc-tangent2 of `Y` and `X`, rounded in the direction
   --  `Rnd`. Atan2 (Y, 0) does not raise any floating-point exception, see the
   --  official MPFR documentation.

   procedure Cosh
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Sinh
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Tanh
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the hyperbolic cosine, sine or tangent of `Op`, rounded in
   --  the direction `Rnd`.

   procedure Sinh_Cosh
     (Sop, Cop : in out Mpfloat;
      Op       : Mpfloat;
      Rnd      : Rounding := RNDEF);
   --  Set simultaneously `Sop` to the hyperbolic sine of `Op` and `Cop` to the
   --  hyperbolic cosine of `Op`, rounded in the direction `Rnd` with the
   --  corresponding precision of `Sop` and `Cop`, which must be different
   --  variables.

   procedure Sech
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Csch
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Coth
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the hyperbolic secant of `Op`, cosecant of `Op`, cotangent
   --  of `Op`, rounded in the direction `Rnd`.

   procedure Acosh
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Asinh
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Atanh
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the inverse hyperbolic cosine, sine or tangent of `Op`,
   --  rounded in the direction `Rnd`.

   procedure Eint
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the exponential integral of `Op`, rounded in the
   --  direction `Rnd`.

   procedure Li2
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to real part of the dilogarithm of `Op`, rounded in the
   --  direction `Rnd`.

   procedure Gamma
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Gamma_Inc
     (Rop     : in out Mpfloat;
      Op, Op2 : Mpfloat;
      Rnd     : Rounding := RNDEF);
   --  Set `Rop` to the value of the Gamma function on `Op`, resp. the
   --  incomplete Gamma function on `Op` and `Op2`, rounded in the direction
   --  `Rnd`. (In the literature, Gamma_Inc is called upper incomplete Gamma
   --  function, or sometimes complementary incomplete Gamma function). For
   --  Gamma (and Gamma_Inc when `Op2` is zero), when `Op` is a negative
   --  integer, `Rop` is set to NaN.

   procedure Lngamma
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the value of the logarithm of the Gamma function on `Op`,
   --  rounded in the direction `Rnd`. When `Op` is 1 or 2, set `Rop` to +0
   --  (in all rounding modes). When `Op` is an infinity or a nonpositive
   --  integer, set `Rop` to +Inf, following the general rules on special
   --  values. When -2k-1 < `Op` < -2k, k being a nonnegative integer, set
   --  `Rop` to NaN. See also Lgamma.

   procedure Lgamma
     (Rop   : in out Mpfloat;
      Signp : in out Sign;
      Op    : Mpfloat;
      Rnd   : Rounding := RNDEF);
   --  Set `Rop` to the value of the logarithm of the absolute value of the
   --  Gamma function on `Op`, rounded in the direction `Rnd`. The sign (`Pos`
   --  or `Neg`) of Gamma(`Op`) is returned in the object pointed to by
   --  `Signp`. When `Op` is 1 or 2, set `Rop` to +0 (in all rounding modes).
   --  When `Op` is an infinity or a nonpositive integer, set `Rop` to +Inf.
   --  When `Op` is NaN, -Inf or a negative integer, `Signp` is undefined,
   --  and when `Op` is +/-0, `Signp` is the sign of the zero.

   procedure Digamma
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the value of the Digamma (sometimes also called Psi)
   --  function on `Op`, rounded in the direction `Rnd`. When `Op` is a
   --  negative integer, set `Rop` to NaN.

   procedure Beta
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF);
   --  Set `Rop`to the value of the Beta function at arguments `Op1` and `Op2`.

   procedure Zeta
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Zeta
     (Rop : in out Mpfloat;
      Op  : Long_Integer;
      Rnd : Rounding := RNDEF)
   with
     Pre => Op >= 0;
   --  Set `Rop` to the value of the Riemann Zeta function on `Op`, rounded in
   --  the direction `Rnd`.

   procedure Erf
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Erfc
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the value of the error function on `Op` (resp. the
   --  complementary error function on `Op`) rounded in the direction `Rnd`.

   procedure J0
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure J1
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Jn
     (Rop : in out Mpfloat;
      N   : Long_Integer;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the value of the first kind Bessel function of order 0,
   --  (resp. 1 and n) on `Op`, rounded in the direction `Rnd`. When `Op` is
   --  NaN, `Rop` is always set to NaN. When `Op` is plus or minus Infinity,
   --  `Rop` is set to +0. When `Op` is zero, and `N` is not zero, `Rop` is
   --  set to +0 or -0 depending on the parity and sign of `N`, and the sign
   --  of `Op`.

   procedure Y0
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Y1
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Yn
     (Rop : in out Mpfloat;
      N   : Long_Integer;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the value of the second kind Bessel function of order 0
   --  (resp. 1 and n) on `Op`, rounded in the direction `Rnd`. When `Op` is
   --  NaN or negative, `Rop` is always set to NaN. When `Op` is +Inf, `Rop`
   --  is set to +0. When `Op` is zero, `Rop` is set to +Inf or -Inf depending
   --  on the parity and sign of `N`.

   procedure Agm
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF);
   --  Set `Rop` to the arithmetic-geometric mean of `Op1` and `Op2`, rounded
   --  in the direction `Rnd`.

   procedure Ai
     (Rop : in out Mpfloat;
      X   : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the value of the Airy function Ai on `X`, rounded in the
   --  direction `Rnd`. When `X` is NaN, `Rop` is always set to NaN. When `X`
   --  is +Inf or -Inf, `Rop` is +0.

   procedure Const_Log2
     (Rop : in out Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Const_Pi
     (Rop : in out Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Const_Euler
     (Rop : in out Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Const_Catalan
     (Rop : in out Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the logarithm of 2, the value of Pi, of Euler’s constant
   --  0.577..., of Catalan’s constant 0.915..., respectively, rounded in the
   --  direction `Rnd`.

   procedure Out_Str
     (Stream : File_Type;
      Op     : Mpfloat;
      Base   : Admpfr.Base := 10;
      Rnd    : Rounding := RNDEF);
   --  Output `Op` on stream `Stream` as a text string in base `abs (Base)`,
   --  rounded in the direction `Rnd`.

   procedure Inp_Str
     (Op     : in out Mpfloat;
      Stream : File_Type;
      Base   : Admpfr.Base := 10;
      Rnd    : Rounding := RNDEF);
   --  Input a string in base `Base` from stream `Stream`, rounded in the
   --  direction `Rnd`, and put the read float in `Rop`.

   procedure Fpif_Export (Stream : File_Type; Op : Mpfloat);
   --  Export the number `Op` to the stream `Stream` in a floating-point
   --  interchange format.

   procedure Fpif_Import (Op : in out Mpfloat; Stream : File_Type);
   --  Import the number `Op` from the stream `Stream` in a floating-point
   --  interchange format.

   procedure Dump (Op : Mpfloat);
   --  Output `Op` on stdout in some unspecified format, then a newline
   --  character. This function is mainly for debugging purpose.

   function Prec_Min return Precision is (Precision'First);
   --  Return the minimum number of bits that can be used to represent the
   --  significand of a `Mpfloat`.

   function Prec_Max return Precision is (Precision'Last);
   --  Return the maximum number of bits that can be used to represent the
   --  significand of a `Mpfloat`.

   procedure Set_Default_Prec (Prec : Precision);
   --  Set the default precision to be exactly `Prec` bits, where `Prec` can be
   --  any integer between `Prec_Min` and `Prec_Max`. The precision of a
   --  variable means the number of bits used to store its significand. All
   --  subsequent `Mpfloat` object creations will use this precision, but
   --  previously initialized variables are unaffected. The default precision
   --  is set to 53 bits initially.

   function Get_Prec (X : Mpfloat) return Precision;
   --  Return the precision of `X`, i.e., the number of bits used to store its
   --  significand.

   procedure Set_Prec (X : Mpfloat; Prec : Precision);
   --  Set the precision of `X` to be exactly `Prec` bits, and set its value to
   --  NaN. The previous value stored in `X` is lost. The precision `Prec` can
   --  be any value between `Prec_Min` and `Prec_Max`. In case you want to keep
   --  the previous value stored in x, use `Prec_Round` (TODO) instead.

   procedure Printf (Template : String;
                     X        : Mpfloat;
                     R        : Rounding := RNDEF);
   --  Format string `Template`. The format specification accepted by
   --  `Printf` is an extension of the printf one. See the mpfr
   --  documentation for a detailed description of the `Template` formats.
   --
   --  WARNING: `Printf` is not a direct binding to `mpfr_printf`. It can
   --  only prints one Mpfloat value and the value/rounding parameters are
   --  inverted. So, if one wants to print `X` in scientific notation, using
   --  the default precision, rounded toward +Inf, use:
   --    `Printf ("%.R*e" & ASCII.LF, X, R)`.

   function Sprintf (Template : String;
                     X        : Mpfloat;
                     R        : Rounding := RNDEF) return String;
   --  Format string `Template`. `Sprintf` has the same behavior than
   --  `Printf` (it is based on `mpfr_sprintf`). It returns the formated
   --  `Template` as a string.

   procedure Rint
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Ceil (Rop : in out Mpfloat; Op : Mpfloat);
   procedure Floor (Rop : in out Mpfloat; Op : Mpfloat);
   procedure Round (Rop : in out Mpfloat; Op : Mpfloat);
   procedure Roundeven (Rop : in out Mpfloat; Op : Mpfloat);
   procedure Trunc (Rop : in out Mpfloat; Op : Mpfloat);
   --  Set `Rop` to `Op` rounded to an integer. `Rint` rounds to the nearest
   --  representable integer in the given direction `Rnd`, and the other five
   --  functions behave in a similar way with some fixed rounding mode:
   --  * `Ceil`: to the next higher or equal representable integer
   --    (like `Rint` with `RNDU`);
   --  * `Floor` to the next lower or equal representable integer
   --    (like `Rint` with `RNDD`);
   --  * `Round` to the nearest representable integer, rounding halfway cases
   --    away from zero (as in the roundTiesToAway mode of IEEE 754-2008);
   --  * `Roundeven` to the nearest representable integer, rounding halfway
   --    cases with the even-rounding rule (like `Rint` with `RNDN`);
   --  * `Trunc` to the next representable integer toward zero
   --    (like `Rint` with `RNDZ`.

   procedure Rint_Ceil
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Rint_Floor
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Rint_Round
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Rint_Roundeven
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   procedure Rint_Trunc
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to `Op` rounded to an integer:
   --  * `Rint_Ceil`: to the next higher or equal integer;
   --  * `Rint_Floor`: to the next lower or equal integer;
   --  * `Rint_Round`: to the nearest integer, rounding halfway cases away from
   --    zero;
   --  * `Rint_Roundeven`: to the nearest integer, rounding halfway cases to
   --    the nearest even integer;
   --  * `Rint_Trunc`: to the next integer toward zero.
   --  Contrary to `Rint`, those functions do perform a double rounding: first
   --  `Op` is rounded to the nearest integer in the direction given by the
   --  function name, then this nearest integer (if not representable) is
   --  rounded in the given direction `Rnd`.

   procedure Frac
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);
   --  Set `Rop` to the fractional part of `Op`, having the same sign as `Op`,
   --  rounded in the direction `Rnd`. When `Op` is an integer or an infinity,
   --  set `Rop` to zero with the same sign as `Op`.

   procedure Modf
     (Iop, Fop : in out Mpfloat;
      Op       : Mpfloat;
      Rnd      : Rounding := RNDEF);
   --  Set simultaneously `Iop` to the integral part of `Op` and `Fop` to the
   --  fractional part of `Op`, rounded in the direction `Rnd` with the
   --  corresponding precision of `Iop` and `Fop` (equivalent to
   --  `Trunc (Iop, Op, Rnd)` and `Frac (Fop, Op, Rnd)`).

   procedure Fmod
     (R    : in out Mpfloat;
      X, Y : Mpfloat;
      Rnd  : Rounding := RNDEF);
   procedure Fmodquo
     (R    : in out Mpfloat;
      Q    : in out Long_Integer;
      X, Y : Mpfloat;
      Rnd  : Rounding := RNDEF);
   procedure Remainder
     (R    : in out Mpfloat;
      X, Y : Mpfloat;
      Rnd  : Rounding := RNDEF);
   procedure Remquo
     (R    : in out Mpfloat;
      Q    : in out Long_Integer;
      X, Y : Mpfloat;
      Rnd  : Rounding := RNDEF);
   --  Set `R` to the value of `X - nY`, rounded according to the direction
   --  `Rnd`, where n is the integer quotient of `X` divided by `Y`, defined as
   --  follows: n is rounded toward zero for `Fmod` and `Fmodquo`, and to the
   --  nearest integer (ties rounded to even) for `Remainder` and `Remquo`.
   --
   --  Additionally, `Fmodquo` and `Remquo` store the low significant bits from
   --  the quotient n in `Q` (more precisely the number of bits in a
   --  Long_Integer minus one), with the sign of `X` divided by `Y` (except if
   --  those low bits are all zero, in which case zero is returned). Note that
   --  `X` may be so large in magnitude relative to `Y` that an exact
   --  representation of the quotient is not practical. The `Remainder` and
   --  `Remquo` functions are useful for additive argument reduction.

   function Is_Integer (Op : Mpfloat) return Boolean;
   --  Return whether `Op`is an integer.

   procedure Set_Default_Rounding_Mode (Rnd : Rounding);
   --  Set the default rounding mode to `Rnd`. The default rounding mode is
   --  RNDN initially.

   procedure Prec_Round
     (X    : in out Mpfloat;
      Prec : Precision;
      Rnd  : Rounding := RNDEF);
   --  Round `X` according to `Rnd` with precision `Prec`.

   function Can_Round
     (B          : Mpfloat;
      Err        : in out Exponent;
      Rnd1, Rnd2 : Rounding;
      Prec       : Precision) return Boolean;
   --  Assuming `B` is an approximation of an unknown number x in the
   --  direction `Rnd1` with error at most two to the power E(`B`)-`Err``
   --  where E(`B`) is the exponent of `B`, return a True if one is able to
   --  round correctly x to precision `Prec` with the direction `Rnd2`
   --  assuming an unbounded exponent range, and False otherwise (including
   --  for NaN and Inf). In other words, if the error on `B` is bounded by two
   --  to the power k ulps, and `B` has precision `Prec`, you should give
   --  `Err`=`Prec`-k.

   function Min_Prec (X : Mpfloat) return Precision;
   --  Return the minimal number of bits required to store the significand of
   --  `X`, raise the Empty_Prec exception for special values, including 0.

   procedure Nexttoward (X : in out Mpfloat; Y : Mpfloat);
   --  If `X` or `Y` is NaN, set `X` to NaN; note that the NaN flag is set as
   --  usual. If `X` and `Y` are equal, `X` is unchanged. Otherwise, if `X` is
   --  different from `Y`, replace `X` by the next floating-point number (with
   --  the precision of `X` and the current exponent range) in the direction of
   --  `Y` (the infinite values are seen as the smallest and largest
   --  floating-point numbers). If the result is zero, it keeps the same sign.

   procedure Nextabove (X : in out Mpfloat);
   procedure Nextbelow (X : in out Mpfloat);
   --  Equivalent to `Nexttoward` where `Y` is plus infinity (resp. minus
   --  infinity).

   procedure Min
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF);
   procedure Max
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF);
   --  Set `Rop`to the minimum (resp. maximum) of `Op1` and `Op2`. If `Op1` and
   --  `Op2` are both NaN, then `Rop` is set to NaN. If `Op1` or `Op2` is NaN,
   --  then `Rop` is set to the numeric value. If `Op1` and `Op2` are zeros of
   --  different signs, then `Rop` is set to -0 (resp. +0).

   function Get_Exp (X : Mpfloat) return Exponent;
   --  Return the exponent of `X`, assuming that `X` is a non-zero ordinary
   --  number and the significand is considered in [1/2,1).

   procedure Set_Exp (X : in out Mpfloat; E : Exponent);
   --  Set the exponent of `X` to `E` if `X` is a non-zero ordinary number and
   --  `E` is in the current exponent range.

   function Signbit (Op : Mpfloat) return Sign;
   --  Return the value of the sign bit of `X`.

   procedure Setsign
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      S   : Sign;
      Rnd : Rounding := RNDEF);
   --  Set the value of `Rop` from `Op`, rounded toward the given direction
   --  `Rnd`, then set its sign bit to `S`.

   procedure Copysign
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF);
   --  Set the value of `Rop` from `Op1`, rounded toward the given direction
   --  `Rnd`, then set its sign bit to that of `Op2`. This function is
   --  equivalent to `Setsign (Rop, Op1, Signbit (Op2), Rnd)`.

   function Get_Version return String;
   --  Return the MPFR version as a String.

   function Get_Emin return Exponent;
   function Get_Emax return Exponent;
   --  Return the (current) smallest and largest exponents allowed for a
   --  floating-point variable.

   procedure Set_Emin (Exp : Exponent);
   procedure Set_Emax (Exp : Exponent);
   --  Set the smallest and largest exponents allowed for a floating-point
   --  variable.

   function Get_Emin_Min return Exponent;
   function Get_Emin_Max return Exponent;
   function Get_Emax_Min return Exponent;
   function Get_Emax_Max return Exponent;
   --  Return the minimum and maximum of the exponents allowed for `Set_Emin`
   --  and `Set_Emax` respectively.

   procedure Check_Range (X : in out Mpfloat; Rnd : Rounding := RNDEF);
   --  This function assumes that `X` is the correctly rounded value of some
   --  real value `Y` in the direction `Rnd` and some extended exponent range.
   --  This procedure modifies `X` if needed to be in the current range of
   --  acceptable values: It generates an underflow or an overflow if the
   --  exponent of `X` is outside the current allowed range.

   procedure Subnormalize (X : in out Mpfloat; Rnd : Rounding := RNDEF);
   --  Rounds `X`emulating subnormal number arithmetic according to rounding
   --  mode `Rnd`.

   --  This is an example of how to emulate binary double IEEE 754 arithmetic
   --  (binary64 in IEEE 754-2008) using MPFR:
   --
   --  Set_Default_Prec (53);
   --  Set_Emin (-1073);
   --  Set_Emax (1024);
   --
   --  declare
   --     Xa, Xb : Mpfloat;
   --     A : Long_Float := 1.1235e-1021;
   --     B : constant Long_Float := 34.3;
   --  begin
   --     Xa.Set (A);
   --     Xb.Set (B);
   --     A := A / B;
   --     Xa.Div (Xa, Xb);
   --     Xa.Subnormalize; -- new ternary value
   --  end;
   --
   --  Below is another example showing how to emulate fixed-point arithmetic
   --  in a specific case. Here we compute the sine of the integers 1 to 17
   --  with a result in a fixed-point arithmetic rounded at 2 power -42 (using
   --  the fact that the result is at most 1 in absolute value):
   --
   --  Set_Emin (-41);
   --  declare
   --     X : Mpfloat (42);
   --  begin
   --     for I in 1 .. 17 loop
   --        X.Set (Long_Integer (I));
   --        X.Sin (X, RNDZ);
   --        X.Subnormalize (RNDZ);
   --        X.Dump;
   --     end loop;
   --  end;

   procedure Clear_Underflow;
   procedure Clear_Overflow;
   procedure Clear_Divby0;
   procedure Clear_Nanflag;
   procedure Clear_Inexflag;
   procedure Clear_Erangeflag;
   --  Clear (lower) the underflow, overflow, divide-by-zero, invalid, inexact
   --  and erange flags.

   procedure Clear_flags;
   --  Clear (lower) all global flags.

   procedure Set_Underflow;
   procedure Set_Overflow;
   procedure Set_Divby0;
   procedure Set_Nanflag;
   procedure Set_Inexflag;
   procedure Set_Erangeflag;
   --  Set (raise) the underflow, overflow, divide-by-zero, invalid, inexact
   --  and erange flags.

   function Underflow return Boolean;
   function Overflow return Boolean;
   function Divby0 return Boolean;
   function Nanflag return Boolean;
   function Inexflag return Boolean;
   function Erangeflag return Boolean;
   --  Return weither the corresponding flag (underflow, overflow,
   --  divide-by-zero, invalid, inexact, erange) is set.

   procedure Flags_Clear (Mask : Flags_Mask);
   --  Clear (lower) the group of flags specified by mask.

   procedure Flags_Set (Mask : Flags_Mask);
   --  Set (raise) the group of flags specified by mask.

   function Flags_Test (Mask : Flags_Mask) return Flags_Mask;
   --  Return the flags specified by mask. To test whether any flag from mask
   --  is set, check is the returned flags is empty. You can then test
   --  individual flags with a case statement. Example:
   --
   --  T : Flags_Mask := Flags_Test (Flags_Mask'[Underflow, Overflow]);
   --  ...
   --  for F of T loop
   --     case F is
   --        when Underflow =>
   --           ...
   --        when Overflow =>
   --           ...
   --        when others => null;
   --     end case;
   --  end loop;

   function Flags_Save return Flags_Mask;
   --  Return all the flags. It is equivalent to `Flags_Test (Flags_All).

   procedure Flags_Restore (Flags, Mask : Flags_Mask);
   --  Restore the flags specified by `Mask` to their state represented in
   --  `Flags`.

   Failure : exception;
   Empty_Prec : exception;

private

   subtype mpfr_exp_t is long;
   subtype mpfr_prec_t is long;
   subtype mpfr_rnd_t is int;
   subtype mpfr_sign_t is int;
   subtype mpfr_flags_t is unsigned;

   type mpfr_t is limited record
      Mpfr_Prec : mpfr_prec_t;
      Mpfr_Sign : mpfr_sign_t;
      Mpfr_Exp  : mpfr_exp_t;
      Mp_Limb   : System.Address;
   end record with Convention => C;
   --  Be careful, mpfr_t may be not portable since mpfr_prec_t, and mpfr_exp_t
   --  can be of a different type depending on the machine the library has been
   --  built for. mpfr_t record must stricty stick to the C mpfr_t struct.

   type Mpfloat (Prec : Precision := Get_Default_Prec) is
     new Limited_Controlled with
      record
         Value   : aliased mpfr_t;
         Ternary : Ternary_Value := NOT_SET;
      end record;

   procedure Initialize (X : in out Mpfloat);
   procedure Finalize   (X : in out Mpfloat);

   procedure Reformat_Printf_Args (T : in out String; R : in out Rounding);
   function To_Ternary_Value (T : int) return Ternary_Value;
   function To_Compare (C : int) return Compare;

   generic
      with function mpfr_fn (Rop : access constant mpfr_t;
                             Op  : access constant mpfr_t;
                             Rnd : mpfr_rnd_t) return int;
   procedure Mpfr_Fn_1
     (Rop : in out Mpfloat;
      Op  : Mpfloat;
      Rnd : Rounding := RNDEF);

   generic
      with function mpfr_fn (Rop : access constant mpfr_t;
                             Op1 : access constant mpfr_t;
                             Op2 : access constant mpfr_t;
                             Rnd : mpfr_rnd_t) return int;
   procedure Mpfr_Fn_2
     (Rop      : in out Mpfloat;
      Op1, Op2 : Mpfloat;
      Rnd      : Rounding := RNDEF);

   generic
      with function mpfr_fn (Rop : access constant mpfr_t;
                             Op1 : access constant mpfr_t;
                             Op2 : long;
                             Rnd : mpfr_rnd_t) return int;
   procedure Mpfr_Fn_2_I
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Integer;
      Rnd : Rounding := RNDEF);

   generic
      with function mpfr_fn (Rop : access constant mpfr_t;
                             Op1 : long;
                             Op2 : access constant mpfr_t;
                             Rnd : mpfr_rnd_t) return int;
   procedure Mpfr_Fn_2_Ib
     (Rop : in out Mpfloat;
      Op1 : Long_Integer;
      Op2 : Mpfloat;
      Rnd : Rounding := RNDEF);

   generic
      with function mpfr_fn (Rop : access constant mpfr_t;
                             Op1 : access constant mpfr_t;
                             Op2 : double;
                             Rnd : mpfr_rnd_t) return int;
   procedure Mpfr_Fn_2_F
     (Rop : in out Mpfloat;
      Op1 : Mpfloat;
      Op2 : Long_Float;
      Rnd : Rounding := RNDEF);

   generic
      with function mpfr_fn (Rop : access constant mpfr_t;
                             Op1 : double;
                             Op2 : access constant mpfr_t;
                             Rnd : mpfr_rnd_t) return int;
   procedure Mpfr_Fn_2_Fb
     (Rop : in out Mpfloat;
      Op1 : Long_Float;
      Op2 : Mpfloat;
      Rnd : Rounding := RNDEF);

   function To_Mpfr_Flags_T (M : Flags_Mask) return mpfr_flags_t;
   function To_Flags_Mask (M : mpfr_flags_t) return Flags_Mask;

end Admpfr;
