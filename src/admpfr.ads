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

with Ada.Finalization; use Ada.Finalization;
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

   procedure Set (Rop : out Mpfloat; Op : Mpfloat; Rnd : Rounding := RNDN);
   --  Set the value of `Rop` from `Op`, rounded toward the given direction
   --  `Rnd`. The sign of a NaN is propagated in order to mimic the IEEE 754
   --  copy operation. But contrary to IEEE 754, the NaN flag is set as usual.

   procedure Set
     (Rop : out Mpfloat;
      Op  : Long_Integer;
      Rnd : Rounding := RNDN);
   --  Set the value of `Rop` from `Op`, rounded toward the given direction
   --  `Rnd`. The sign of a NaN is propagated in order to mimic the IEEE 754
   --  copy operation. But contrary to IEEE 754, the NaN flag is set as usual.
   --  The input 0 is converted to +0.

   procedure Set (Rop : out Mpfloat; Op : Float; Rnd : Rounding := RNDN);
   procedure Set (Rop : out Mpfloat; Op : Long_Float; Rnd : Rounding := RNDN);
   procedure Set
     (Rop : out Mpfloat;
      Op  : Long_Long_Float;
      Rnd : Rounding := RNDN);
   --  Set the value of `Rop` from `Op`, rounded toward the given direction
   --  `Rnd`. The sign of a NaN is propagated in order to mimic the IEEE 754
   --  copy operation. But contrary to IEEE 754, the NaN flag is set as usual.

   procedure Set
     (Rop : out Mpfloat;
      Op  : Long_Integer;
      E   : Exponent;
      Rnd : Rounding := RNDN);
   --  Set the value of `Rop` from `Op` multiplied by two to the power `E`,
   --  rounded toward the given direction `Rnd`. Note that the input 0 is
   --  converted to +0.

   procedure Set
     (Rop  : out Mpfloat;
      S    : String;
      Base : Admpfr.Base := 10;
      Rnd  : Rounding    := RNDN)
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

   function Get_Float (Op : Mpfloat; Rnd : Rounding := RNDN) return Float;
   function Get_Long_Float
     (Op  : Mpfloat;
      Rnd : Rounding := RNDN) return Long_Float;
   function Get_Long_Long_Float
     (Op  : Mpfloat;
      Rnd : Rounding := RNDN) return Long_Long_Float;
   --  Convert `Op` to a Float (respectively Long_Float, Long_Long_Float) using
   --  the rounding mode `Rnd`. If `Op` is NaN or an Inf, a Failure exception
   --  is raised. If `Op` is zero, these functions return a zero, trying to
   --  preserve its sign, if possible.

   function Get_Long_Integer
     (Op  : Mpfloat;
      Rnd : Rounding := RNDN) return Long_Integer;
   --  Convert `op` to a Long_Integer after rounding it to an integer with
   --  respect to `Rnd`. If `Op` is NaN, 0 is returned and the erange flag
   --  is set. If `Op` is too big for the return type, the function returns
   --  the maximum or the minimum of the corresponding C type, depending on
   --  the direction of the overflow; the erange flag is set too. When there
   --  is no such range error, if the return value differs from `Op`, i.e.,
   --  if `Op` is not an integer, the inexact flag is set. See also
   --  `Fits_Long_Integer` (TODO).

   function Get_Long_Float
     (Op  : Mpfloat;
      Exp : out Long_Integer;
      Rnd : Rounding := RNDN) return Long_Float;
   function Get_Long_Long_Float
     (Op  : Mpfloat;
      Exp : out Long_Integer;
      Rnd : Rounding := RNDN) return Long_Long_Float;
   --  Return d and set `Exp` such that 0.5<=abs(d)<1 and d times 2 raised to
   --  `Exp` equals `Op` rounded to Long_Float (resp. Long_Long_Float)
   --  precision, using the given rounding mode. If `Op` is zero, then a zero
   --  of the same sign is returned, and `Exp` is set to 0. If `Op` is NaN or
   --  an infinity, then a Failure exception is raised.

   function To_String
     (X    : Mpfloat;
      Base : Admpfr.Base := 10;
      Rnd  : Rounding    := RNDN) return String
   with
     Pre => Base /= 0;
   --  Convert `X` to a String in base `Base` rounded in the direction `Rnd`.
   --  This function is based on mpfr_get_str and has no real equivalent in the
   --  original C library.
   --
   --  Default behavior mimics mpfr_printf("%.RNe", X), (at least for base 10)!

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

   procedure Mpfr_Printf (Template : String;
                          X        : Mpfloat;
                          R        : Rounding := RNDN);
   --  Format string `Template`. The format specification accepted by
   --  `Mpfr_Printf` is an extension of the printf one. See the mpfr
   --  documentation for a detailed description of the `Template` formats.
   --
   --  WARNING: `Mpfr_Printf` is not a direct binding to `mpfr_printf`. It can
   --  only prints one Mpfloat value and the value/rounding parameters are
   --  inverted. So, if one wants to print `X` in scientific notation, using
   --  the default precision, rounded toward +Inf, use:
   --    `Mpfr_Printf ("%.R*e" & ASCII.LF, X, R)`.

   function Mpfr_Sprintf (Template : String;
                          X        : Mpfloat;
                          R        : Rounding := RNDN) return String;
   --  Format string `Template`. `Mpfr_Sprintf` has the same behavior than
   --  `Mpfr_Printf` (it is based on `mpfr_sprintf`). It returns the formated
   --  `Template` as a string.

   Failure : exception;

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

end Admpfr;
