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

with System;

package Admpfr is

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

   procedure Set
     (Rop  : out Mpfloat;
      S    : String;
      Base : Admpfr.Base := 10;
      Rnd  : Rounding    := RNDN)
   with
     Pre => Base = 0 or Base > 1;
   --  Set `Rop` to the value of the string `S` in base `Base`, rounded in the
   --  direction `Rnd`. `Base` and `Rnd` are optionals, their default values
   --  are 10 and RNDN, respectively. See the documentation of
   --  `String_To_Mpfloat` (TODO) for a detailed description of the valid
   --  string formats and base values.
   --
   --  Note: it is preferable to use `Str_To_Mpfloat` if one wants to
   --  distinguish between an infinite rop value coming from an infinite s or
   --  from an overflow.

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
         Value : aliased mpfr_t;
      end record;

   procedure Initialize (X : in out Mpfloat);
   procedure Finalize   (X : in out Mpfloat);

   procedure Reformat_Printf_Args (T : in out String; R : in out Rounding);

end Admpfr;
