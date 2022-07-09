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

with Ada.Finalization;     use Ada.Finalization;
with Interfaces.C;         use Interfaces.C;

with System;

package Admpfr is

   type Mpfloat is tagged limited private;

   type Base is range 2 .. 62;

   type Rounding is (Rndn, Rndz, Rndu, Rndd, Rnda, Rndf);
   --  Stick to the order declared in mpfr.h's mpfr_rnd_t enum

   type Precision is new Standard.Long_Integer;

   procedure Set
     (Rop  : out Mpfloat;
      S    : String;
      Base : Admpfr.Base := 10;
      Rnd  : Rounding  := Rndn);

   function To_String
     (X    : Mpfloat;
      Base : Admpfr.Base := 10;
      Rnd  : Rounding  := Rndn) return String;

   function Get_Prec (X : Mpfloat) return Precision;
   procedure Set_Prec (X : Mpfloat; Prec : Precision);

   procedure Mpfr_Printf (Template : String;
                          X : Mpfloat;
                          R : Rounding := Rndn);

   Failure : exception;

private

   type mpfr_exp_t is new long;
   type mpfr_prec_t is new long;
   type mpfr_rnd_t is new int;
   type mpfr_sign_t is new int;

   type mpfr_t is limited record
      Mpfr_Prec : mpfr_prec_t;
      Mpfr_Sign : mpfr_sign_t;
      Mpfr_Exp  : mpfr_exp_t;
      Mp_Limb   : System.Address;
   end record with Convention => C;
   --  Be careful, mpfr_t may be not portable since mpfr_prec_t, and mpfr_exp_t
   --  can be of a different type depending on the machine the library has been
   --  built for. mpfr_t record must stricty stick to the C mpfr_t struct.

   type Mpfloat is new Limited_Controlled with
      record
         Value : aliased mpfr_t;
      end record;

   function Rounding_To_Mpfr_Rnd_T (Rnd : Rounding) return mpfr_rnd_t;

   procedure Initialize (X : in out Mpfloat);
   procedure Finalize   (X : in out Mpfloat);

end Admpfr;
