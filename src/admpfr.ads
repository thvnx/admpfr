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
with Interfaces.C.Strings; use Interfaces.C.Strings;

with System;


package AdMPFR is
   type Mpfr_Float is tagged limited private;

   type Base_T is range 2 .. 62;

   type Rnd_T is (Rndn, Rndd, Rndu, Rndz, Rnda, Rndf);

   procedure Set (Rop : out Mpfr_Float; S : String;
                  Base : Base_T := 10; Rnd : Rnd_T := Rndn);

   function To_String (X : Mpfr_Float; Base : Base_T := 10;
                       Rnd : Rnd_T := Rndn) return String;

   Failure : exception;

private
   type Prec_T is new Long;
   type Exp_T is new Long;

   -- Warning: this may not be portable since the mpfr_prec_t, and mpfr_exp_t
   -- can be of a different type depending on the machine the library has been
   -- built for.
   type Mpfr_T is limited record
      Mpfr_Prec_T : Prec_T;
      Mpfr_Sign_T : Int;
      Mpfr_Exp_T  : Exp_T;
      Mp_Limb_T   : System.Address;
   end record with Convention => C;

   type Mpfr_Float is new Limited_Controlled with
      record
         Value : aliased Mpfr_T;
      end record;

   function Rnd_T_Pos_To_Int (Rnd : Rnd_T) return Int;

   procedure Initialize (X : in out Mpfr_Float);
   procedure Finalize   (X : in out Mpfr_float);
end AdMPFR;
