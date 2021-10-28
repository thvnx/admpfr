with Ada.Finalization;     use Ada.Finalization;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with System;


package AdMPFR is
   type Mpfr_T is limited private;
   type Mpfr_Float is tagged limited private;

   type Base_T is range 2 .. 62;

   type Rnd_T is (Rndn, Rndd, Rndu, Rndz, Rnda, Rndf);

   procedure Set (Rop : out Mpfr_Float; S : String;
                  Base : Base_T := 10; Rnd : Rnd_T := Rndn);

   Failure : exception;

private
   type Mpfr_t is new System.Address;
   type Mpfr_Float is new Limited_Controlled with
      record
         Value : aliased Mpfr_T;
      end record;

   function Rnd_T_Pos_To_Int (Rnd : Rnd_T) return Int;

   procedure Initialize (X : in out Mpfr_Float);
   procedure Finalize   (X : in out Mpfr_float);
end AdMPFR;
