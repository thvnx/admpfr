with Ada.Finalization;     use Ada.Finalization;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with System;


package AdMPFR is
   type Mpfr_T is limited private;
   type Mpfr_Float is tagged limited private;

   type Base_T is range 2 .. 62;

   -- NOTE: used as simple test for now, to be removed
   procedure Main;

   -- TODO: add rounding parameter
   procedure Set (Rop : out Mpfr_Float; S : String; Base : Base_T := 10);

   Failure : exception;

private
   type Mpfr_t is new System.Address;
   type Mpfr_Float is new Limited_Controlled with
      record
         Value : aliased Mpfr_T;
      end record;

   procedure Initialize (X : in out Mpfr_Float);
   procedure Finalize   (X : in out Mpfr_float);
end AdMPFR;
