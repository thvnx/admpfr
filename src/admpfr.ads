with Ada.Finalization;     use Ada.Finalization;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with System;


package AdMPFR is
   type Mpfr_T is limited private;
   type Mpfr_Float is tagged limited private;

   -- NOTE: used as simple test for now, to be removed
   procedure Main;

   -- TODO: create enum/range type for Base
   -- TODO: add rounding parameter
   procedure Set (Rop : out Mpfr_Float; S : String; Base : int := 10);

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
