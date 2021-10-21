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
   procedure mpfr_init (X : access Mpfr_T) with
     -- Import, Convention, and External_Name aspects could be replaced by a
     -- single pragma: `pragma Import (C, mpfr_init, "mpfr_init")`.
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_init";

   procedure mpfr_clear (X : access Mpfr_T) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_clear";

   function mpfr_set_str (Rop : access Mpfr_T; S : chars_ptr;
                          Base : int; Rnd : int) return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_str";

   type Mpfr_t is new System.Address;
   type Mpfr_Float is new Limited_Controlled with
      record
         Value : aliased Mpfr_T;
      end record;

   procedure Initialize (X : in out Mpfr_Float);
   procedure Finalize   (X : in out Mpfr_float);
end AdMPFR;
