with Ada.Finalization;
with System;

package Admpfr is
   type mpfr_t is limited private;
   type Mpfr_Float is tagged limited private;

   procedure Main;

private
   procedure mpfr_init (m : access mpfr_t);
   pragma Import (C, mpfr_init, "mpfr_init");

   procedure mpfr_clear (m : access mpfr_t);
   pragma Import (C, mpfr_clear, "mpfr_clear");

   type mpfr_t is record
      m : System.Address;
   end record;
   pragma Convention (C, mpfr_t);

   type Mpfr_Float is new Ada.Finalization.Limited_Controlled with
      record
         Value : aliased mpfr_t;
      end record;

   procedure Initialize (M : in out Mpfr_Float);
   procedure Finalize   (M : in out Mpfr_float);
end Admpfr;
