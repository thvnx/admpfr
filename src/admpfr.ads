with Ada.Finalization;
with Interfaces.C;
with System;

with Interfaces.C.Strings; use Interfaces.C.Strings;

package Admpfr is
   type mpfr_t is limited private;
   type Mpfr_Float is tagged limited private;

   type Int is new Interfaces.C.int;

   procedure Main;

   procedure Set
     (M : out Mpfr_Float;
      To   : String;
      Base : Int := 10);

   Failure : exception;

private
   procedure mpfr_init (m : access mpfr_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_init";
   -- Import, Convention, and External_Name aspects could be replaced by a
   -- single pragma: `pragma Import (C, mpfr_init, "mpfr_init")`.

   procedure mpfr_clear (m : access mpfr_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_clear";

   function mpfr_set_str (m : access mpfr_t;
                          str : chars_ptr;
                          base : Int;
                          rnd : Int)
                         return Int  with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_str";

   type mpfr_t is record
      m : System.Address;
   end record with
     Convention => C;
   -- Convention aspect could be replaced by pragma `Convention (C, mpfr_t)`

   type Mpfr_Float is new Ada.Finalization.Limited_Controlled with
      record
         Value : aliased mpfr_t;
      end record;

   procedure Initialize (M : in out Mpfr_Float);
   procedure Finalize   (M : in out Mpfr_float);
end Admpfr;
