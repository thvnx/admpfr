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

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

package body Admpfr is

   --  Direct mpfr calls

   procedure mpfr_init (X : access mpfr_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_init";

   procedure mpfr_clear (X : access mpfr_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_clear";

   function mpfr_prec_min return mpfr_prec_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_prec_min";
   Prec_Min : constant Precision := Precision (mpfr_prec_min);

   function mpfr_prec_max return mpfr_prec_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_prec_max";
   Prec_Max : constant Precision := Precision (mpfr_prec_max);

   function mpfr_set_str
     (Rop  : access mpfr_t;
      S    : chars_ptr;
      Base : int;
      Rnd  : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_str";

   function mpfr_get_str
     (S      : System.Address;
      Expptr : System.Address;
      Base   : int;
      N      : size_t;
      Op     : access constant mpfr_t;
      Rnd    : mpfr_rnd_t) return chars_ptr
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_str";

   function mpfr_get_str_ndigits (Base : int;
                                  Prec : mpfr_prec_t) return size_t
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_str_ndigits";

   --  Calls to admpfr C wrappers

   function mpfr_get_prec (X : access constant mpfr_t) return mpfr_prec_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_prec";

   procedure mpfr_set_prec (X : access constant mpfr_t;
                            Prec : mpfr_prec_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_prec";

   ----------------------------
   -- Rounding_To_Mpfr_Rnd_T --
   ----------------------------

   function Rounding_To_Mpfr_Rnd_T (Rnd : Rounding) return mpfr_rnd_t is
      function c_stub (Rnd : mpfr_rnd_t) return mpfr_rnd_t
      with
        Import        => True,
        Convention    => C,
        External_Name => "rounding_to_mpfr_rnd_t";

      Res : mpfr_rnd_t;
   begin
      Res := c_stub (Rounding'Pos (Rnd));
      if Res < 0 then
         raise Failure with "invalid rounding mode";
      end if;
      return Res;
   end Rounding_To_Mpfr_Rnd_T;

   -----------------
   -- Mpfr_Printf --
   -----------------

   procedure Mpfr_Printf (Template : String; R : Rounding; X : Mpfloat) is
      function Printf_Stub (T : chars_ptr;
                            R : mpfr_rnd_t;
                            X : access constant mpfr_t) return int
      with
        Import        => True,
        Convention    => C,
        External_Name => "mpfr_printf_stub";

      Res : int;
   begin
      Res := Printf_Stub (New_String (Template),
                          Rounding_To_Mpfr_Rnd_T (R),
                          X.Value'Access);

      if Res < 0 then
         raise Failure with "mpfr_printf failure";
      end if;
   end Mpfr_Printf;

   -----------------
   -- Mpfr_Printf --
   -----------------

   procedure Mpfr_Printf (Template : String; X : Mpfloat) is
   begin
      Mpfr_Printf (Template, Rndn, X);
   end Mpfr_Printf;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (X : in out Mpfloat) is
   begin
      mpfr_init (X.Value'Access);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Mpfloat) is
   begin
      mpfr_clear (X.Value'Access);
   end Finalize;

   ---------
   -- Set --
   ---------

   procedure Set
     (Rop  : out Mpfloat;
      S    : String;
      Base : Admpfr.Base := 10;
      Rnd  : Rounding  := Rndn)
   is
      Result : int;
      Input  : chars_ptr := New_String (S);
   begin
      Result := mpfr_set_str
        (Rop.Value'Access, Input, int (Base),
         Rounding_To_Mpfr_Rnd_T (Rnd));
      Free (Input);
      if Result /= 0 then
         raise Failure with "mpfr_set_str failure";
      end if;
   end Set;

   --  TODO: Add 'Image attribute on Float type when GCC FSF will support
   --  Ada 2022, see:
   --  https://stackoverflow.com/questions/67969309/ada-customise-image.

   ---------------
   -- To_String --
   ---------------

   function To_String
     (X    : Mpfloat;
      Base : Admpfr.Base := 10;
      Rnd  : Rounding  := Rndn) return String
   is
      --  TODO: Rely on mpfr_get_str_ndigits for now but allows the user to set
      --  the number of digits to print by adding a parameter to this function.

      --  Default behavior mimics mpfr_printf("%.RNe", X),
      --  at least for base 10!

      Number_Digits : constant size_t := size_t'Max
        (mpfr_get_str_ndigits (int (Base), mpfr_get_prec (X.Value'Access)), 7);

      Significand_Buffer : String (1 .. Integer (Number_Digits + 2));
      Exponent           : mpfr_exp_t;

      Exponent_S, Number : Unbounded_String;
      Significand        : chars_ptr;
   begin
      Significand := mpfr_get_str
        (Significand_Buffer'Address,
         Exponent'Address,
         int (Base),
         Number_Digits,
         X.Value'Access,
         Rounding_To_Mpfr_Rnd_T (Rnd));

      --  Remove 1 if first digit is not zero as we'll insert the implicit
      --  radix point in the significand.

      if Significand_Buffer (1) /= '-' then
         if Significand_Buffer (1) /= '0' then
            Exponent := Exponent - 1;
         end if;
      else
         if Significand_Buffer (2) /= '0' then
            Exponent := Exponent - 1;
         end if;
      end if;

      --  Convert Exponent to a string

      Exponent_S := To_Unbounded_String (mpfr_exp_t'Image (Exponent));

      --  Format exponent as mpfr_printf("%RNe")

      if Length (Exponent_S) = 2 then
         Insert (Exponent_S, 2, "0");
      end if;

      --  Add '+' sign if exponent is zero or positive.

      if Exponent >= 0 then
         Overwrite (Exponent_S, 1, "+");
      end if;

      --  Concat significand and exponent

      Number := Value (Significand) & "e" & Exponent_S;

      --  Insert the radix point

      Insert (Number, (if Significand_Buffer (1) = '-' then 3 else 2), ".");

      return To_String (Number);
   end To_String;

   --------------
   -- Get_Prec --
   --------------

   function Get_Prec (X : Mpfloat) return Precision is
   begin
      return Precision (mpfr_get_prec (X.Value'Access));
   end Get_Prec;

   --------------
   -- Set_Prec --
   --------------

   procedure Set_Prec (X : Mpfloat; Prec : Precision) is
   begin
      if Prec > Prec_Max or Prec < Prec_Min then
         raise Failure with "precision out of bounds";
      else
         mpfr_set_prec (X.Value'Access, mpfr_prec_t (Prec));
      end if;
   end Set_Prec;

end Admpfr;
