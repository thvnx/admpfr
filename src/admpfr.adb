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

with Admpfr.Bindings;        use Admpfr.Bindings;
with Admpfr.Custom_Bindings; use Admpfr.Custom_Bindings;

package body Admpfr is

   Prec_Min : constant Precision := Precision (mpfr_prec_min);
   Prec_Max : constant Precision := Precision (mpfr_prec_max);

   -----------------
   -- Mpfr_Printf --
   -----------------

   procedure Mpfr_Printf (Template : String;
                          X : Mpfloat;
                          R : Rounding := RNDN) is
      Res : int;
   begin
      Res := printf_stub (New_String (Template),
                          Rounding'Pos (R),
                          X.Value'Access);

      if Res < 0 then
         raise Failure with "mpfr_printf failure";
      end if;
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
      Rnd  : Rounding  := RNDN)
   is
      Result : int;
      Input  : chars_ptr := New_String (S);
   begin
      Result := mpfr_set_str
        (Rop.Value'Access, Input, int (Base),
         Rounding'Pos (Rnd));
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
      Rnd  : Rounding  := RNDN) return String
   is
      --  TODO: Rely on mpfr_get_str_ndigits for now but allows the user to set
      --  the number of digits to print by adding a parameter to this function.

      --  Default behavior mimics mpfr_printf("%.RNe", X),
      --  at least for base 10!

      Number_Digits : constant size_t := size_t'Max
        (mpfr_get_str_ndigits (abs int (Base),
                               mpfr_get_prec (X.Value'Access)), 7);

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
         Rounding'Pos (Rnd));

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
