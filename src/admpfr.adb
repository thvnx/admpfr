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

package body AdMPFR is

   procedure mpfr_init (X : access Mpfr_T) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_init";

   procedure mpfr_clear (X : access Mpfr_T) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_clear";

   function mpfr_prec_min return Prec_T with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_prec_min";
   Prec_Min : constant Prec_T := mpfr_prec_min;

   function mpfr_prec_max return Prec_T with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_prec_max";
   Prec_Max : constant Prec_T := mpfr_prec_max;

   function mpfr_set_str
     (Rop  : access Mpfr_T;
      S    : chars_ptr;
      Base : int;
      Rnd  : int) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_str";

   function mpfr_get_str
     (S      : System.Address;
      Expptr : System.Address;
      Base   : int;
      N      : size_t;
      Op     : access constant Mpfr_T;
      Rnd    : int) return chars_ptr
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_str";

   function mpfr_get_str_ndigits (Base : int; Prec : Prec_T) return size_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_str_ndigits";

   function mpfr_get_prec (X : access constant Mpfr_T) return Prec_T with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_prec";

   procedure mpfr_set_prec (X : access constant Mpfr_T; Prec : Prec_T) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_prec";

   function Rnd_T_Pos_To_Int (Rnd : Rnd_T) return int is
      function C_Stub (Rnd : int) return int
      with
        Import        => True,
        Convention    => C,
        External_Name => "rnd_t_pos_to_int";

      Res : int;
   begin
      Res := C_Stub (Rnd_T'Pos (Rnd));
      pragma Assert (Res > 0);
      return Res;
   end Rnd_T_Pos_To_Int;

   procedure Initialize (X : in out Mpfr_Float) is
   begin
      mpfr_init (X.Value'Access);
   end Initialize;

   procedure Finalize (X : in out Mpfr_Float) is
   begin
      mpfr_clear (X.Value'Access);
   end Finalize;

   procedure Set
     (Rop  : out Mpfr_Float;
      S    : String;
      Base : Base_T := 10;
      Rnd  : Rnd_T  := Rndn)
   is
      Result : int;
      Input  : chars_ptr := New_String (S);
   begin
      Result := mpfr_set_str
        (Rop.Value'Access, Input, int (Base),
                   Rnd_T_Pos_To_Int (Rnd));
      Free (Input);
      if Result /= 0 then
         raise Failure;
      end if;
   end Set;

   --  TODO: Add 'Image attribute on Mpfr_Float type when GCC FSF will support
   --  Ada 2022, see:
   --  https://stackoverflow.com/questions/67969309/ada-customise-image.

   function To_String
     (X    : Mpfr_Float;
      Base : Base_T := 10;
      Rnd  : Rnd_T  := Rndn) return String
   is
      --  TODO: Rely on mpfr_get_str_ndigits for now but allows the user to set
      --  the number of digits to print by adding a parameter to this function.

      --  Default behavior mimics mpfr_printf("%.RNe", X),
      --  at least for base 10!

      Number_Digits : constant size_t := size_t'Max
        (mpfr_get_str_ndigits (int (Base), mpfr_get_prec (X.Value'Access)), 7);

      Significand_Buffer : String (1 .. Integer (Number_Digits + 2));
      Exponent           : Exp_T;

      Exponent_S, Number : Unbounded_String;
      Significand        : chars_ptr;
   begin
      Significand := mpfr_get_str
                       (Significand_Buffer'Address,
                        Exponent'Address,
                        int (Base),
                        Number_Digits,
                        X.Value'Access,
                        Rnd_T_Pos_To_Int (Rnd));

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

      Exponent_S := To_Unbounded_String (Exp_T'Image (Exponent));

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

   function Get_Prec (X : Mpfr_Float) return Prec_T is
   begin
      return mpfr_get_prec (X.Value'Access);
   end Get_Prec;

   procedure Set_Prec (X : Mpfr_Float; Prec : Prec_T) is
   begin
      if Prec > Prec_Max or Prec < Prec_Min then
         raise Failure;
      else
         mpfr_set_prec (X.Value'Access, Prec);
      end if;
   end Set_Prec;

end AdMPFR;
