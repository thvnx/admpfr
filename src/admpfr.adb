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

   Prec_Min_Cst : constant Precision := Precision (mpfr_prec_min);
   Prec_Max_Cst : constant Precision := Precision (mpfr_prec_max);

   --------------
   -- Prec_Min --
   --------------

   function Prec_Min return Precision is (Prec_Min_Cst);

   --------------
   -- Prec_Max --
   --------------

   function Prec_Max return Precision is (Prec_Max_Cst);

   --------------------------
   -- Reformat_Printf_Args --
   --------------------------

   procedure Reformat_Printf_Args (T : in out String; R : in out Rounding) is
   begin
      for I in 1 .. T'Length loop
         if T (I) = 'R' and then R = RNDN then
            case T (I + 1) is
               when 'N' => T (I + 1) := '*';
               when 'U' => T (I + 1) := '*'; R := RNDU;
               when 'D' => T (I + 1) := '*'; R := RNDD;
               when 'Y' => T (I + 1) := '*'; R := RNDA;
               when 'Z' => T (I + 1) := '*'; R := RNDZ;
               when '*' => null;
               when others => raise Failure;
            end case;
         end if;
         exit when T (I) = '*';
      end loop;
   end Reformat_Printf_Args;

   -----------------
   -- Mpfr_Printf --
   -----------------

   procedure Mpfr_Printf (Template : String;
                          X        : Mpfloat;
                          R        : Rounding := RNDN) is
      Res   : int := -1;
      Input : chars_ptr := Null_Ptr;
      Tpl   : String := Template;
      Rnd   : Rounding := R;
   begin
      begin
         Reformat_Printf_Args (Tpl, Rnd);
         Input := New_String (Tpl);
         Res := printf_stub (Input, Rounding'Pos (Rnd), X.Value'Access);
         Free (Input);

         if Res < 0 then
            raise Failure with "mpfr_printf failure";
         end if;

      exception
         when Storage_Error =>
            raise Failure with "mpfr_printf malformation";
      end;
   end Mpfr_Printf;

   ------------------
   -- Mpfr_Sprintf --
   ------------------

   function Mpfr_Sprintf (Template : String;
                          X        : Mpfloat;
                          R        : Rounding := RNDN) return String is
      Res : int := -1;
      Buf : String (1 .. 256);
      --  TODO: find a way to compute a safe upper bound for Buf. mpfr_sprintf
      --  will overflow if the result is bigger than that.

      Input : chars_ptr := Null_Ptr;
      Tpl   : String := Template;
      Rnd   : Rounding := R;
   begin
      begin
         Reformat_Printf_Args (Tpl, Rnd);
         Input := New_String (Tpl);

         Res := sprintf_stub (Buf'Address,
                              Input,
                              Rounding'Pos (Rnd),
                              X.Value'Access);
         Free (Input);

         if Res > 0 then
            return Buf (1 .. Integer (Res));
         else
            raise Failure with "mpfr_sprintf failure";
         end if;

      exception
         when Storage_Error =>
            raise Failure with "mpfr_sprintf malformation";
      end;
   end Mpfr_Sprintf;

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
      Rnd  : Rounding    := RNDN)
   is
      Result : int       := -1;
      Input  : chars_ptr := New_String (S);
   begin
      Result := mpfr_set_str (Rop.Value'Access,
                              Input,
                              int (Base),
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
      Rnd  : Rounding    := RNDN) return String
   is
      --  TODO: Rely on mpfr_get_str_ndigits for now but allows the user to set
      --  the number of digits to print by adding a parameter to this function.

      Number_Digits : constant size_t :=
        size_t'Max (mpfr_get_str_ndigits (abs int (Base),
                                          mpfr_get_prec (X.Value'Access)),
                    7);

      Significand_Buffer : String (1 .. Integer (Number_Digits + 2));
      Exponent           : mpfr_exp_t;

      Exponent_S, Number : Unbounded_String;
      Significand        : chars_ptr;
   begin
      Significand := mpfr_get_str (Significand_Buffer'Address,
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

   ----------------------
   -- Set_Default_Prec --
   ----------------------

   procedure Set_Default_Prec (Prec : Precision) is
   begin
      if Prec > Prec_Max or Prec < Prec_Min then
         raise Failure with "precision out of bounds";
      else
         mpfr_set_default_prec (mpfr_prec_t (Prec));
      end if;
   end Set_Default_Prec;

   ----------------------
   -- Get_Default_Prec --
   ----------------------

   function Get_Default_Prec return Precision is
   begin
      return Precision (mpfr_get_default_prec);
   end Get_Default_Prec;

end Admpfr;
