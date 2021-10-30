with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body AdMPFR is
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

   function mpfr_get_str (S : System.Address; Expptr : System.Address;
                          Base : int; N : Size_T; Op : access constant Mpfr_T;
                          Rnd : int) return Chars_ptr with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_str";

   function mpfr_get_str_ndigits (Base : Int; Prec: Prec_T) return Size_T with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_str_ndigits";

   function mpfr_get_prec (X : access constant Mpfr_T) return Prec_T with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_prec";

   function Rnd_T_Pos_To_Int (Rnd : Rnd_T) return Int is
      function C_Stub (Rnd : Int) return Int
      with
        Import => True,
        Convention => C,
        External_Name => "rnd_t_pos_to_int";

      Res : Int;
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
     (Rop : out Mpfr_Float;
      S   : String;
      Base : Base_T := 10;
      Rnd : Rnd_T := Rndn)
   is
      use Interfaces.C.Strings;

      Result : int;
      Input  : chars_ptr := New_String (S);
   begin
      Result := mpfr_set_str (Rop.Value'Access, Input,
                              Int (Base), Rnd_T_Pos_To_Int (Rnd));
      Free (Input);
      if Result /= 0 then
         raise Failure;
      end if;
   end Set;

   -- TODO: redefine the 'Image attribute when GCC FSF will support Ada 2022,
   -- see: https://stackoverflow.com/questions/67969309/ada-customise-image.
   function To_String (X : Mpfr_Float; Base : Base_T := 10;
                       Rnd : Rnd_T := Rndn) return String is

      -- TODO: rely on mpfr_get_str_ndigits for now but allows the user to set
      -- the number of digits to print.
      Number_Digits : constant Size_T := Size_T'Max (mpfr_get_str_ndigits
        (Int (Base), mpfr_get_prec (X.Value'Access)), 7);

      Significand_Buffer : String (1 .. Integer (Number_Digits + 2));
      Exponent           : Exp_T;

      Exponent_S, Number : Unbounded_String;
      Significand        : Chars_Ptr;
   begin
      Significand := mpfr_get_str (Significand_Buffer'Address, Exponent'Address,
                                   Int (Base), Number_Digits, X.Value'Access,
                                   Rnd_T_Pos_To_Int (Rnd));

      -- Convert Exponent to a string. Add '+' sign if exponent is zero or
      -- positive. Remove one as we'll insert the implicit radix point in the
      -- significand.
      Exponent_S := To_Unbounded_String (Exp_T'Image (Exponent - 1));
      if Exponent - 1 >= 0 then
         Overwrite (Exponent_S, 1, "+");
      end if;

      -- Concat significand and exponent
      Number := Value (Significand) & "E" & Exponent_S;

      -- Insert the radix point
      Insert (Number, (if Significand_Buffer (1) = '-' then 3 else 2), ".");

      return To_String (Number);
   end To_String;
end Admpfr;
