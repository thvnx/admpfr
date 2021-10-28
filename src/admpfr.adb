with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Ada.Text_IO;  use Ada.Text_IO;

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
end Admpfr;
