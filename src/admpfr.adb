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

   procedure Main is

      function Get_Value (a : int) return int
      with
        Import        => True,
        Convention    => C,
        External_Name => "my_func";

      --  Imports function 'my_func' from C and
      --  rename it to 'Get_Value'

      V : int;
   begin
      V := Get_Value (20);
      Put_Line ("Result is " & int'Image (V));
   end Main;

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
      Base : int := 10)
   is
      use Interfaces.C.Strings;

      Result : int;
      Rnd : int := 0;
      Input  : chars_ptr := New_String (S);
   begin
      Result := mpfr_set_str (Rop.Value'Access, Input, Base, Rnd);
      Free (Input);
      if Result /= 0 then
         raise Failure;
      end if;
   end Set;
end Admpfr;
