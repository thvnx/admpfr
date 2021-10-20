with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Ada.Text_IO;  use Ada.Text_IO;

package body AdMPFR is
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

   procedure Initialize (M : in out Mpfr_Float) is
   begin
      mpfr_init (M.Value'Access);
   end Initialize;

   procedure Finalize (M : in out Mpfr_Float) is
   begin
      mpfr_clear (M.Value'Access);
   end Finalize;

   procedure Set
     (M : out Mpfr_Float;
      To   : String;
      Base : Int := 10)
   is
      use Interfaces.C.Strings;

      Result : Int;
      Rnd : Int := 0;
      Input  : chars_ptr := New_String (To);
   begin
      Result := mpfr_set_str (M.Value'Access, Input, Base, Rnd);
      Free (Input);
      if Result /= 0 then
         raise Failure;
      end if;
   end Set;
end Admpfr;
