with Interfaces.C; use Interfaces.C;
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
end Admpfr;
