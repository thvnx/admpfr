--  This file is part of Admpfr.
--
--  Admpfr is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  Admpfr is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Admpfr.  If not, see <https://www.gnu.org/licenses/>.

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C_Streams;

private package Admpfr.Bindings is

   --  Initialization Functions

   procedure mpfr_init2 (X : access constant mpfr_t; Prec : mpfr_prec_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_init2";

   procedure mpfr_clear (X : access constant mpfr_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_clear";

   procedure mpfr_init (X : access constant mpfr_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_init";

   procedure mpfr_set_default_prec (Prec : mpfr_prec_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_default_prec";

   function mpfr_get_default_prec return mpfr_prec_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_default_prec";

   procedure mpfr_set_prec (X : access constant mpfr_t;
                            Prec : mpfr_prec_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_prec";

   function mpfr_get_prec (X : access constant mpfr_t) return mpfr_prec_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_prec";

   --  Assignment Functions

   function mpfr_set
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set";

   function mpfr_set_ui
     (Rop : access constant mpfr_t;
      Op  : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_ui";

   function mpfr_set_si
     (Rop : access constant mpfr_t;
      Op  : long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_si";

   function mpfr_set_flt
     (Rop : access constant mpfr_t;
      Op  : C_float;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_flt";

   function mpfr_set_d
     (Rop : access constant mpfr_t;
      Op  : double;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_d";

   function mpfr_set_ld
     (Rop : access constant mpfr_t;
      Op  : long_double;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_ld";

   function mpfr_set_ui_2exp
     (Rop : access constant mpfr_t;
      Op  : unsigned_long;
      E   : mpfr_exp_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_ui_2exp";

   function mpfr_set_si_2exp
     (Rop : access constant mpfr_t;
      Op  : long;
      E   : mpfr_exp_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_si_2exp";

   function mpfr_set_str
     (Rop  : access constant mpfr_t;
      S    : chars_ptr;
      Base : int;
      Rnd  : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_str";

   function mpfr_strtofr
     (Rop    : access constant mpfr_t;
      Nptr   : Interfaces.C.Strings.chars_ptr;
      Endptr : access constant Interfaces.C.Strings.chars_ptr;
      Base   : int;
      Rnd    : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_strtofr";

   procedure mpfr_set_nan (X : access constant mpfr_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_nan";

   procedure mpfr_set_inf (X : access constant mpfr_t; Sign : int) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_inf";

   procedure mpfr_set_zero (X : access constant mpfr_t; Sign : int) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_zero";

   procedure mpfr_swap (X : access constant mpfr_t;
                        Y : access constant mpfr_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_swap";

   --  Combined Initialization and Assignment Functions

   --  NOTE: since initialization is handled by a controlled type,
   --  initialization and assignment functions won't be binded here.

   --  Conversion Functions

   function mpfr_get_flt (Op  : access constant mpfr_t;
                          Rnd : mpfr_rnd_t) return C_float
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_flt";

   function mpfr_get_d (Op  : access constant mpfr_t;
                        Rnd : mpfr_rnd_t) return double
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_d";

   function mpfr_get_ld (Op  : access constant mpfr_t;
                         Rnd : mpfr_rnd_t) return long_double
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_ld";

   function mpfr_get_si (Op  : access constant mpfr_t;
                         Rnd : mpfr_rnd_t) return long
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_si";

   function mpfr_get_ui (Op  : access constant mpfr_t;
                         Rnd : mpfr_rnd_t) return unsigned_long
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_ui";

   function mpfr_get_d_2exp
     (Exp : access constant long;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return double
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_d_2exp";

   function mpfr_get_ld_2exp
     (Exp : access constant long;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return long_double
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_ld_2exp";

   function mpfr_frexp
     (Exp : access constant mpfr_exp_t;
      X   : access constant mpfr_t;
      Y   : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_frexp";

   function mpfr_get_str_ndigits (Base : int;
                                  Prec : mpfr_prec_t) return size_t
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_str_ndigits";

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

   function mpfr_fits_ulong_p (Op  : access constant mpfr_t;
                               Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_fits_ulong_p";

   function mpfr_fits_slong_p (Op  : access constant mpfr_t;
                               Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_fits_slong_p";

   function mpfr_fits_uint_p (Op  : access constant mpfr_t;
                              Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_fits_uint_p";

   function mpfr_fits_sint_p (Op  : access constant mpfr_t;
                              Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_fits_sint_p";

   function mpfr_fits_ushort_p (Op  : access constant mpfr_t;
                                Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_fits_ushort_p";

   --  Arithmetic Functions

   function mpfr_add
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_add";

   function mpfr_add_ui
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_add_ui";

   function mpfr_add_si
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_add_si";

   function mpfr_add_d
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : double;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_add_d";

   function mpfr_sub
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_sub";

   function mpfr_ui_sub
     (Rop : access constant mpfr_t;
      Op1 : unsigned_long;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_ui_sub";

   function mpfr_sub_ui
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_sub_ui";

   function mpfr_si_sub
     (Rop : access constant mpfr_t;
      Op1 : long;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_si_sub";

   function mpfr_sub_si
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_sub_si";

   function mpfr_d_sub
     (Rop : access constant mpfr_t;
      Op1 : double;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_d_sub";

   function mpfr_sub_d
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : double;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_sub_d";

   function mpfr_mul
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_mul";

   function mpfr_mul_ui
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_mul_ui";

   function mpfr_mul_si
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_mul_si";

   function mpfr_mul_d
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : double;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_mul_d";

   function mpfr_sqr
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_sqr";

   function mpfr_div
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_div";

   function mpfr_ui_div
     (Rop : access constant mpfr_t;
      Op1 : unsigned_long;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_ui_div";

   function mpfr_div_ui
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_div_ui";

   function mpfr_si_div
     (Rop : access constant mpfr_t;
      Op1 : long;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_si_div";

   function mpfr_div_si
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_div_si";

   function mpfr_d_div
     (Rop : access constant mpfr_t;
      Op1 : double;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_d_div";

   function mpfr_div_d
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : double;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_div_d";

   function mpfr_sqrt
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_sqrt";

   function mpfr_sqrt_ui
     (Rop : access constant mpfr_t;
      Op  : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_sqrt_ui";

   function mpfr_rec_sqrt
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_rec_sqrt";

   function mpfr_cbrt
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_cbrt";

   function mpfr_rootn_ui
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      N   : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_rootn_ui";

   function mpfr_root
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      N   : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_root";

   function mpfr_neg
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_neg";

   function mpfr_abs
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_abs";

   function mpfr_dim
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_dim";

   function mpfr_mul_2ui
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_mul_2ui";

   function mpfr_mul_2si
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_mul_2si";

   function mpfr_div_2ui
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_div_2ui";

   function mpfr_div_2si
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_div_2si";

   function mpfr_fac_ui
     (Rop : access constant mpfr_t;
      Op : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_fac_ui";

   function mpfr_fma
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Op3 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_fma";

   function mpfr_fms
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Op3 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_fms";

   function mpfr_fmma
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Op3 : access constant mpfr_t;
      Op4 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_fmma";

   function mpfr_fmms
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Op3 : access constant mpfr_t;
      Op4 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_fmms";

   function mpfr_hypot
     (Rop : access constant mpfr_t;
      X   : access constant mpfr_t;
      Y   : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_hypot";

   function mpfr_sum
     (Rop : access constant mpfr_t;
      Tab : System.Address;
      N   : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_sum";

   function mpfr_dot
     (Rop : access constant mpfr_t;
      A   : System.Address;
      B   : System.Address;
      N   : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_dot";

   --  Comparison Functions

   function mpfr_cmp
     (Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_cmp";

   function mpfr_cmp_ui
     (Op1 : access constant mpfr_t;
      Op2 : unsigned_long) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_cmp_ui";

   function mpfr_cmp_si
     (Op1 : access constant mpfr_t;
      Op2 : long) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_cmp_si";

   function mpfr_cmp_d
     (Op1 : access constant mpfr_t;
      Op2 : double) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_cmp_d";

   function mpfr_cmp_ld
     (Op1 : access constant mpfr_t;
      Op2 : long_double) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_cmp_ld";

   function mpfr_cmp_ui_2exp
     (Op1 : access constant mpfr_t;
      Op2 : unsigned_long;
      E   : mpfr_exp_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_cmp_ui_2exp";

   function mpfr_cmp_si_2exp
     (Op1 : access constant mpfr_t;
      Op2 : long;
      E   : mpfr_exp_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_cmp_si_2exp";

   function mpfr_cmpabs
     (Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_cmpabs";

   function mpfr_cmpabs_ui
     (Op1 : access constant mpfr_t;
      Op2 : unsigned_long) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_cmpabs_ui";

   function mpfr_nan_p (Op : access constant mpfr_t) return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_nan_p";

   function mpfr_inf_p (Op : access constant mpfr_t) return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_inf_p";

   function mpfr_number_p (Op : access constant mpfr_t) return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_number_p";

   function mpfr_zero_p (Op : access constant mpfr_t) return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_zero_p";

   function mpfr_regular_p (Op : access constant mpfr_t) return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_regular_p";

   function mpfr_sgn (Op : access constant mpfr_t) return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_sgn";

   function mpfr_greater_p
     (Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_greater_p";

   function mpfr_greaterequal_p
     (Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_greaterequal_p";

   function mpfr_less_p
     (Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_less_p";

   function mpfr_lessequal_p
     (Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_lessequal_p";

   function mpfr_equal_p
     (Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_equal_p";

   function mpfr_lessgreater_p
     (Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_lessgreater_p";

   function mpfr_unordered_p
     (Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_unordered_p";

   function mpfr_total_order_p
     (Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_total_order_p";

   --  Transcendental Functions

   function mpfr_log
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_log";

   function mpfr_log_ui
     (Rop : access constant mpfr_t;
      Op  : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_log_ui";

   function mpfr_log2
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_log2";

   function mpfr_log10
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_log10";

   function mpfr_log1p
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_log1p";

   function mpfr_exp
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_exp";

   function mpfr_exp2
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_exp2";

   function mpfr_exp10
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_exp10";

   function mpfr_expm1
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_expm1";

   function mpfr_pow
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_pow";

   function mpfr_pow_ui
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_pow_ui";

   function mpfr_pow_si
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_pow_si";

   function mpfr_ui_pow_ui
     (Rop : access constant mpfr_t;
      Op1 : unsigned_long;
      Op2 : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_ui_pow_ui";

   function mpfr_ui_pow
     (Rop : access constant mpfr_t;
      Op1 : unsigned_long;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_ui_pow";

   function mpfr_cos
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_cos";

   function mpfr_sin
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_sin";

   function mpfr_tan
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_tan";

   function mpfr_sin_cos
     (Sop : access constant mpfr_t;
      Cop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_sin_cos";

   function mpfr_sec
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_sec";

   function mpfr_csc
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_csc";

   function mpfr_cot
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_cot";

   function mpfr_acos
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_acos";

   function mpfr_asin
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_asin";

   function mpfr_atan
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_atan";

   function mpfr_atan2
     (Rop : access constant mpfr_t;
      X   : access constant mpfr_t;
      Y   : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_atan2";

   function mpfr_cosh
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_cosh";

   function mpfr_sinh
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_sinh";

   function mpfr_tanh
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_tanh";

   function mpfr_sinh_cosh
     (Sop : access constant mpfr_t;
      Cop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_sinh_cosh";

   function mpfr_sech
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_sech";

   function mpfr_csch
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_csch";

   function mpfr_coth
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_coth";

   function mpfr_acosh
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_acosh";

   function mpfr_asinh
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_asinh";

   function mpfr_atanh
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_atanh";

   function mpfr_eint
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_eint";

   function mpfr_li2
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_li2";

   function mpfr_gamma
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_gamma";

   function mpfr_gamma_inc
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_gamma_inc";

   function mpfr_lngamma
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_lngamma";

   function mpfr_lgamma
     (Rop   : access constant mpfr_t;
      Signp : access int;
      Op    : access constant mpfr_t;
      Rnd   : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_lgamma";

   function mpfr_digamma
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_digamma";

   function mpfr_beta
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_beta";

   function mpfr_zeta
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_zeta";

   function mpfr_zeta_ui
     (Rop : access constant mpfr_t;
      Op : unsigned_long;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_zeta_ui";

   function mpfr_erf
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_erf";

   function mpfr_erfc
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_erfc";

   function mpfr_j0
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_j0";

   function mpfr_j1
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_j1";

   function mpfr_jn
     (Rop : access constant mpfr_t;
      N   : long;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_jn";

   function mpfr_y0
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_y0";

   function mpfr_y1
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_y1";

   function mpfr_yn
     (Rop : access constant mpfr_t;
      N   : long;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_yn";

   function mpfr_agm
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_agm";

   function mpfr_ai
     (Rop : access constant mpfr_t;
      X   : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_ai";

   function mpfr_const_log2
     (Rop : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_const_log2";

   function mpfr_const_pi
     (Rop : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_const_pi";

   function mpfr_const_euler
     (Rop : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_const_euler";

   function mpfr_const_catalan
     (Rop : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_const_catalan";

   --  Input and Output Functions

   function mpfr_out_str (Stream : Interfaces.C_Streams.FILEs;
                          Base   : int;
                          N      : size_t;
                          Op     : access constant mpfr_t;
                          Rnd    : mpfr_rnd_t) return size_t
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpfr_out_str";

   function mpfr_inp_str (Rop    : access constant mpfr_t;
                          Stream : Interfaces.C_Streams.FILEs;
                          Base   : int;
                          Rnd    : mpfr_rnd_t) return size_t
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpfr_inp_str";

   function mpfr_fpif_export (Stream : Interfaces.C_Streams.FILEs;
                              Rop    : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpfr_fpif_export";

   function mpfr_fpif_import (Op     : access constant mpfr_t;
                              Stream : Interfaces.C_Streams.FILEs) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "__gmpfr_fpif_import";

   procedure mpfr_dump (Op : access constant mpfr_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_dump";

   --  Integer and Remainder Related Functions

   function mpfr_rint
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_rint";

   function mpfr_ceil
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_ceil";

   function mpfr_floor
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_floor";

   function mpfr_round
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_round";

   function mpfr_roundeven
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_roundeven";

   function mpfr_trunc
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_trunc";

   function mpfr_rint_ceil
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_rint_ceil";

   function mpfr_rint_floor
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_rint_floor";

   function mpfr_rint_round
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_rint_round";

   function mpfr_rint_roundeven
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_rint_roundeven";

   function mpfr_rint_trunc
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_rint_trunc";

   function mpfr_frac
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_frac";

   function mpfr_modf
     (Iop : access constant mpfr_t;
      Fop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_modf";

   function mpfr_fmod
     (R   : access constant mpfr_t;
      X   : access constant mpfr_t;
      Y   : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_fmod";

   function mpfr_fmodquo
     (R   : access constant mpfr_t;
      Q   : access long;
      X   : access constant mpfr_t;
      Y   : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_fmodquo";

   function mpfr_remainder
     (R   : access constant mpfr_t;
      X   : access constant mpfr_t;
      Y   : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_remainder";

   function mpfr_remquo
     (R   : access constant mpfr_t;
      Q   : access long;
      X   : access constant mpfr_t;
      Y   : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_remquo";

   function mpfr_integer_p (Op : access constant mpfr_t) return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_integer_p";

   --  Rounding-Related Functions

   procedure mpfr_set_default_rounding_mode (Rnd : mpfr_rnd_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_default_rounding_mode";

   function mpfr_get_default_rounding_mode return mpfr_rnd_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_default_rounding_mode";

   function mpfr_prec_round
     (X    : access constant mpfr_t;
      Prec : mpfr_prec_t;
      Rnd  : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_prec_round";

   function mpfr_can_round
     (B    : access constant mpfr_t;
      Err  : mpfr_exp_t;
      Rnd1 : mpfr_rnd_t;
      Rnd2 : mpfr_rnd_t;
      Prec : mpfr_prec_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_can_round";

   function mpfr_min_prec (X : access constant mpfr_t) return mpfr_prec_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_min_prec";

   function mpfr_print_rnd_mode
     (Rnd : mpfr_rnd_t) return Interfaces.C.Strings.chars_ptr
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_print_rnd_mode";

   --  Miscellaneous Functions

   procedure mpfr_nexttoward
     (X : access constant mpfr_t;
      Y : access constant mpfr_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_nexttoward";

   procedure mpfr_nextabove (X : access constant mpfr_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_nextabove";

   procedure mpfr_nextbelow (X : access constant mpfr_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_nextbelow";

   function mpfr_min
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_min";

   function mpfr_max
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_max";

   --  NOTE: *random* functions not supported

   function mpfr_get_exp (X : access constant mpfr_t) return mpfr_exp_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_exp";

   function mpfr_set_exp
     (X : access constant mpfr_t;
      E : mpfr_exp_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_exp";

   function mpfr_signbit (Op : access constant mpfr_t) return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_signbit";

   function mpfr_setsign
     (Rop : access constant mpfr_t;
      Op  : access constant mpfr_t;
      S   : int;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_setsign";

   function mpfr_copysign
     (Rop : access constant mpfr_t;
      Op1 : access constant mpfr_t;
      Op2 : access constant mpfr_t;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_copysign";

   function mpfr_get_version return Interfaces.C.Strings.chars_ptr with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_version";

   function mpfr_get_patches return System.Address with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_patches";

   function mpfr_buildopt_tls_p return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_buildopt_tls_p";

   function mpfr_buildopt_float128_p return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_buildopt_float128_p";

   function mpfr_buildopt_decimal_p return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_buildopt_decimal_p";

   function mpfr_buildopt_gmpinternals_p return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_buildopt_gmpinternals_p";

   function mpfr_buildopt_sharedcache_p return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_buildopt_sharedcache_p";

   function mpfr_buildopt_tune_case return System.Address with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_buildopt_tune_case";

   --  Exception Related Functions

   function mpfr_get_emin return mpfr_exp_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_emin";

   function mpfr_get_emax return mpfr_exp_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_emax";

   function mpfr_set_emin (arg1 : mpfr_exp_t) return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_emin";

   function mpfr_set_emax (arg1 : mpfr_exp_t) return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_emax";

   function mpfr_get_emin_min return mpfr_exp_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_emin_min";

   function mpfr_get_emin_max return mpfr_exp_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_emin_max";

   function mpfr_get_emax_min return mpfr_exp_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_emax_min";

   function mpfr_get_emax_max return mpfr_exp_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_emax_max";

   function mpfr_check_range
     (X   : access constant mpfr_t;
      T   : int;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_check_range";

   function mpfr_subnormalize
     (X   : access constant mpfr_t;
      T   : int;
      Rnd : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_subnormalize";

   procedure mpfr_clear_underflow  with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_clear_underflow";

   procedure mpfr_clear_overflow with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_clear_overflow";

   procedure mpfr_clear_divby0 with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_clear_divby0";

   procedure mpfr_clear_nanflag with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_clear_nanflag";

   procedure mpfr_clear_inexflag with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_clear_inexflag";

   procedure mpfr_clear_erangeflag with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_clear_erangeflag";

   procedure mpfr_clear_flags with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_clear_flags";

   procedure mpfr_set_underflow with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_underflow";

   procedure mpfr_set_overflow with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_overflow";

   procedure mpfr_set_divby0 with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_divby0";

   procedure mpfr_set_nanflag with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_nanflag";

   procedure mpfr_set_inexflag with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_inexflag";

   procedure mpfr_set_erangeflag with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_erangeflag";

   function mpfr_underflow_p return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_underflow_p";

   function mpfr_overflow_p return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_overflow_p";

   function mpfr_divby0_p return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_divby0_p";

   function mpfr_nanflag_p return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_nanflag_p";

   function mpfr_inexflag_p return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_inexflag_p";

   function mpfr_erangeflag_p return int with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_erangeflag_p";

   procedure mpfr_flags_clear (Mask : mpfr_flags_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_flags_clear";

   procedure mpfr_flags_set (Mask : mpfr_flags_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_flags_set";

   function mpfr_flags_test (Mask : mpfr_flags_t) return mpfr_flags_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_flags_test";

   function mpfr_flags_save return mpfr_flags_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_flags_save";

   procedure mpfr_flags_restore (Flags : mpfr_flags_t; Mask : mpfr_flags_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_flags_restore";

end Admpfr.Bindings;
