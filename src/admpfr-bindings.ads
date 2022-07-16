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

with Interfaces.C.Strings; use Interfaces.C.Strings;

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
     (Rop  : access mpfr_t;
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
      Endptr : System.Address;
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

end Admpfr.Bindings;
