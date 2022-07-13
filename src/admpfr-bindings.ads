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

   procedure mpfr_init (X : access mpfr_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_init";

   procedure mpfr_clear (X : access mpfr_t) with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_clear";

   function mpfr_set_str
     (Rop  : access mpfr_t;
      S    : chars_ptr;
      Base : int;
      Rnd  : mpfr_rnd_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_str";

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

   function mpfr_get_prec (X : access constant mpfr_t) return mpfr_prec_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_get_prec";

   procedure mpfr_set_prec (X : access constant mpfr_t;
                            Prec : mpfr_prec_t)
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_set_prec";

end Admpfr.Bindings;
