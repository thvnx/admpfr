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

with Interfaces.C.Strings;  use Interfaces.C.Strings;

private package Admpfr.Custom_Bindings is

   function mpfr_prec_min return mpfr_prec_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_prec_min";

   function mpfr_prec_max return mpfr_prec_t with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_prec_max";

   function rounding_to_mpfr_rnd_t (Rnd : mpfr_rnd_t) return mpfr_rnd_t with
     Import        => True,
     Convention    => C,
     External_Name => "rounding_to_mpfr_rnd_t";

   function printf_stub (T : chars_ptr;
                         R : mpfr_rnd_t;
                         X : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "mpfr_printf_stub";

end Admpfr.Custom_Bindings;
