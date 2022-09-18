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
--  along with Admpfr.  If not, see <https://www.gnu.org/licenses/>.

with Interfaces.C.Strings;  use Interfaces.C.Strings;

private package Admpfr.Custom_Bindings is

   function mpfr_prec_min return mpfr_prec_t with
     Import        => True,
     Convention    => C,
     External_Name => "admpfr_prec_min";

   function mpfr_prec_max return mpfr_prec_t with
     Import        => True,
     Convention    => C,
     External_Name => "admpfr_prec_max";

   function printf_stub (T : chars_ptr;
                         R : mpfr_rnd_t;
                         X : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "admpfr_printf_stub";

   function sprintf_stub (B : System.Address;
                          T : chars_ptr;
                          R : mpfr_rnd_t;
                          X : access constant mpfr_t) return int
   with
     Import        => True,
     Convention    => C,
     External_Name => "admpfr_sprintf_stub";

   --  TODO: mpfr_round_nearest_away

   function mpfr_version return int with
     Import        => True,
     Convention    => C,
     External_Name => "admpfr_version";

   function mpfr_version_major return int with
     Import        => True,
     Convention    => C,
     External_Name => "admpfr_version_major";

   function mpfr_version_minor return int with
     Import        => True,
     Convention    => C,
     External_Name => "admpfr_version_minor";

   function mpfr_version_patchlevel return int with
     Import        => True,
     Convention    => C,
     External_Name => "admpfr_version_patchlevel";

   function mpfr_version_string return System.Address with
     Import        => True,
     Convention    => C,
     External_Name => "admpfr_version_string";

end Admpfr.Custom_Bindings;
