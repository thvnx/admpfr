//  This file is part of AdMPFR.
//
//  AdMPFR is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  AdMPFR is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with Foobar.  If not, see <https://www.gnu.org/licenses/>.

#ifndef ADMPFR_STUBS_H
#define ADMPFR_STUBS_H

mpfr_rnd_t rnd_t_pos_to_int (int r);
mpfr_prec_t mpfr_prec_min ();
mpfr_prec_t mpfr_prec_max ();
int mpfr_printf_stub (const char *template, mpfr_rnd_t r, mpfr_t x);

#endif /* ADMPFR_STUBS_H */
