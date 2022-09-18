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
//  along with Admpfr.  If not, see <https://www.gnu.org/licenses/>.

#ifndef ADMPFR_STUBS_H
#define ADMPFR_STUBS_H

int mpfr_printf_stub (const char *template, mpfr_rnd_t r, mpfr_t x);
int mpfr_sprintf_stub (char *buf, const char *template, mpfr_rnd_t r, mpfr_t x);
int admpfr_version ();
int admpfr_version_major ();
int admpfr_version_minor ();
int admpfr_version_patchlevel ();
char *admpfr_version_string ();

#endif /* ADMPFR_STUBS_H */
