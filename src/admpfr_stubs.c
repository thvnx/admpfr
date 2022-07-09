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

#include <mpfr.h>

#include "admpfr_stubs.h"


// Assuming that mpfr_rnd_t is an int, mpfr_prec_t is a long.

mpfr_rnd_t rounding_to_mpfr_rnd_t (int r)
{
  switch (r)
    {
    case 0: return MPFR_RNDN;
    case 1: return MPFR_RNDZ;
    case 2: return MPFR_RNDU;
    case 3: return MPFR_RNDD;
    case 4: return MPFR_RNDA;
    case 5: return MPFR_RNDF;
    default: return -1;
    }
}

mpfr_prec_t mpfr_prec_min ()
{
  return MPFR_PREC_MIN;
}

mpfr_prec_t mpfr_prec_max ()
{
  return MPFR_PREC_MAX;
}

int mpfr_printf_stub (const char *template, mpfr_rnd_t r, mpfr_t x)
{
  return mpfr_printf (template, r, x);
}
