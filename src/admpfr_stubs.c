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

#include <mpfr.h>

#include "admpfr_stubs.h"


int admpfr_printf_stub (const char *template, mpfr_rnd_t r, mpfr_t x)
{
  return mpfr_printf (template, r, x);
}

int admpfr_sprintf_stub (char *buf, const char *template, mpfr_rnd_t r, mpfr_t x)
{
  return mpfr_sprintf (buf, template, r, x);
}

int admpfr_version ()
{
  return MPFR_VERSION;
}

int admpfr_version_major ()
{
  return MPFR_VERSION_MAJOR;
}

int admpfr_version_minor ()
{
  return MPFR_VERSION_MINOR;
}

char *admpfr_version_string ()
{
  return MPFR_VERSION_STRING;
}

int admpfr_version_patchlevel ()
{
  return MPFR_VERSION_PATCHLEVEL;
}
