#include <mpfr.h>

#include "admpfr_stubs.h"

int my_func (int a)
{
  mpfr_t t;
  long r;
  mpfr_init2 (t, 200);
  mpfr_set_d (t, (double)a, MPFR_RNDD);
  mpfr_mul_ui (t, t, 3, MPFR_RNDU);
  r = mpfr_get_si (t, MPFR_RNDN);
  mpfr_clear (t);
  return r;
}

int rnd_t_pos_to_int (int r)
{
  switch (r)
    {
    case 0: return MPFR_RNDN;
    case 1: return MPFR_RNDD;
    case 2: return MPFR_RNDU;
    case 3: return MPFR_RNDZ;
    case 4: return MPFR_RNDA;
    case 5: return MPFR_RNDF;
    default: return -1;
    }
}
