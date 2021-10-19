#include <mpfr.h>

#include "my_func.h"

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
