
#include "diff_int.h"

/**
 * @return
 * Number of integers same as length x minus one.
 **/
SEXP _diff_int (SEXP x) {
  if (!Rf_isInteger(x)) {
    Rf_error("input must be integer, not '%s'", Rf_type2char(TYPEOF(x)));
  }
  R_xlen_t x_len_minus;
  x_len_minus = Rf_xlength(x) - 1;
  int res;
  SEXP out = PROTECT(Rf_allocVector(INTSXP, x_len_minus));
  int *px = INTEGER(x);
  size_t i;
  i = 0;
  while (i < x_len_minus) {
    res = px[i] - px[i + 1];
    res = -res; // flipping the sign
    INTEGER(out)[i] = res;
    i = i + 1;
  }
  UNPROTECT(1);
  return out;
}

