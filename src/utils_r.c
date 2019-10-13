
/**
 * Utils for R data structures
 * (with SEXP)
 **/
#include "utils_r.h"

/**
 * Coerce input to integer
 * @details
 * A sanitized version of:
 * `SEXP x = Rf_coerceVector(x, INTSXP);`
 **/
SEXP coerce_to_integer (SEXP x, int x_len) {
  if (!Rf_isInteger(x)) {
    if (Rf_isReal(x)) {
      SEXP integer_out, real;
      int integer;
      integer_out = PROTECT(Rf_allocVector(INTSXP, x_len));
      int *pinteger_out = INTEGER(integer_out);
      for (size_t i = 0; i < x_len; i++) {
        real = Rf_ScalarReal(REAL(x)[i]);
        integer = Rf_asInteger(real);
        pinteger_out[i] = integer;
      }
      Rf_warning("input '%s' coerced to integer", Rf_type2char(TYPEOF(x)));
      UNPROTECT(1);
      return integer_out;
    }
  }
  return x;
}

/**
 * Check if an integer array has any values.
 * 
 * @return
 * Returns an integer.
 * If it's one, means 'true' and if it's zero, means 'false'.
 **/
int int_has_any(int val, SEXP arr, int size) {
  if (!Rf_isInteger(arr)) {
    Rf_errorcall(R_NilValue, "not integer input in `int_has_any`");
  }
  int *parr = INTEGER(arr);
  for (size_t i = 0; i < size; i++) {
    if (parr[i] == val) {
      return 1;
    }
  }
  return 0;
}

int int_has_all(int val, SEXP arr, int size) {
  if (!Rf_isInteger(arr)) {
    Rf_errorcall(R_NilValue, "not integer input in `int_has_all`");
  }
  int *parr = INTEGER(arr);
  for (size_t i = 0; i < size; i++) {
    if (parr[i] != val) {
      return 0;
    }
  }
  return 1;
}

