
#include "grp_cns.h"

static void is_grp_cns_valid_vector (SEXP x) {
  if (!Rf_isNumeric(x)) {
    Rf_error("input must be numeric vector");
  }
}

/*
 * Group sequences of integers
 * 
 * @param x vector
 * @param early_exit logical flag. it's not publicly available, 
 *        it's there for bench issues. Default value is TRUE.
 */
SEXP _grp_cns(SEXP x, SEXP early_exit) {  
  
  is_grp_cns_valid_vector(x);
  
  R_xlen_t x_len;
  x_len = Rf_xlength(x);
  
  x = coerce_to_integer(x, x_len);
  
  // check the diff:
  SEXP dif;
  R_xlen_t dif_len;
  dif = PROTECT(Rf_allocVector(INTSXP, x_len - 1));
  int *p_dif = INTEGER(dif);
  int *p__integer_diff = INTEGER(_diff_int(x));
  for (size_t i = 0; i < x_len - 1; i++) {
    p_dif[i] = p__integer_diff[i];
  }
  dif_len = Rf_xlength(dif);
  
  // diff accepted values:
  const int dif_values[2] = {1, -1};
  
  /**
   * early return when the input vector doesn't 
   * have 'any' consecutive numbers:
   **/
  if (Rf_asLogical(early_exit)) {
    if (int_has_all(dif_values[0], dif, dif_len) &&
        int_has_all(dif_values[1], dif, dif_len)) {
      SEXP early_out;
      early_out = PROTECT(Rf_allocVector(INTSXP, x_len));
      int *p_early_out = INTEGER(early_out);
      for (size_t i = 0; i < x_len; i++) {
        p_early_out[i] = i + 1;
      }
      UNPROTECT(2);
      return early_out;
    }
  }
  
  // group sequences:
  int grp_ind, pivot;
  SEXP grp_out;
  grp_ind = GRP_START_IND;
  pivot = GRP_INIT_PIVOT;
  
  int *pdif = INTEGER(dif);
  
  grp_out = PROTECT(Rf_allocVector(INTSXP, x_len));
  int *pgrp_out = INTEGER(grp_out);
  
  // add 1 to first diff
  // `pivot + 1` because added 1 to the first one
  pgrp_out[pivot] = grp_ind;
  while (pivot < dif_len) {
    
    if (pivot % 1024 == 0) {
      R_CheckUserInterrupt();
    }
    
    // TODO check dif_values[1]
    if (!(pdif[pivot] > dif_values[0])) {
      pgrp_out[pivot + 1] = grp_ind;
    } else {
      grp_ind++;
      pgrp_out[pivot + 1] = grp_ind;
    }
    
    pivot++;
  }
  
  UNPROTECT(2);
  return grp_out;
}

