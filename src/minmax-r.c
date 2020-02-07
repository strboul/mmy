#include "minmax.h"

/**
 * Validates the function input
 */
void ValidateRescaleMinMax(SEXP x, R_xlen_t x_len) {
    if (!Rf_isReal(x)) {
        Rf_errorcall(R_NilValue, "input has to be numeric vector");
    }
	if (!(x_len > 1)) {
		Rf_errorcall(R_NilValue, "numeric vector must be greater than 1");
	}
}


/**
 * This behavior only exists to adapt how the R handles
 * incomparable values.
 * Important: In R, NA preceeds NaN.
 *
 * @param x double value changed-in-place.
 */
int MutateForIncomparables(double *x, R_xlen_t x_len) {

	int has_any_incomparable, has_na, has_nan;
	has_any_incomparable = 0;
	has_na = 0;
	has_nan = 0;

	for (size_t i = 0; i < x_len; i++) {
		if (!has_na) {
			if (R_IsNA(x[i])) {
				has_na = 1;
			}
		}
		if (!has_nan) {
			if (R_IsNaN(x[i])) {
				has_nan = 1;
			}
		}
		if (has_na && has_nan) {
			break;
		}
	}

	if (has_na || has_nan) {
		has_any_incomparable = 1;
	}

	if (has_any_incomparable) {
		if (has_na) {
			for (size_t i = 0; i < x_len; i++) {
				if (R_finite(x[i])) {
					x[i] = R_NaReal;
				}
			}
		}
		if (has_nan) {
			for (size_t i = 0; i < x_len; i++) {
				if (R_finite(x[i])) {
					x[i] = R_NaN;
				}
			}
		}
	}

	return has_any_incomparable;
}


/**
 * R implementation
 */
SEXP _RescaleMinMax(SEXP x) {
	R_xlen_t x_len = Rf_length(x);
	ValidateRescaleMinMax(x, x_len);
	double *x_dbl = REAL(x);
	int has_incomparables = 0;
	has_incomparables = MutateForIncomparables(x_dbl, x_len);
	double res_calc_min_max[x_len];
	if (has_incomparables) {
		x_dbl = res_calc_min_max;
	} else {
		CalcMinMax(x_dbl, x_len, res_calc_min_max);
	}
	SEXP out = PROTECT(Rf_allocVector(REALSXP, x_len));
	double *pout = REAL(out);
	for (size_t i = 0; i < x_len; i++) {
		pout[i] = res_calc_min_max[i];
	}
	UNPROTECT(1);
	return out;
}


