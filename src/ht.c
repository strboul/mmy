
#include "mmy.h"

SEXP _ht (SEXP df, SEXP n) {

	if (!Rf_isFrame(df)) Rf_error("input must be a data.frame");

	if (!(Rf_xlength(n) == 1)) Rf_error("input length must be 1");
	if (!(Rf_isInteger(n) || Rf_isReal(n))) Rf_error("n isn't numeric");

	int nn = Rf_asInteger(n);
	// Rf_xlength is up to 64-bit unlike Rf_length which is 32-bit.
	int ncol = Rf_xlength(df);

	for (int i = 0; i <= ncol; i++) {

		for (int j = 0; j <= nn; j++) {

			SEXP el = Rf_protect(VECTOR_ELT(df, j));

			if (Rf_isReal(el)) {

				Rprintf("%f ", REAL(el)[i]);

			} else if (Rf_isInteger(el)) {

				Rprintf("%d ", INTEGER(el)[i]);

			} else if (Rf_isString(el)) {

				Rprintf("%s ", CHAR(STRING_ELT(el, i)));

			} else if (Rf_isFactor(el)) {

				SEXP attr = Rf_protect(Rf_getAttrib(el, R_LevelsSymbol));

				Rprintf("%s ", CHAR(STRING_ELT(attr, INTEGER(el)[i]-1)));

			} else {
				Rprintf("none");
			}
		}

		Rprintf("\n");

	}

	Rf_unprotect(2);

	return R_NilValue;
}
