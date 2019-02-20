
#include "mmy.h"

SEXP _ht (SEXP df, SEXP n) {

	if (!Rf_isFrame(df)) Rf_error("input must be a data.frame");

	if (!(Rf_xlength(n) == 1)) Rf_error("input length must be 1");
	if (!(Rf_isInteger(n) || Rf_isReal(n))) Rf_error("n isn't numeric");

	int nn = Rf_asInteger(n);
	// Rf_xlength is for up to 64-bit unlike Rf_length which is 32-bit.
	int ncol = Rf_xlength(df);

	// colnames
	SEXP names = Rf_protect(Rf_getAttrib(df, R_NamesSymbol));
	for (int c = 0; c < ncol; c++) {
		Rprintf("%s ", CHAR(STRING_ELT(names, c)));
	}
	Rf_unprotect(1);

	Rprintf("\n");
	// head
	// TODO tail. This time from btm to top
	for (int j = 0; j < nn; j++) {

		for (int i = 0; i < ncol; i++) {

			SEXP el = Rf_protect(VECTOR_ELT(df, i));
			if (Rf_isReal(el)) {
				Rprintf("%f ", REAL(el)[j]);
			} else if (Rf_isInteger(el)) {
				Rprintf("%d ", INTEGER(el)[j]);
			} else if (Rf_isString(el)) {
				Rprintf("%s ", CHAR(STRING_ELT(el, j)));
			} else if (Rf_isFactor(el)) {
				SEXP attr = Rf_protect(Rf_asCharacterFactor(el));
				Rprintf("%s ", CHAR(STRING_ELT(attr, j)));
				Rf_unprotect(1);
			} else {
				Rprintf("NONE");
			}
		}
		Rf_unprotect(1);
		Rprintf("\n");
	}

	return R_NilValue;
}
