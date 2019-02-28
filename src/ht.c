
#include "mmy.h"

SEXP _ht (SEXP df, SEXP n) {

	if (!Rf_isFrame(df)) Rf_error("input must be a data.frame");

	if (!(Rf_xlength(n) == 1)) Rf_error("input length cannot be greater than one");
	if (!(Rf_isInteger(n) || Rf_isReal(n))) Rf_error("n isn't numeric");

	const int nn = Rf_asInteger(n);
	// Rf_xlength is for up to 64-bit unlike Rf_length which is 32-bit.
	const R_xlen_t ncol = Rf_xlength(df);
	const R_xlen_t nrow = Rf_xlength(Rf_getAttrib(df, R_RowNamesSymbol));

	// get head and tail indices:
       /*  int *indlen = malloc(4 * sizeof(int)); */
	/* int indices[indlen] = {1,2,3,4}; */
	/* printf("%i\n", indices[2]); */

	// colnames
	SEXP names = Rf_protect(Rf_getAttrib(df, R_NamesSymbol));
	for (R_xlen_t i = 0; i < ncol; i++) {
		Rprintf("%s ", CHAR(STRING_ELT(names, i)));
	}
	Rf_unprotect(1);

	Rprintf("\n");
	// head
	// TODO tail. This time from btm to top
	for (int j = 0; j < nn; j++) {

		for (R_xlen_t i = 0; i < ncol; i++) {

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

	/* free(indlen); */
	return R_NilValue;
}
