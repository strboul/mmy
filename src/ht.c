
// Rf_xlength is for up to 64-bit unlike Rf_length which is 32-bit.
#include "mmy.h"

// get length of an integer
// TODO move to utils.c
int numlen(int *val){
	int res = 0;
	while(!(*val == 0)) {
		*val /= 10;
		res++;
	}
	return res;
}

void prt_hspace(int *len) { Rprintf("%*s", *len, ""); }

void prt(SEXP df, int *init, int *num, int *ncol) {
	for (int j = *init; j < *num; j++) {
		Rprintf("%i: ", j + 1);
		for (R_xlen_t i = 0; i < *ncol; i++) {
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
				Rf_error("some error occured");
			}
			Rf_unprotect(1);
		}
		Rprintf("\n");
	}
}

// TODO struct
void head(SEXP df, int *init, int *num, int *ncol) { prt(df, init, num, ncol); }
void tail(SEXP df, int *init, int *num, int *ncol) { prt(df, init, num, ncol); }

void is_valid (SEXP df, SEXP n) {
	if (!Rf_isFrame(df)) Rf_error("input must be a data.frame");
	if (!(Rf_xlength(n) == 1)) Rf_error("input length cannot be greater than one");
	if (!(Rf_isInteger(n) || Rf_isReal(n))) Rf_error("n isn't numeric");
}

SEXP _ht (SEXP df, SEXP n) {

	is_valid(df, n);

	int nn = Rf_asInteger(n);
	R_xlen_t nrow = Rf_xlength(Rf_getAttrib(df, R_RowNamesSymbol));
	R_xlen_t ncol = Rf_xlength(df);

	// print widths:
	int nrowlen = numlen((int*)&nrow);
	/* int ncollen =  */

	// colnames:
	SEXP names = Rf_protect(Rf_getAttrib(df, R_NamesSymbol));
	prt_hspace(&nrowlen);
	for (R_xlen_t i = 0; i < ncol; i++) {
		Rprintf("%s ", CHAR(STRING_ELT(names, i)));
	}
	Rf_unprotect(1);

	Rprintf("\n");

	int begin = 0;
	head(df, &begin, &nn, (int*)&ncol);

	prt_hspace(&nrowlen);

	for (R_xlen_t i = 0; i < ncol; i++) {
		for (int j = 0; j < 8; j++) {
			Rprintf("%s","-");
		}
		Rprintf(" ");
	}
	Rprintf("\n");

	int remain = (int)nrow - nn;
	tail(df, &remain, (int*)&nrow, (int*)&ncol);

	Rprintf("\n");

	return R_NilValue;
}
