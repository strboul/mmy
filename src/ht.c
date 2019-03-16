
// Rf_xlength is for up to 64-bit unlike Rf_length which is 32-bit.
#include "mmy.h"

void prt_hspace(int *len) { Rprintf("%*s", *len, ""); }

void prt_colnames(SEXP df, int *ncol, int *nrowlen) {
	SEXP names = PROTECT(Rf_getAttrib(df, R_NamesSymbol));
	prt_hspace(nrowlen);
	for (R_xlen_t i = 0; i < *ncol; i++) {
		Rprintf("%s ", CHAR(STRING_ELT(names, i)));
	}
	UNPROTECT(1);
	Rprintf("\n");
}

void prt_dashes(int *ncol, int *ndash) {
	for (R_xlen_t i = 0; i < *ncol; i++) {
		for (int j = 0; j < *ndash; j++) {
			Rprintf("%s","-");
		}
		Rprintf(" ");
	}
	Rprintf("\n");
}

void prit(SEXP df, int *init, int *num, int *ncol) {

	for (int j = *init; j < *num; j++) {

		Rprintf("%i: ", j + 1);

		int width = 5;

		for (R_xlen_t i = 0; i < *ncol; i++) {

			SEXP el = PROTECT(VECTOR_ELT(df, i));

			if (Rf_isReal(el)) {

				/* Rprintf("%f ", REAL(el)[j]); */

				Rprintf("%*s%f ", width, "", REAL(el)[j]);

			} else if (Rf_isInteger(el)) {

				Rprintf("%d ", INTEGER(el)[j]);

			} else if (Rf_isString(el)) {

				Rprintf("%s ", CHAR(STRING_ELT(el, j)));

			} else if (Rf_isFactor(el)) {

				SEXP attr = PROTECT(Rf_asCharacterFactor(el));

				Rprintf("%s ", CHAR(STRING_ELT(attr, j)));

				UNPROTECT(1);

			} else {

				Rf_error("some error occured");

			}

			UNPROTECT(1);

		}

		Rprintf("\n");
	}
}

void is_valid (SEXP df, SEXP n) {
	if (!Rf_isFrame(df))
		Rf_error("input must be a data.frame");
	if (!(Rf_xlength(n) == 1))
		Rf_error("input length cannot be greater than one");
	if (!(Rf_isInteger(n) || Rf_isReal(n)))
		Rf_error("n isn't numeric");
}

SEXP _ht (SEXP df, SEXP n) {

	is_valid(df, n);

	struct Invariants {
		int nn;
		R_xlen_t nrow;
		R_xlen_t ncol;
		int nrowi;
		int ncoli;
		int nrowlen;
		int Ndash;
		int remain;
	} dim;

	dim.nn = Rf_asInteger(n);
	dim.nrow = Rf_xlength(Rf_getAttrib(df, R_RowNamesSymbol));
	dim.ncol = Rf_xlength(df);
	dim.nrowi = (int)dim.nrow;
	dim.ncoli = (int)dim.ncol;
	dim.nrowlen = NumLen(dim.nrow);
	dim.Ndash = 8;
	dim.remain = dim.nrowi - dim.nn;

	prt_colnames(df, &dim.ncoli, &dim.nrowlen);

	int begin = 0;
	prit(df, &begin, &dim.nn, &dim.ncoli);

	prt_hspace(&dim.nrowlen);

	prt_dashes(&dim.ncoli, &dim.Ndash);

	prit(df, &dim.remain, &dim.nrowi, &dim.ncoli);

	return R_NilValue;
}
