
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

		for (R_xlen_t i = 0; i < *ncol; i++) {

			SEXP el = PROTECT(VECTOR_ELT(df, i));

			if (Rf_isReal(el)) {

				/* Rprintf("%f ", REAL(el)[j]); */

				Rprintf("%*s.9g\n", 5, "", REAL(el)[j]);

			} else if (Rf_isInteger(el)) {

				Rprintf("%d ", INTEGER(el)[j]);

			} else if (Rf_isString(el)) {

				Rprintf("%s ", CHAR(STRING_ELT(el, j)));

			} else if (Rf_isFactor(el)) {

				SEXP attr = PROTECT(Rf_asCharacterFactor(el));

				Rprintf("%s ", CHAR(STRING_ELT(attr, j)));

				UNPROTECT(1);

			} else {
				Rf_error("unknown column type");
			}
			UNPROTECT(1);
		}

		Rprintf("\n");
	}
}

int * find_max_per_col(SEXP df, int *inds, int nn, int ncol) {

	// TODO reduce for loop repetition

	int l_inds = nn * 2;

	int *maxs;
	maxs = (int *) R_alloc(ncol, sizeof(int));

	// rows:
	for (R_xlen_t i = 0; i < ncol; i++) {

		SEXP col = PROTECT(VECTOR_ELT(df, i));

		int *ptr;
		ptr = (int *) R_alloc(l_inds, sizeof(int));

		char str[100];
		if (Rf_isReal(col)) {
			double *px;
			px = REAL(col);
			for (int k = 0; k < l_inds; k++) {
				int ki = inds[k] - 1;
				sprintf(str, "%.1f", px[ki]);
				ptr[k] = nchar(str);
				Rprintf("i%i %s\n", i, str);
			}
		} else if (Rf_isInteger(col)) {
			int *px;
			px = INTEGER(col);
			for (int k = 0; k < l_inds; k++) {
				int ki = inds[k] - 1;
				sprintf(str, "%i", px[ki]);
				ptr[k] = nchar(str);
				Rprintf("i%i %s\n", i, str);
			}
		} else if (Rf_isString(col)) {
			for (int k = 0; k < l_inds; k++) {
				int ki = inds[k] - 1;
				sprintf(str, "%s", CHAR(STRING_ELT(col, ki)));
				ptr[k] = nchar(str);
				Rprintf("i%i %s\n", i, str);
			}
		} else if (Rf_isFactor(col)) {
			SEXP attr = PROTECT(Rf_asCharacterFactor(col));
			for (int k = 0; k < l_inds; k++) {
				int ki = inds[k] - 1;
				sprintf(str, "%s", CHAR(STRING_ELT(attr, ki)));
				ptr[k] = nchar(str);
				Rprintf("i%i %s\n", i, str);
			}
			UNPROTECT(1);
		} else if (Rf_isLogical(col)) {
			int *px;
			px = LOGICAL(col);
			for (int k = 0; k < l_inds; k++) {
				// TODO what about NA values?
				char *s = (px[k] == 1) ? "TRUE" : "FALSE";
				ptr[k] = nchar(s);
				Rprintf("i%i %s\n", i, s);
			}
		} else {
			Rf_error("unknown column type");
		}
		int max_col = int_maxima(ptr, &l_inds);
		Rprintf("%i max_col: %i\n", i, max_col);

		/* column names: */
		// SEXP colnames = PROTECT(Rf_getAttrib(df, R_NamesSymbol));
		// const char * colname = CHAR(STRING_ELT(colnames, i));
		// int l_colname = strlen(colname);

		// get the higher one for maximum:
		// if (max_col > l_colname) {
		// 	maxs_cols[i] = max_col;
		// } else {
		// 	maxs[i] = l_colname;
		// }

		// UNPROTECT(2);
	}

	// random value for maxs for now:
	*maxs = 101;
	return maxs;
}

int * find_indices(int nn, int nrow) {
	int dnn = nn * 2;
	int *arr;
	arr = (int *) R_alloc(dnn, sizeof(int));
	int remain = nrow - nn + 1;
	int i, j;
	for(i = 0, j = remain; i < dnn; i++) {
		if (i < nn) {
			arr[i] = i+1;
		} else {
			arr[i] = j;
			j = j + 1;
		}
	}
	return arr;
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

	/* set const vars: */
	const int nn = Rf_asInteger(n);
	const R_xlen_t nrow = Rf_xlength(Rf_getAttrib(df, R_RowNamesSymbol));
	const R_xlen_t ncol = Rf_xlength(df);

	// start :
	// int nrowlen = nint((int*)&nrow);

	int *indices;
	indices = find_indices(nn, nrow);

	int *max_length;
	max_length = find_max_per_col(df, indices, nn, (int)ncol);

	/* prt_colnames(df, &dim.ncoli, &dim.nrowlen);

	int begin = 0;
	prit(df, &begin, &dim.nn, &dim.ncoli);

	prt_hspace(&dim.nrowlen);

	int Ndash = 8;
	prt_dashes(&dim.ncoli, &dim.Ndash);

	prit(df, &dim.remain, &dim.nrowi, &dim.ncoli); */

	/* for(int i = 0; i < ncol; i++) {
		Rprintf("max rows: %i\n", max_length[i]);
	} */

	// for(int i = 0; i < ncol; i++) {
	// 	Rprintf("max_length: %i\n", max_length[i]);
	// }

	return R_NilValue;
}
