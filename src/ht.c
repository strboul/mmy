
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

				Rprintf("%*s%f ", 5, "", REAL(el)[j]);

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

int * find_max_per_col(SEXP df, int *inds, int ncol) {

	int *maxs;
	maxs = (int *) malloc(sizeof(int) * ncol);

	int l_inds = sizeof(inds) / sizeof(inds[0]);

	SEXP colnames = PROTECT(Rf_getAttrib(df, R_NamesSymbol));

	// rows:
	for (R_xlen_t i = 0; i < ncol; i++) {
		SEXP col = PROTECT(VECTOR_ELT(df, i));
		int *cont;
		cont = (int *) malloc(sizeof(int) * l_inds);
		if (Rf_isReal(col)) {
			double *px = REAL(col);
			for (int k = 0; k < l_inds; k++) {
				int ki = *(inds+k);
				cont[i] = px[ki];
			}
		} else if (Rf_isInteger(col)) {
			
		} else if (Rf_isString(col)) {

		} else if (Rf_isFactor(col)) {
			
		} else {
			Rf_error("unknown column type");
		}

		// find maximum columns:
		int max_col = maxima(cont, &l_inds);
		free(cont);
		
		// column names:
		const char * colname = CHAR(STRING_ELT(colnames, i));
		int l_colname = strlen(colname);
		//Rprintf("max_colname: %i\n", l_colname);

		// get the higher one for maximum:
		if (max_col > l_colname) {
			maxs[i] = max_col;
		} else {
			maxs[i] = l_colname;
		}

		UNPROTECT(1);
	}

	return maxs;
}

int * find_indices(int nn, int nrow) {
	int dnn = nn * 2;
    int *arr;
	arr = (int *) malloc(sizeof(int) * dnn);
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

	// const vars:
	const int nn = Rf_asInteger(n);
	const R_xlen_t nrow = Rf_xlength(Rf_getAttrib(df, R_RowNamesSymbol));
	const R_xlen_t ncol = Rf_xlength(df);

	// start :
	// int nrowlen = nint((int*)&nrow);

	int *indices;
	indices = find_indices(nn, nrow);

	int *max_length;
	max_length = find_max_per_col(df, indices, (int)ncol);

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

	for(int i = 0; i < ncol; i++)
	{
		Rprintf("max_length: %i\n", max_length[i]);
	}
	

	free(max_length);

	free(indices);
	return R_NilValue;
}
