
// Rf_xlength is for up to 64-bit unlike Rf_length which is 32-bit.
#include "mmy.h"

void prt_hspace(int len) { Rprintf("%*s", len, ""); }

void prt_colnames(SEXP df, int ncol, int nrow) {
	SEXP names = PROTECT(Rf_getAttrib(df, R_NamesSymbol));
	int nrowlen = nint(&nrow);
	prt_hspace(nrowlen);
	for (R_xlen_t i = 0; i < ncol; i++) {
		Rprintf("%s ", CHAR(STRING_ELT(names, i)));
	}
	Rprintf("\n");
	UNPROTECT(1);
}

/* print dashes same as the number of max chars */
void prt_dashes(R_xlen_t ncol, int *row_max_length) {
	int max_len, width;
	for (R_xlen_t i = 0; i < ncol; i++) {
		max_len = row_max_length[i];
		width = max_len;
		for (int j = 0; j < width; j++) {
			Rprintf("%s","-");
		}
		Rprintf(" ");
	}
	Rprintf("\n");
}

void prit(SEXP df, R_xlen_t start, int end, R_xlen_t ncol, int *row_max_length) {

	int j;
	for (j = start; j < end; j++) {

		Rprintf("%i: ", j + 1);

		for (R_xlen_t i = 0; i < ncol; i++) {

			SEXP el = PROTECT(VECTOR_ELT(df, i));
			int max_len, width;
			max_len = row_max_length[i];
			char str[100];
			if (Rf_isReal(el)) {
				sprintf(str, "%.1f", REAL(el)[j]);
				width = max_len - nchar(str);
			} else if (Rf_isInteger(el)) {
				sprintf(str, "%i", INTEGER(el)[j]);
				width = max_len - nchar(str);
			} else if (Rf_isString(el)) {
				sprintf(str, "%s", CHAR(STRING_ELT(el, j)));
				width = max_len - nchar(str);
			} else if (Rf_isFactor(el)) {
				SEXP attr = PROTECT(Rf_asCharacterFactor(el));
				sprintf(str, "%s", CHAR(STRING_ELT(attr, j)));
				width = max_len - nchar(str);
				UNPROTECT(1);
			} else if (Rf_isLogical(el)) {
				// TODO
				Rprintf("%i ", LOGICAL(el)[j]);
			} else {
				Rf_error("unknown column type");
			}
			Rprintf("%*s%s ", width, "", str);
			UNPROTECT(1);
		}
		Rprintf("\n");
	}
}

int * find_max_per_col(SEXP df, int *inds, int nn, R_xlen_t ncol) {

	int l_inds = nn * 2;

	int *maxs;
	maxs = (int *) R_alloc(ncol, sizeof(int));

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
			}
		} else if (Rf_isInteger(col)) {
			int *px;
			px = INTEGER(col);
			for (int k = 0; k < l_inds; k++) {
				int ki = inds[k] - 1;
				sprintf(str, "%i", px[ki]);
				ptr[k] = nchar(str);
			}
		} else if (Rf_isString(col)) {
			for (int k = 0; k < l_inds; k++) {
				int ki = inds[k] - 1;
				sprintf(str, "%s", CHAR(STRING_ELT(col, ki)));
				ptr[k] = nchar(str);
			}
		} else if (Rf_isFactor(col)) {
			SEXP attr = PROTECT(Rf_asCharacterFactor(col));
			for (int k = 0; k < l_inds; k++) {
				int ki = inds[k] - 1;
				sprintf(str, "%s", CHAR(STRING_ELT(attr, ki)));
				ptr[k] = nchar(str);
			}
			UNPROTECT(1);
		} else if (Rf_isLogical(col)) {
			int *px;
			px = LOGICAL(col);
			for (int k = 0; k < l_inds; k++) {
				// TODO error: what about NA values?
				char *s = (px[k] == 1) ? "TRUE" : "FALSE";
				ptr[k] = nchar(s);
			}
		} else {
			Rf_error("unknown column type");
		}
		int max_col = int_maxima(ptr, &l_inds);

		/* column names: */
		SEXP colnames = PROTECT(Rf_getAttrib(df, R_NamesSymbol));
		const char * colname = CHAR(STRING_ELT(colnames, i));
		int len_colname = strlen(colname);

		/* higher length one becoming maximum for that col: */
		if (max_col > len_colname) {
			maxs[i] = max_col;
		} else {
			maxs[i] = len_colname;
		}
		UNPROTECT(2);
	}
	return maxs;
}

/* finds indices started on R index level */
int * find_indices(int nn, int nrow) {
	int dnn = nn * 2;
	int *arr;
	arr = (int *) R_alloc(dnn, sizeof(int));
	int remain = nrow - nn + 1;
	int i, j;
	for(i = 0, j = remain; i < dnn; i++) {
		if (i < nn) {
			arr[i] = i + 1;
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

	int *indices;
	indices = find_indices(nn, nrow);

	int *max_length;
	max_length = find_max_per_col(df, indices, nn, ncol);

	prt_colnames(df, ncol, nrow);

	/* head: */
	R_xlen_t begin = 0;
	prit(df, begin, nn, ncol, max_length);

	/* print dashes */
	int len_nrow, intnrow;
	intnrow = (int)nrow;
	len_nrow = nint(&intnrow);
	prt_hspace(len_nrow);
	prt_dashes(ncol, max_length);

	/* tail: */
	R_xlen_t remain = nrow - (R_xlen_t)nn;
	prit(df, remain, nrow, ncol, max_length);

	return R_NilValue;
}
