
#include "ht.h"

void prt_hspace(int len) { Rprintf("%*s", len, ""); }

void prt_colnames(SEXP df, R_xlen_t ncol, R_xlen_t nrow) {
	SEXP names = PROTECT(Rf_getAttrib(df, R_NamesSymbol));
	int nrowlen = nint((int)nrow);
	prt_hspace(nrowlen + 1);

	int padding;
	for (R_xlen_t i = 0; i < ncol; i++) {
		padding = (i == ncol - 1) ? 0 : 1;
		Rprintf("%s%*s", CHAR(STRING_ELT(names, i)), padding, "");
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

void prit(SEXP df, int start, int end, R_xlen_t ncol, int len_nrow, int *row_max_length) {

	R_xlen_t j;
	for (j = start; j < end; j++) {

		/* print row indices */
		int indlen = nint((int)j+1);
		int ind_width = len_nrow - indlen;
		Rprintf("%*s%i: ", ind_width, "", j + 1);

		for (R_xlen_t i = 0; i < ncol; i++) {
			SEXP el = PROTECT(VECTOR_ELT(df, i));
			int max_len, width;
			max_len = row_max_length[i];
			char str[MAX_ROW_SIZE];
			if (Rf_isReal(el)) {
				snprintf(str, MAX_ROW_SIZE, "%.1f", REAL(el)[j]);
				width = max_len - nchar(str);
			} else if (Rf_isInteger(el)) {
				snprintf(str, MAX_ROW_SIZE, "%i", INTEGER(el)[j]);
				width = max_len - nchar(str);
			} else if (Rf_isString(el)) {
				snprintf(str, MAX_ROW_SIZE, "%s", CHAR(STRING_ELT(el, j)));
				width = max_len - nchar(str);
			} else if (Rf_isFactor(el)) {
				SEXP attr = PROTECT(Rf_asCharacterFactor(el));
				snprintf(str, MAX_ROW_SIZE, "%s", CHAR(STRING_ELT(attr, j)));
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
	maxs = (int *) R_alloc(ncol, sizeof(R_xlen_t));

	for (R_xlen_t i = 0; i < ncol; i++) {

		SEXP col = PROTECT(VECTOR_ELT(df, i));

		int *ptr;
		ptr = (int *) R_alloc(l_inds, sizeof(int));

		char str[MAX_ROW_SIZE];
		for (int k = 0; k < l_inds; k++) {
			int ki = inds[k] - 1;
			if (Rf_isReal(col)) {
				double *px;
				px = REAL(col);
				snprintf(str, MAX_ROW_SIZE, "%.1f", px[ki]);	
			} else if (Rf_isInteger(col)) {
				int *px;
				px = INTEGER(col);
				snprintf(str, MAX_ROW_SIZE, "%i", px[ki]);
			} else if (Rf_isString(col)) {
				snprintf(str, MAX_ROW_SIZE, "%s", CHAR(STRING_ELT(col, ki)));
			} else if (Rf_isFactor(col)) {
				SEXP attr = PROTECT(Rf_asCharacterFactor(col));
				snprintf(str, MAX_ROW_SIZE, "%s", CHAR(STRING_ELT(attr, ki)));
				UNPROTECT(1);
			} else if (Rf_isLogical(col)) {
				int *px;
				px = LOGICAL(col);
				// TODO error: what about NA values?
				char *str = (px[k] == 1) ? "TRUE" : "FALSE";
			} else {
				Rf_error("unknown column type");
			}
			/* assign it to pointer: */
			ptr[k] = nchar(str);
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

/*
 * Finds indices started on R index level 
 * 
 * A side note: The memory allocated with R_alloc does not have to be 
 * freed at the end as "R will reclaim the memory at the end of the 
 * call"
 */
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

	int len_nrow;
	len_nrow = nint((int)nrow);

	int *indices;
	indices = find_indices(nn, nrow);

	int *max_length;
	max_length = find_max_per_col(df, indices, nn, ncol);

	prt_colnames(df, ncol, nrow);

	/* head: */
	int begin = 0;
	prit(df, begin, nn, ncol, len_nrow, max_length);

	/* print dashes */
	prt_hspace(len_nrow+1);
	prt_dashes(ncol, max_length);

	/* tail: */
	int remain = (int)nrow - nn;
	prit(df, remain, nrow, ncol, len_nrow, max_length);

	return R_NilValue;
}
