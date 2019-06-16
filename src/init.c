#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <R_ext/Rdynload.h>

extern SEXP _sexptype(SEXP);
extern SEXP _ht_df(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_sexptype", (DL_FUNC) &_sexptype, 1},
    {"_ht_df",       (DL_FUNC) &_ht_df, 2},
    {NULL, NULL, 0}
};

void R_init_mmy(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
