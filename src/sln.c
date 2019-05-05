
#include "sln.h"

static traverse(SEXP list, int n) {

  //Rf_getAttrib(list, R_NamesSymbol);
  traverse(list, n);
}

// static get_list_names(SEXP list, int n);

// p Rf_PrintValue(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(list,0), 0), 2));
// p Rf_getAttrib(VECTOR_ELT(list, 0), R_NamesSymbol);

SEXP _sln (SEXP list, SEXP query) {
  
  // check and transform inputs
  if (!Rf_isNewList(list)) {
    Rf_error("input must be a list");
  }
  if (!Rf_isString(query)) {
    Rf_error("query must be character");
  }

  int lenlist1;

  lenlist1 = (int)Rf_xlength(list);

  SEXP initNames = PROTECT(Rf_getAttrib(list, R_NamesSymbol));
  // get list names - traverse:
  for (int i = 0; i < lenlist1; i++) {
    SEXP names = PROTECT(Rf_getAttrib(VECTOR_ELT(list, i), R_NamesSymbol));
  }
  
  // match list names with the query string:

  // replace the parts in list names to highlight:

  UNPROTECT(2);
  return R_NilValue;
}

