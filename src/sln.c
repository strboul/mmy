
#include "sln.h"

/*
 *  @brief Get names from the list object.
 *  @param list SEXP list object.
 *  @param index index to slice list.
 *  @param collect list object to save names. 
 */
static void get_list_names(SEXP list, int index, SEXP collect) {
  if (TYPEOF(list) != VECSXP) {
    return;
  } else {
    SEXP elem = PROTECT(VECTOR_ELT(list, index));
    SEXP names = PROTECT(Rf_getAttrib(elem, R_NamesSymbol));
    SET_VECTOR_ELT(collect, index, names);
  }
}

// p Rf_PrintValue(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(list,0), 0), 2));

SEXP _sln (SEXP list, SEXP query) {
  
  // check and transform inputs:
  if (!Rf_isNewList(list)) {
    Rf_error("input must be a list");
  }
  if (!Rf_isValidString(query)) {
    Rf_error("query must be character");
  }

  int lenlist1;
  lenlist1 = (int)Rf_xlength(list);
  SEXP initNames = PROTECT(Rf_getAttrib(list, R_NamesSymbol));

  // get list names - traverse:
  SEXP res = PROTECT(Rf_allocVector(VECSXP, lenlist1));
  for (size_t i = 0; i < lenlist1; i++) {
    get_list_names(list, i, res);
  }
  
  Rf_PrintValue(res);
  // match list names with the query string:

  // replace the parts in list names to highlight:

  //UNPROTECT(2);
  return R_NilValue;
}

