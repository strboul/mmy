
#include "mmy.h"

SEXP _sexptype(SEXP x) {
    // SEXPTYPE list taken from the `Rinternals.h` file.
    const char* s;
    switch (TYPEOF(x)) {
    case NILSXP: s = "NILSXP"; break;
    case SYMSXP: s = "SYMSXP"; break;
    case LISTSXP: s = "LISTSXP"; break;
    case CLOSXP: s = "CLOSXP"; break;
    case ENVSXP: s = "ENVSXP"; break;
    case PROMSXP: s = "PROMSXP"; break;
    case LANGSXP: s = "LANGSXP"; break;
    case SPECIALSXP: s = "SPECIALSXP"; break;
    case BUILTINSXP: s = "BUILTINSXP"; break;
    case CHARSXP: s = "CHARSXP"; break;
    case LGLSXP: s = "LGLSXP"; break;
    case INTSXP: s = "INTSXP"; break;
    case REALSXP: s = "REALSXP"; break;
    case CPLXSXP: s = "CPLXSXP"; break;
    case STRSXP: s = "STRSXP"; break;
    case DOTSXP: s = "DOTSXP"; break;
    case ANYSXP: s = "ANYSXP"; break;
    case VECSXP: s = "VECSXP"; break;
    case EXPRSXP: s = "EXPRSXP"; break;
    case BCODESXP: s = "BCODESXP"; break;
    case EXTPTRSXP: s = "EXTPTRSXP"; break;
    case WEAKREFSXP: s = "WEAKREFSXP"; break;
    case RAWSXP: s = "RAWSXP"; break;
    case S4SXP: s = "S4SXP"; break;
    case NEWSXP: s = "NEWSXP"; break;
    case FREESXP: s = "FREESXP"; break;
    case FUNSXP: s = "FUNSXP"; break;
    default: Rf_error("SEXPTYPE not found");
    }
    return Rf_mkString(s);
}

