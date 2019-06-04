
#include "sln.h"

static void recur_list_names(SEXP node, int level, SEXP parent) {

    SEXP _node = PROTECT(node); // copy node to overcome 'uninitialized variable' error.

    // recursively subset list unless level is 0:
    if (level != -1) {
        _node = PROTECT(VECTOR_ELT(_node, level));
        if (TYPEOF(_node) != VECSXP) {
            return;
        }
    }

    SEXP node_names = PROTECT(Rf_getAttrib(_node, R_NamesSymbol));

    SEXP visited = PROTECT(p_stack_init());

    SEXP to_visit = PROTECT(p_queue_init());

    for (R_xlen_t i = 0; i < Rf_xlength(node_names); i++) {
        to_visit = p_enqueue(to_visit, Rf_ScalarString(STRING_ELT(node_names, i)));  
    }

    SEXP parent_pattern, dest_pattern, front;
    const char* parent_pattern_str;
    const char* front_str;
    char dest[STR_BUFF_SIZE];

    while (Rf_xlength(to_visit) > 0) {

        parent_pattern = (TYPEOF(parent) == NILSXP) ? Rf_mkString(empty_str) : parent;
        parent_pattern_str = CHAR(STRING_ELT(parent_pattern, 0));

        front = p_queue_front(to_visit);
        front_str = CHAR(STRING_ELT(front, 0));

        snprintf(dest, STR_BUFF_SIZE, "%s$%s", parent_pattern_str, front_str);
        dest_pattern = Rf_mkString(dest);

        Rprintf("%s\n", dest);

        visited = p_push(visited, dest_pattern);
        to_visit = p_dequeue(to_visit);

    }

    SEXP next_parent;
    for (R_xlen_t i = 0; i < Rf_xlength(visited); i++) {
        next_parent = p_stack_top(visited);
        recur_list_names(_node, i, next_parent);
    }
}

void validate_sln(SEXP list, SEXP query) {
    if (!Rf_isNewList(list)) {
        Rf_error("input must be a list");
    }
    if (!Rf_isValidString(query) || Rf_isNull(query)) {
        Rf_error("query must be a character vector");
    }
}

SEXP _sln(SEXP list, SEXP query) {

    // check and transform inputs:
    validate_sln(list, query);

    recur_list_names(list, -1, R_NilValue);

    // match list names with the query string:
    
    // skip matching if query string is null or empty:

    // replace the parts in list names to highlight:

    //UNPROTECT(2);
    return R_NilValue;
}
