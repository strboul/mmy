
#include "pairlist.h"

/* PAIRLIST */

static SEXP pairlist_create(int length) {
    return Rf_allocList(length);
}

static SEXP pairlist_first_element(SEXP x) {
    stop_if_not_pairlist(x);
    return CAR(x);
}

static SEXP pairlist_last_element(SEXP x) {
    stop_if_not_pairlist(x);
    SEXP next = x;
    while (next != R_NilValue) {
        x = next;
        next = CDR(next);
    }
    return x;
}

static SEXP pairlist_length(SEXP x) {
    stop_if_not_pairlist(x);
    int len = Rf_xlength(x);
    return Rf_ScalarInteger(len);
}

static SEXP pairlist_reverse(SEXP x) {
    SEXP prev = R_NilValue;
    SEXP tail = x;
    SEXP next;
    while (tail != R_NilValue) {
        next = CDR(tail);
        SETCDR(tail, prev);
        prev = tail;
        tail = next;
    }
    return prev;
}

static Rboolean pairlist_is_empty(SEXP x) {
    stop_if_not_pairlist(x);
    return (Rf_xlength(x) == 0) ? TRUE : FALSE;
}

static Rboolean pairlist_is_pairlist(SEXP x) {
    return Rf_isPairList(x);
}

static void stop_if_not_pairlist(SEXP x) {
    if (!pairlist_is_pairlist(x)) {
        Rf_errorcall(R_NilValue, "not a pairlist");
    }
}

/* STACK */

SEXP p_stack_init() {
    return pairlist_create(0);
}

SEXP p_push(SEXP x, SEXP val) {
    stop_if_not_pairlist(x);
    return Rf_cons(val, x);
}

SEXP p_pop(SEXP x) {
    stop_if_not_pairlist(x);
    return CDR(x);
}

Rboolean p_is_stack_empty(SEXP x) {
    stop_if_not_pairlist(x);
    return pairlist_is_empty(x);
}

/* QUEUE */

SEXP p_queue_init() {
    return pairlist_create(0);
}

SEXP p_enqueue(SEXP x, SEXP val) {
    stop_if_not_pairlist(x);
    return Rf_cons(val, x);
}

SEXP p_dequeue(SEXP x) {
    stop_if_not_pairlist(x);
    SEXP rev = CDR(pairlist_reverse(x));
    return pairlist_reverse(rev);
}

Rboolean p_is_queue_empty(SEXP x) {
    stop_if_not_pairlist(x);
    return pairlist_is_empty(x);
}
