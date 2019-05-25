
/*
 * Data structures by using R's built-in pairlist
 *
 * The function names are prepended by "p" to imply they are implemented by
 * pairlists.
 * 
 * ### Pairlist ###
 * + pairlist_create
 * + pairlist_first_element
 * + pairlist_last_element
 * + pairlist_length
 * + pairlist_reverse
 * + pairlist_is_empty
 * + pairlist_is_pairlist
 * + stop_if_not_pairlist
 * ### Stack ###
 * + p_stack_init
 * + p_push
 * + p_pop
 * + p_is_stack_empty
 * ### Queue ###
 * + p_queue_init
 * + p_enqueue
 * + p_dequeue
 * + p_is_queue_empty
 */

#include "mmy.h"

/* PAIRLIST */
static SEXP pairlist_create(int length);
static SEXP pairlist_first_element(SEXP x);
static SEXP pairlist_last_element(SEXP x);
static SEXP pairlist_length(SEXP x);
static SEXP pairlist_reverse(SEXP x);
static Rboolean pairlist_is_empty(SEXP x);
static Rboolean pairlist_is_pairlist(SEXP x);
static void stop_if_not_pairlist(SEXP x);

/* STACK */
SEXP p_stack_init();
SEXP p_push(SEXP x, SEXP val);
SEXP p_pop(SEXP x);
Rboolean p_is_stack_empty(SEXP x);

/* QUEUE */
SEXP p_queue_init();
SEXP p_enqueue(SEXP x, SEXP val);
SEXP p_dequeue(SEXP x);
Rboolean p_is_queue_empty(SEXP x);
