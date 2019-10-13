
#include "mmy.h"
#include "diff_int.h"

#define GRP_START_IND 1 // group start index
#define GRP_INIT_PIVOT 0 // initial pivot value

static void is_grp_cns_valid_vector (SEXP x);
SEXP _grp_cns(SEXP x, SEXP early_exit);

