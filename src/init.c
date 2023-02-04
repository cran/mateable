#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _mateable_daysEither_noself(SEXP, SEXP, SEXP);
extern SEXP _mateable_daysEither_self(SEXP, SEXP, SEXP);
extern SEXP _mateable_daysSync_noself(SEXP, SEXP, SEXP);
extern SEXP _mateable_daysSync_self(SEXP, SEXP, SEXP);
extern SEXP _mateable_kemp_ind(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _mateable_pair_dioecious(SEXP);
extern SEXP _mateable_pair_si_ech(SEXP, SEXP);
extern SEXP _mateable_row_kth(SEXP, SEXP);
extern SEXP _mateable_row_medians(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_mateable_daysEither_noself", (DL_FUNC) &_mateable_daysEither_noself, 3},
    {"_mateable_daysEither_self",   (DL_FUNC) &_mateable_daysEither_self,   3},
    {"_mateable_daysSync_noself",   (DL_FUNC) &_mateable_daysSync_noself,   3},
    {"_mateable_daysSync_self",     (DL_FUNC) &_mateable_daysSync_self,     3},
    {"_mateable_kemp_ind",          (DL_FUNC) &_mateable_kemp_ind,          5},
    {"_mateable_pair_dioecious",    (DL_FUNC) &_mateable_pair_dioecious,    1},
    {"_mateable_pair_si_ech",       (DL_FUNC) &_mateable_pair_si_ech,       2},
    {"_mateable_row_kth",           (DL_FUNC) &_mateable_row_kth,           2},
    {"_mateable_row_medians",       (DL_FUNC) &_mateable_row_medians,       1},
    {NULL, NULL, 0}
};

void R_init_mateable(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
