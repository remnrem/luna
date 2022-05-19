#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP R_filter(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R1d_denoise(SEXP, SEXP);
extern SEXP Radd_annot(SEXP);
extern SEXP Radd_annot_fromR(SEXP, SEXP);
extern SEXP Rannot(SEXP);
extern SEXP Rannots();
extern SEXP Rattach_edf(SEXP, SEXP, SEXP);
extern SEXP Rchannels();
extern SEXP Rclear_vars();
extern SEXP Rdb2retval(SEXP, SEXP);
extern SEXP Rdesc();
extern SEXP Rdrop();
extern SEXP Reval_cmd(SEXP);
extern SEXP Reval_cmd_noreturns(SEXP);
extern SEXP Reval_get_returns();
extern SEXP Reval_init_returns();
extern SEXP Rflush_log();
extern SEXP Riterate(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Rlogmode(SEXP);
extern SEXP Rmask(SEXP);
extern SEXP Rmatrix_epochs(SEXP, SEXP, SEXP);
extern SEXP Rmatrix_intervals(SEXP, SEXP, SEXP);
extern SEXP Rproblem();
extern SEXP Rset_var(SEXP, SEXP);
extern SEXP Rsetproblem(SEXP);
extern SEXP Rshow_var(SEXP);
extern SEXP Rstat();

static const R_CallMethodDef CallEntries[] = {
  {"R_filter",            (DL_FUNC) &R_filter,            6},
  {"R1d_denoise",         (DL_FUNC) &R1d_denoise,         2},
  {"Radd_annot",          (DL_FUNC) &Radd_annot,          1},
  {"Radd_annot_fromR",    (DL_FUNC) &Radd_annot_fromR,    2},
  {"Rannot",              (DL_FUNC) &Rannot,              1},
  {"Rannots",             (DL_FUNC) &Rannots,             0},
  {"Rattach_edf",         (DL_FUNC) &Rattach_edf,         3},
  {"Rchannels",           (DL_FUNC) &Rchannels,           0},
  {"Rclear_vars",         (DL_FUNC) &Rclear_vars,         0},
  {"Rdb2retval",          (DL_FUNC) &Rdb2retval,          2},
  {"Rdesc",               (DL_FUNC) &Rdesc,               0},
  {"Rdrop",               (DL_FUNC) &Rdrop,               0},
  {"Reval_cmd",           (DL_FUNC) &Reval_cmd,           1},
  {"Reval_cmd_noreturns", (DL_FUNC) &Reval_cmd_noreturns, 1},
  {"Reval_get_returns",   (DL_FUNC) &Reval_get_returns,   0},
  {"Reval_init_returns",  (DL_FUNC) &Reval_init_returns,  0},
  {"Rflush_log",          (DL_FUNC) &Rflush_log,          0},
  {"Riterate",            (DL_FUNC) &Riterate,            6},
  {"Rlogmode",            (DL_FUNC) &Rlogmode,            1},
  {"Rmask",               (DL_FUNC) &Rmask,               1},
  {"Rmatrix_epochs",      (DL_FUNC) &Rmatrix_epochs,      3},
  {"Rmatrix_intervals",   (DL_FUNC) &Rmatrix_intervals,   3},
  {"Rproblem",            (DL_FUNC) &Rproblem,            0},
  {"Rset_var",            (DL_FUNC) &Rset_var,            2},
  {"Rsetproblem",         (DL_FUNC) &Rsetproblem,         1},
  {"Rshow_var",           (DL_FUNC) &Rshow_var,           1},
  {"Rstat",               (DL_FUNC) &Rstat,               0},
  {NULL, NULL, 0}
};

void R_init_luna(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
