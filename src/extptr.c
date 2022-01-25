#include <R.h>
#include <Rinternals.h>

static void eptrFinalizer(SEXP eptr) {
  if(!R_ExternalPtrAddr(eptr)) return;
  R_ClearExternalPtr(eptr); 
}

SEXP create_eptr(SEXP x) {
  SEXP eptr = PROTECT(R_MakeExternalPtr(x, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(eptr, eptrFinalizer, TRUE);
  UNPROTECT(1);
  return eptr;
}

void * get_eptr(SEXP x) {
  return R_ExternalPtrAddr(x);
}

static const R_CallMethodDef CallEntries[] = {
  {"C_create_eptr", (DL_FUNC) &create_eptr, 1},
  {"C_get_eptr", (DL_FUNC) &get_eptr, 1},
  {NULL, NULL, 0}
};

void R_init_plm(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}