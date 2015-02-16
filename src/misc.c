#include "alike.h"
#include "pfhash.h"

// - Helper Functions ----------------------------------------------------------

/* equivalent to `mode` in R, note this is a bit approximate and just trying to
hit the obvious corner cases between `typeof` and `mode`*/

SEXP ALIKEC_mode(SEXP obj) {
  const char * class;
  switch(TYPEOF(obj)) {
    case NILSXP: class = "NULL"; break;
    case SYMSXP: class = "name"; break;
    case FUNSXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case CLOSXP: class = "function"; break;
    case LANGSXP: class = "call"; break;
    case REALSXP: class = "numeric"; break;
    default: class = type2char(TYPEOF(obj));
  }
  return(mkString(class));
}
/*
returns specified class, or implicit class if none
*/
SEXP ALIKEC_class(SEXP obj, SEXP class) {
  if(class == R_NilValue) return(ALIKEC_mode(obj));
  return class;
}
// - Abstraction ---------------------------------------------------------------
/*
sets the select `tsp` values to zero
*/
SEXP ALIKEC_abstract_ts(SEXP x, SEXP what) {
  if(TYPEOF(what) != INTSXP)
    error("Logic Error: expected integer `what`, contact maintainer");
  SEXP x_cp = PROTECT(duplicate(x));
  SEXP attrs = ATTRIB(x_cp), attrs_cpy;
  for(attrs_cpy = attrs; attrs_cpy != R_NilValue; attrs_cpy = CDR(attrs_cpy))
    if(TAG(attrs_cpy) == R_TspSymbol) break;
  if(attrs_cpy == R_NilValue)
    error("Logic Error: object does not appear to have a `tsp` attribute");
  SEXP tsp = CAR(attrs_cpy);
  if(TYPEOF(tsp) != REALSXP || XLENGTH(tsp) != 3)
    error("Logic Error: unexpected time series attribute format");

  double * tsp_dbl = REAL(tsp);
  for(R_xlen_t i = 0; i < XLENGTH(what); i++) {
    int ind_abs = INTEGER(what)[i];
    if(ind_abs < 0 || ind_abs > 2)
      error("Logic Error: unexpected index to abstract %d", ind_abs);
    tsp_dbl[ind_abs] = 0.0;
  }
  UNPROTECT(1);
  return x_cp;
}
// - Testing Function ----------------------------------------------------------
SEXP ALIKEC_test() {

  return R_NilValue;

  // int tmp = 0;
  // int * x = &tmp;
  // return R_tryEval(obj1, R_GlobalEnv, x);
  // SET_TYPEOF(ALIKEC_CALL_matchcall_sub, LANGSXP);
  // SEXP ALIKEC_CALL_matchcall = PROTECT(list3(ALIKEC_SYM_matchcall, R_NilValue, ALIKEC_CALL_matchcall_sub));
  // SET_TYPEOF(ALIKEC_CALL_matchcall, LANGSXP);
  // UNPROTECT(2);


  // if(TYPEOF(obj1) != LISTSXP || TYPEOF(obj2) != LISTSXP)
  //   error("incorrect input type");
  // SEXP res;
  // R_xlen_t i, j;

  // PrintValue(TAG(R_NilValue));
  // if(asInteger(obj3)) for(R_xlen_t i = 0; obj1 != R_NilValue && i < 25; i++, obj1 = CDR(obj1)) 1;
  // else xlength(obj1) < 25;
  // if(asInteger(obj3)) {
  //   pfHashTable * hash = pfHashCreate(NULL);
  //   for(i = 0; obj1 != R_NilValue; obj1 = CDR(obj1), i++) {
  //     const char * symb_chr = CHAR(PRINTNAME(TAG(obj1)));
  //     char * symb_abs = pfHashFind(hash, (char *) symb_chr);
  //     if(symb_abs == NULL) {
  //       symb_abs = (char *) CHAR(asChar(CAR(obj1)));
  //       pfHashSet(hash, (char *) symb_chr, symb_abs);
  //   } }
  //   res = PROTECT(allocVector(STRSXP, i));

  //   for(j = 0; obj2 != R_NilValue && j < i; obj2 = CDR(obj2), j++) {
  //     const char * symb_chr = CHAR(PRINTNAME(TAG(obj2)));
  //     char * map_val = pfHashFind(hash, (char *) symb_chr);
  //     SET_STRING_ELT(res, j, mkChar(map_val == NULL ? "" : map_val));
  //   }
  // } else {
  //   res = PROTECT(allocVector(STRSXP, xlength(obj1)));
  //   SEXP obj2_sub;
  //   for(i = 0; obj1 != R_NilValue; obj1 = CDR(obj1), i++) {
  //     const char * symb_chr = CHAR(PRINTNAME(TAG(obj1)));
  //     for(obj2_sub = obj2; obj2_sub != R_NilValue; obj2_sub = CDR(obj2_sub)) {
  //       const char * symb_chr_2 = CHAR(PRINTNAME(TAG(obj2_sub)));
  //       if(!strcmp(symb_chr, symb_chr_2)) {
  //         SET_STRING_ELT(res, i, asChar(CAR(obj1)));
  //         break;
  //       }
  //     }
  //   }
  // }
  // UNPROTECT(1);
  // return res;
  // duplicate(obj1);
  // return R_NilValue;
}
/*
deparse into character
*/
const char * ALIKEC_deparse(SEXP obj, R_xlen_t lines) {
  SEXP quot_call = PROTECT(list2(R_QuoteSymbol, obj));
  SET_TYPEOF(quot_call, LANGSXP);

  SEXP dep_call = PROTECT(list2(ALIKEC_SYM_deparse, quot_call));
  SET_TYPEOF(dep_call, LANGSXP);

  SEXP obj_dep = PROTECT(eval(dep_call, R_BaseEnv));
  //PrintValue(obj_dep);
  R_xlen_t line_max = XLENGTH(obj_dep), i;
  if(!line_max) return "";
  if(lines < 0) lines = line_max;
  char * res = "";

  for(i = 0; i < lines; i++) {
    res = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "%s%s%s%s", res, i ? "\n" : "",
      CHAR(STRING_ELT(obj_dep, i)),
      i == lines - 1 && lines < line_max ? "..." : ""
  );}
  UNPROTECT(3);
  return res;
}
SEXP ALIKEC_deparse_ext(SEXP obj, SEXP lines) {
  return mkString(ALIKEC_deparse(obj, asInteger(lines)));
}

/*
Simplified version of R's internal findFun

Doesn't do quick lookups for special symbols, or use the global cache if it is
available.

Most importantly, instead of failing if function is not found, returns
R_UnboundValue.

The code is copied almost verbatim from src/main/envir.c:findFun()
*/

SEXP ALIKEC_findFun(SEXP symbol, SEXP rho) {
  SEXP vl;
  while (rho != R_EmptyEnv) {
    vl = findVarInFrame3(rho, symbol, TRUE);
    if (vl != R_UnboundValue) {
      if (TYPEOF(vl) == PROMSXP) {
        PROTECT(vl);
        vl = eval(vl, rho);
        UNPROTECT(1);
      }
      if (
        TYPEOF(vl) == CLOSXP || TYPEOF(vl) == BUILTINSXP ||
        TYPEOF(vl) == SPECIALSXP
      )
        return (vl);
      if (vl == R_MissingArg) return R_UnboundValue;
    }
    rho = ENCLOS(rho);
  }
  return R_UnboundValue;
}

/*
Convert convention of zero length string == TRUE to SEXP
*/

SEXP ALIKEC_string_or_true(const char * var) {
  if(var[0]) return(mkString(var));
  return(ScalarLogical(1));
}
