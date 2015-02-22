#include "alike.h"
#include "pfhash.h"
#include <time.h>

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
SEXP ALIKEC_abstract_ts(SEXP x, SEXP attr) {
  if(TYPEOF(attr) != REALSXP || XLENGTH(attr) != 3)
    error("Logic Error: incorrect format for tsp attr, contact maintainer");
  SEXP x_cp = PROTECT(duplicate(x));

  // Get to last attribute, and make sure tsp is not set

  SEXP attrs = ATTRIB(x_cp), attrs_cpy, attrs_last;
  for(attrs_cpy = attrs; attrs_cpy != R_NilValue; attrs_cpy = CDR(attrs_cpy)) {
    attrs_last = attrs_cpy;
    if(TAG(attrs_cpy) == R_TspSymbol) break;
  }
  if(attrs_cpy != R_NilValue)
    error("Logic Error: object already has a `tsp` attribute");

  // Illegally append non-kosher tsp attribute

  SETCDR(attrs_last, list1(attr));
  SET_TAG(CDR(attrs_last), R_TspSymbol);
  UNPROTECT(1);
  return x_cp;
}
// - Testing Function ----------------------------------------------------------
SEXP ALIKEC_test(SEXP target, SEXP current, SEXP settings) {
  R_xlen_t obj_len = XLENGTH(current), i, items=0;
  int finite;
  double tolerance = sqrt(DOUBLE_EPS), * obj_real, val, diff_abs, flr;
  obj_real = REAL(current);

  switch(asInteger(target)) {
    case 0: {
      for(i = 0; i < obj_len; i++) {
        if(
          !isnan(obj_real[i]) && (finite = isfinite(obj_real[i])) &&
          obj_real[i] != (flr = floor(obj_real[i]))
        ) {
          items = items + 1;
          diff_abs = diff_abs + fabs((obj_real[i] - flr) / obj_real[i]);
          val = val + fabs(obj_real[i]);
        } else if (!finite) return ScalarLogical(1);
      }
      if(items > 0 && val / items > tolerance && diff_abs / items > tolerance) {
        return ScalarLogical(0);
      } else {
        return ScalarLogical(1);
      }
    }
    break;
    case 1: {
      for(i = 0; i < obj_len; i++) {
        if(
          !isnan(obj_real[i]) && (finite = isfinite(obj_real[i])) &&
          obj_real[i] != floor(obj_real[i])
        ) {
          return ScalarLogical(0);
        } else if (!finite) return ScalarLogical(0);
      }
      return ScalarLogical(1);
    }
    case 2: {
      for(i = 0; i < obj_len; i++) {
        if(
          !isnan(obj_real[i]) && (finite = isfinite(obj_real[i])) &&
          obj_real[i] != nearbyintl(obj_real[i])
        ) {
          return ScalarLogical(0);
        } else if (!finite) return ScalarLogical(0);
      }
      return ScalarLogical(1);
    }
    case 3: {
      for(i = 0; i < obj_len; i++) {
        if(
          !isnan(obj_real[i]) && (finite = isfinite(obj_real[i])) &&
          obj_real[i] != trunc(obj_real[i])
        ) {
          return ScalarLogical(0);
        } else if (!finite) return ScalarLogical(0);
      }
      return ScalarLogical(1);
    }
    case 4: {
      for(i = 0; i < obj_len; i++) {
        if(
          obj_real[i] != trunc(obj_real[i])
        ) {
          return ScalarLogical(0);
        }
      }
      return ScalarLogical(1);
    }
    case 5: {
      for(i = 0; i < obj_len; i++) {
        if(
          obj_real[i] != (int) obj_real[i]
        ) {
          return ScalarLogical(0);
        }
      }
      return ScalarLogical(1);
    }
  }

  return R_NilValue;
}
SEXP ALIKEC_test2(
    SEXP target, SEXP current, SEXP type_mode, SEXP int_tol, SEXP attr_mode,
    SEXP suppress_warnings, SEXP match_env
) {
  return R_NilValue;
};


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
