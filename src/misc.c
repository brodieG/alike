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
SEXP ALIKEC_test(SEXP obj) {
  return R_NilValue;
}
SEXP ALIKEC_test2(
    SEXP target, SEXP current
) {
  return R_NilValue;
}
/*
Run deparse command and return character vector with results

set width_cutoff to be less than zero to use default
*/
SEXP ALIKEC_deparse_core(SEXP obj, int width_cutoff) {
  SEXP quot_call = PROTECT(list2(R_QuoteSymbol, obj)), dep_call;
  SET_TYPEOF(quot_call, LANGSXP);

  if(width_cutoff < 0){
    dep_call = PROTECT(list2(ALIKEC_SYM_deparse, quot_call));
  } else {
    dep_call = PROTECT(
      list3(ALIKEC_SYM_deparse, quot_call, ScalarInteger(width_cutoff))
    );
    SET_TAG(CDDR(dep_call), ALIKEC_SYM_widthcutoff);
  }
  SET_TYPEOF(dep_call, LANGSXP);

  UNPROTECT(2);
  return eval(dep_call, R_BaseEnv);
}
/*
Do a one line deparse
*/
const char * ALIKEC_deparse_oneline(SEXP obj, size_t max_chars) {
  if(max_chars < 8)
    error("Logic Error: argument `max_chars` must be >= 8");
  const char * res, * dep_line = CHAR(asChar(ALIKEC_deparse_core(obj, 500)));
  size_t dep_len = CSR_strmlen(dep_line, ALIKEC_MAX_CHAR);
  if(dep_len > max_chars) {
    // truncate string and use '..' at the end

    char * res_tmp = R_alloc(dep_len + 1, sizeof(char));
    size_t i;
    for(i = 0; i < max_chars - 2; i++) res_tmp[i] = dep_line[i];
    res_tmp[i] = res_tmp[i + 1] = '.';
    res_tmp[i + 2] = '\0';
    res = (const char *) res_tmp;
  } else res = dep_line;

  return res;
}
SEXP ALIKEC_deparse_oneline_ext(SEXP obj, SEXP max_chars) {
  int char_int = asInteger(max_chars);
  if(char_int < 0) error("Logic Error: arg max_chars must be positive");
  return mkString(ALIKEC_deparse_oneline(obj, (size_t) char_int));
}


/*
deparse into character

@param max_chars determines how many characters before we try to turn into
  multi-line deparse; set to -1 to ignore
@param lines how many lines to show, if it deparses to more than lines append
  `...` a end; set to -1 to ignore
*/
const char * ALIKEC_deparse(SEXP obj, R_xlen_t lines, int max_chars) {
  SEXP quot_call = PROTECT(list2(R_QuoteSymbol, obj));
  SET_TYPEOF(quot_call, LANGSXP);

  SEXP dep_call = PROTECT(list2(ALIKEC_SYM_deparse, quot_call));
  SET_TYPEOF(dep_call, LANGSXP);

  SEXP obj_dep = PROTECT(eval(dep_call, R_BaseEnv));
  //PrintValue(obj_dep);
  R_xlen_t line_max = XLENGTH(obj_dep), i;
  if(!line_max) return "";
  if(lines < 0) lines = line_max;
  char * res = "", * dep_pad = "";
  int nl = 0;

  for(i = 0; i < lines; i++) {
    const char * dep_err = CHAR(STRING_ELT(obj_dep, i));
    if(!i) {
      nl = lines > 1 || (max_chars > 0 && strlen(dep_err) > max_chars);
      if(nl) dep_pad = "> ";
    } else if(nl) dep_pad ="+ ";
    res = CSR_smprintf6(
      ALIKEC_MAX_CHAR, "%s%s%s%s%s", res, dep_pad, dep_err,
      i == lines - 1 && lines < line_max ? "..." : "",
      nl && i < lines - 1 ? "\n" : "", ""
  );}
  UNPROTECT(3);
  return res;
}

SEXP ALIKEC_deparse_ext(SEXP obj, SEXP lines, SEXP chars) {
  return mkString(ALIKEC_deparse(obj, asInteger(lines), asInteger(chars)));
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
