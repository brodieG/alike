#include "alike.h"
#include "pfhash.h"
#include <time.h>

// - Helper Functions ----------------------------------------------------------
/*
print current PROTECT stack height; used for debugging
*/
void psh(const char * lab) {
  PROTECT_INDEX i;
  PROTECT_WITH_INDEX(R_NilValue, &i);
  UNPROTECT(1);
  Rprintf("Protect Stack %s: %d\n", lab, i);
}
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
/*
run `getOption` from C
*/
SEXP ALIKEC_getopt(const char * opt) {
  SEXP opt_call = PROTECT(list2(ALIKEC_SYM_getOption, mkString(opt)));
  SET_TYPEOF(opt_call, LANGSXP);
  SEXP opt_val = PROTECT(eval(opt_call, R_BaseEnv));
  UNPROTECT(2);
  return opt_val;
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
  return mkString(CHAR(asChar(obj)));
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
Do a one line deparse, optionally replacing characters in excess of `max_chars`
by `..` to keep deparse short; `keep_at_end` indicates how many characters to
keep at end of deparsed when shortening (e.g. `i_m_deparsed(xyz..)` is keeping
the last parenthesis
*/
const char * ALIKEC_deparse_oneline(
    SEXP obj, size_t max_chars, size_t keep_at_end
) {
  if(max_chars < 8)
    error("Logic Error: argument `max_chars` must be >= 8");
  if(keep_at_end > max_chars - 2)
    error("Logic Error: arg `keep_at_end` too large");

  const char * res, * dep_line = CHAR(asChar(ALIKEC_deparse_core(obj, 500)));
  size_t dep_len = CSR_strmlen(dep_line, ALIKEC_MAX_CHAR);
  if(dep_len > max_chars) {
    // truncate string and use '..' at the end

    char * res_tmp = R_alloc(dep_len + 1, sizeof(char));
    size_t i, j;
    for(i = 0; i < max_chars - keep_at_end - 2; i++) res_tmp[i] = dep_line[i];
    res_tmp[i] = res_tmp[i + 1] = '.';
    i += 2;
    for(j = dep_len - keep_at_end; j < dep_len && i < dep_len; j++, i++) {
      res_tmp[i] = dep_line[j];
    }
    res_tmp[i] = '\0';
    res = (const char *) res_tmp;
  } else res = dep_line;

  return res;
}
SEXP ALIKEC_deparse_oneline_ext(SEXP obj, SEXP max_chars, SEXP keep_at_end) {
  int char_int = asInteger(max_chars);
  int keep_int = asInteger(keep_at_end);
  if(char_int < 0 || keep_int < 0)
    error("Logic Error: arg max_chars and keep_at_end must be positive");
  return mkString(
    ALIKEC_deparse_oneline(obj, (size_t) char_int, (size_t) keep_int)
  );
}
SEXP ALIKEC_deparse(SEXP obj, int width_cutoff) {
  return ALIKEC_deparse_core(obj, width_cutoff);
}
/*
version that uses default deparse width if console is wide enought, otherwise
based on console width
*/
SEXP ALIKEC_deparse_width(SEXP obj, int width) {
  if(width < 0) width = asInteger(ALIKEC_getopt("width"));
  if(width < 10 || width > 1000) width = 80;

  int dep_cutoff;

  if(width < 62) dep_cutoff = width - 2;
  else dep_cutoff = 60;
  if(dep_cutoff < 20) dep_cutoff = 60;
  return ALIKEC_deparse(obj, dep_cutoff);
}
SEXP ALIKEC_deparse_ext(SEXP obj, SEXP width_cutoff) {
  return ALIKEC_deparse(obj, asInteger(width_cutoff));
}
/*
Pad a character vector

@param obj character vector to pad
@param pad how to pad the character vector
  - -1, use the R prompt and continue symbols
  - 0-n pad with that many spaces
@param lines how many lines to show, append `...` a end; set to -1 to ignore
*/
const char * ALIKEC_pad(SEXP obj, R_xlen_t lines, int pad) {
  if(TYPEOF(obj) != STRSXP)
    error("Logic Error: argument `obj` should be STRSXP");
  R_xlen_t line_max = XLENGTH(obj), i;
  if(!line_max) return "";
  for(i = 0; i < line_max; i++)
    if(STRING_ELT(obj, i) == NA_STRING)
      error("Logic Error: argument `obj` contains NAs");

  if(lines < 0) lines = line_max;

  char * res = "";
  const char * dep_prompt = "", * dep_continue = "";

  // Figure out what to use as prompt and continue

  if(pad < 0) {
    SEXP prompt_val = PROTECT(ALIKEC_getopt("prompt"));
    SEXP prompt_continue = PROTECT(ALIKEC_getopt("continue"));

    if(
      TYPEOF(prompt_val) != STRSXP || TYPEOF(prompt_continue) != STRSXP ||
      asChar(prompt_val) != NA_STRING || asChar(prompt_continue) != NA_STRING
    ) {
      dep_prompt = "> ";
      dep_continue = "+ ";
    } else {
      dep_prompt = CHAR(asChar(prompt_val));
      dep_continue = CHAR(asChar(prompt_continue));
    }
    UNPROTECT(2);
  } else if (pad > 0) {
    char * pad_chr = R_alloc(pad + 1, sizeof(char));
    int i;
    for(i = 0; i < pad; i++) pad_chr[i] = ' ';
    pad_chr[i] = '\0';
    dep_prompt = dep_continue = (const char *) pad_chr;
  }
  // Cycle through lines

  for(i = 0; i < lines; i++) {
    const char * dep_pad = "";
    const char * dep_err = CHAR(STRING_ELT(obj, i));
    if(!i) dep_pad = dep_prompt; else dep_pad = dep_continue;
    res = CSR_smprintf6(
      ALIKEC_MAX_CHAR, "%s%s%s%s%s", res, dep_pad, dep_err,
      i == lines - 1 && lines < line_max ? "..." : "",
      lines > 1 && line_max > 1 ? "\n" : "", ""
  );}
  return res;
}
SEXP ALIKEC_pad_ext(SEXP obj, SEXP lines, SEXP pad) {
  return mkString(ALIKEC_pad(obj, asInteger(lines), asInteger(pad)));
}
/*
deparse into character

@param width_cutoff to use as `width.cutoff` param to `deparse`
@param lines to use as `lines` arg to ALIKEC_pad
*/
const char * ALIKEC_deparse_chr(SEXP obj, int width_cutoff) {
  return ALIKEC_pad(ALIKEC_deparse_core(obj, width_cutoff), -1, 0);
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

SEXP ALIKEC_string_or_true(struct ALIKEC_res_fin res) {
  if(res.message[0]) {
    const char * res_str = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "%sshould %s", res.call, res.message, "", ""
    );
    return(mkString(res_str));
  }
  return(ScalarLogical(1));
}
/*
Basic checks that `obj` could be a data frame; does not check class, only that
object is list and that contents are all same length
unfortunately, may not be actually used due to how compare_class is structured
tbd
*/
int ALIKEC_is_dfish(SEXP obj) {
  int res = 1;
  R_xlen_t vec_len, col_num, col_count;
  if(TYPEOF(obj) == VECSXP) {
    col_count = XLENGTH(obj);
    if(col_count) {
      vec_len = XLENGTH(VECTOR_ELT(obj, 0));
      for(col_num = 1; col_num < col_count; col_num++) {
        if(XLENGTH(VECTOR_ELT(obj, col_num)) != vec_len) {
          res = 0;
          break;
    } } }
  } else res = 0;
  return res;
}
SEXP ALIKEC_is_dfish_ext(SEXP obj) {
  return ScalarLogical(ALIKEC_is_dfish(obj));
}

