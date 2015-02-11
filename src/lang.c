#include "alike.h"
#include "pfhash.h"

// Moves pointer on language object to skip any `(` calls since those are
// already accounted for in parsing and as such don't add anything


SEXP ALIKEC_skip_paren(SEXP lang) {
  if(TYPEOF(lang) != LANGSXP) return(lang);
  while(
    CAR(lang) == ALIKEC_SYM_paren_open && CDR(CDR(lang)) == R_NilValue
  ) lang = CADR(lang);
  return lang;
}

// - Anonymize Formula ---------------------------------------------------------

/* Look up symbol in hash table, if already present, return the anonymized
version of the symbol.  If not, add to the hash table.

symb the symbol to lookup
hash the hash table
varnum used to generate the anonymized variable name
*/

char * ALIKEC_symb_abstract(SEXP symb, pfHashTable * hash, size_t * varnum) {
  const char * symb_chr = CHAR(PRINTNAME(symb));
  char * symb_abs = pfHashFind(hash, (char *) symb_chr);  // really shouldn't have to do this, but can't be bothered re-defining the hash library
  if(symb_abs == NULL) {
    symb_abs = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "a%s", CSR_len_as_chr(*varnum), "", "", ""
    );
    pfHashSet(hash, (char *) symb_chr, symb_abs);
    (*varnum)++;
  }
  return symb_abs;
}
/*
Marks current symbol in langsxp
*/
void ALIKEC_symb_mark(SEXP obj) {
  SEXPTYPE obj_type = TYPEOF(obj);
  if(obj_type != LANGSXP && obj_type != LISTSXP) error("Unexpected argument");

  const char * car_dep = ALIKEC_deparse(CAR(obj), 1);
  SETCAR(
    obj,
    install(CSR_smprintf4(ALIKEC_MAX_CHAR, "{%s}", car_dep, "", "", ""))
  );
}
/*
Try to find function in env and return function if it exists, R_NilValue
otherwise

@param call the function we will ultimately match
@param env an environment to start the lookup
*/
SEXP ALIKEC_get_fun(SEXP call, SEXP env) {
  SEXP fun = CAR(call);
  switch(TYPEOF(fun)) {
    case CLOSXP:
      return fun;
      break;
    case SYMSXP:
      {
        SEXP fun_def = ALIKEC_findFun(fun, env);  // Assuming no GC happens in next couple of steps
        if(TYPEOF(fun_def) == CLOSXP) return fun_def;
      }
      break;
  }
  return R_NilValue;
}
/*
@param match_call a preconstructed call to retrieve the function; needed because
  can't figure out a way to create preconstructed call in init without
  sub-components getting GCed
*/
SEXP ALIKEC_match_call(
  SEXP call, SEXP match_call, SEXP env
) {
  SEXP fun = PROTECT(ALIKEC_get_fun(call, env));
  if(fun == R_NilValue) {
    UNPROTECT(1);
    return call;
  }
  SETCADR(match_call, fun);  // remember, match_call is pre-defined as: match.call(def, quote(call))
  UNPROTECT(1);
  SETCADR(CADDR(match_call), call);
  int tmp = 0;
  int * err =& tmp;
  SEXP res = R_tryEvalSilent(match_call, env, err);
  if(* err) return call; else return res;
}
/*
Creates a copy of the call mapping objects to a deterministic set of names
based on the order in which they appear in the call

Here we use a hash table to identify whether a symbol already showed up or not.
This is probably faster if langauge object has 25 or more elements, so may
eventually want to add logic that choses path based on how many elements.

If return value is zero length string then comparison succeeded, otherwise
return value is error message.  Note that we also return information by modifying
the `cur_par` argument by reference.  We either mark the token the error message
refers to by wrapping it in ``{}``, or blow it away to indicate we don't want
the final error message to try to point out where the error occurred (this is
typically the case when the error is not specific to a particular part of the
call).
*/

const char * ALIKEC_lang_alike_rec(
  SEXP target, SEXP cur_par, pfHashTable * tar_hash, pfHashTable * cur_hash,
  pfHashTable * rev_hash, size_t * tar_varnum, size_t * cur_varnum, int formula,
  SEXP match_call, SEXP match_env
) {
  SEXP current = CAR(cur_par);
  SEXP tar_fun = CAR(target), cur_fun = CAR(current);
  if(tar_fun != cur_fun) {  // Actual fun call must match exactly
    char * res = CSR_smprintf4(
      ALIKEC_MAX_CHAR,
      "be a call to `%s` (is a call to `%s`)", ALIKEC_deparse(CAR(target), 1),
      ALIKEC_deparse(CAR(current), 1), "", ""
    );
    ALIKEC_symb_mark(current);
    return (const char *) res;
  }
  // Match the calls before comparison; small inefficiency below since we know
  // that target and current must be the same fun; we shouldn't need to retrieve
  // it twice as we do now

  if(match_env != R_NilValue) {
    target = ALIKEC_match_call(target, match_call, match_env);
    SETCAR(cur_par, ALIKEC_match_call(current, match_call, match_env));  // want this change to persist back to calling fun
    current = CAR(cur_par);
  }
  SEXP tar_sub, cur_sub, cur_sub_tag, prev_tag = R_NilValue;
  for(
    tar_sub = CDR(target), cur_sub = CDR(current);
    tar_sub != R_NilValue && cur_sub != R_NilValue;
    tar_sub = CDR(tar_sub), cur_sub = CDR(cur_sub), prev_tag = cur_sub_tag
  ) {
    if(TAG(tar_sub) != (cur_sub_tag = TAG(cur_sub))) {
      char * prev_tag_msg = "as first argument";
      if(prev_tag != R_NilValue) {
        prev_tag_msg = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "after argument `%s`", CHAR(PRINTNAME(prev_tag)),
          "", "", ""
      );}
      ALIKEC_symb_mark(cur_par);
      return CSR_smprintf4(
        ALIKEC_MAX_CHAR, "have argument `%s` %s", CHAR(PRINTNAME(TAG(tar_sub))),
        prev_tag_msg, "", ""
    );}
    tar_sub = ALIKEC_skip_paren(tar_sub);
    cur_sub = ALIKEC_skip_paren(cur_sub);

    SEXP tar_sub_car = CAR(tar_sub), cur_sub_car = CAR(cur_sub);
    SEXPTYPE tsc_type = TYPEOF(tar_sub_car), csc_type = TYPEOF(cur_sub_car);

    // PrintValue(tar_sub_car);
    // Rprintf("type tsc: %s csc: %s\n", type2char(tsc_type), type2char(csc_type));

    if(tsc_type == SYMSXP && csc_type == SYMSXP) {
      char * tar_abs = ALIKEC_symb_abstract(tar_sub_car, tar_hash, tar_varnum);
      char * cur_abs = ALIKEC_symb_abstract(cur_sub_car, cur_hash, cur_varnum);
      char * rev_symb = pfHashFind(rev_hash, tar_abs);  // reverse hash to get what symbol should be in case of error
      const char * csc_text = CHAR(PRINTNAME(cur_sub_car));
      if(rev_symb == NULL) {
        rev_symb = (char *) csc_text;
        pfHashSet(rev_hash, cur_abs, rev_symb);
      }
      if(strcmp(tar_abs, cur_abs)) {
        ALIKEC_symb_mark(cur_sub);
        if(*tar_varnum > *cur_varnum) {
          return CSR_smprintf4(
            ALIKEC_MAX_CHAR, "have a symbol different to `%s`", csc_text, "",
            "", ""
          );
        }
        return CSR_smprintf4(
          ALIKEC_MAX_CHAR, "have symbol `%s` (is `%s`)",
          rev_symb, csc_text, "", ""
        );
      };
    } else if (tsc_type == LANGSXP && csc_type != LANGSXP) {
      ALIKEC_symb_mark(cur_sub);
      return CSR_smprintf4(
        ALIKEC_MAX_CHAR, "be \"language\" (is \"%s\") for `%s`",
        type2char(csc_type), ALIKEC_deparse(cur_sub_car, 1), "", ""
      );
    } else if (tsc_type == LANGSXP) {
      const char * res;
      // Note how we pass cur_sub and not cur_sub_car so we can modify cur_sub
      res = ALIKEC_lang_alike_rec(
        tar_sub_car, cur_sub, tar_hash, cur_hash, rev_hash, tar_varnum,
        cur_varnum, formula, match_call, match_env
      );
      if(CAR(cur_sub) == R_NilValue) SETCAR(cur_par, R_NilValue); //Nunking call since we don't want it in this case
      if(res[0]) {
        return res;
      }
    } else if(tsc_type == SYMSXP || csc_type == SYMSXP) {
      ALIKEC_symb_mark(cur_sub);

      return (const char *) CSR_smprintf4(
        ALIKEC_MAX_CHAR,
        "be \"%s\" (is \"%s\") for token `%s`",
        type2char(tsc_type), type2char(csc_type),
        ALIKEC_deparse(cur_sub_car, 1), ""
      );
    } else if (formula && !R_compute_identical(tar_sub_car, cur_sub_car, 16)) {
      // Maybe this shouldn't be "identical", but too much of a pain in the butt
      // to do an all.equals type comparison

      ALIKEC_symb_mark(cur_sub);
      return "have identical constant values";  // could have constant vs. language here, right?
    }
  }
  if(tar_sub != R_NilValue || cur_sub != R_NilValue) {
    SETCAR(cur_par, R_NilValue);  // Unorthodox way of signaling that we don't wan to show function call
    return (const char *) CSR_smprintf4(
      ALIKEC_MAX_CHAR, "be the same length (is %s)",
      tar_sub == R_NilValue ? "longer" : "shorter", "", "", ""
  );}
  return "";
}
/*
Determine whether objects should be compared as calls or as formulas; the main
difference in treatment is that calls are match-called if possible, and also
that for calls constants need not be the same
*/

const char * ALIKEC_lang_alike_internal(
  SEXP target, SEXP current, SEXP match_env
) {
  if(TYPEOF(target) != LANGSXP || TYPEOF(current) != LANGSXP)
    error("Arguments must be LANGSXP");
  if(TYPEOF(match_env) != ENVSXP && match_env != R_NilValue)
    error("Argument `match.call.env` must be an environment or NULL");

  // Create persistent objects for use throught recursion

  pfHashTable * tar_hash = pfHashCreate(NULL);
  pfHashTable * cur_hash = pfHashCreate(NULL);
  pfHashTable * rev_hash = pfHashCreate(NULL);
  size_t tartmp = 0, curtmp=0;
  size_t * tar_varnum = &tartmp;
  size_t * cur_varnum = &curtmp;

  SEXP match_call = PROTECT(  // Can't figure out how to do this on init; cost ~60ns
    list3(
      ALIKEC_SYM_matchcall, R_NilValue,
      list2(R_QuoteSymbol, R_NilValue)
  ) );
  SET_TYPEOF(match_call, LANGSXP);
  SET_TYPEOF(CADDR(match_call), LANGSXP);

  int formula = 0;

  // Determine if it is a formular or not

  SEXP class = getAttrib(target, R_ClassSymbol);
  if(
    class != R_NilValue && TYPEOF(class) == STRSXP &&
    !strcmp("formula", CHAR(asChar(STRING_ELT(class, XLENGTH(class) - 1)))) &&
    CAR(target) == ALIKEC_SYM_tilde
  ) {
    formula = 1;
  }
  // Check if alike

  SEXP curr_cpy_par = PROTECT(list1(duplicate(current)));
  const char * res = ALIKEC_lang_alike_rec(
    target, curr_cpy_par, tar_hash, cur_hash, rev_hash, tar_varnum, cur_varnum,
    formula, match_call, match_env
  );
  // Construct error message

  const char * err_msg = "";
  if(res[0]) {
    // Find display width

    SEXP width_call = PROTECT(list2(ALIKEC_SYM_getOption, mkString("width")));
    SET_TYPEOF(width_call, LANGSXP);
    SEXP disp_width = PROTECT(eval(width_call, R_BaseEnv));
    int max_chars = 200;
    if(TYPEOF(disp_width) == INTSXP && XLENGTH(disp_width) == 1) {
      int width_opt = asInteger(disp_width);
      if(width_opt != NA_INTEGER && width_opt > 0) max_chars = width_opt;
    }
    UNPROTECT(2);
    /*
    Based on width, determine how to display result, with these rules:
    - if deparse contains newline, then start on new line
    - if deparse plus rest of err message greater than max_char, then start on
      new line
    */
    int use_in = CAR(curr_cpy_par) != R_NilValue;
    if(use_in) {
      int  i, has_nl = 0;
      const char * err_dep =ALIKEC_deparse(CAR(curr_cpy_par), -1);
      for(i = 0; i < max_chars; i++) { // calc dep length
        if(!err_dep[i]) break;
        if(err_dep[i] == '\n') {
          has_nl = 1;
          break;
        }
      }
      int with_nl = has_nl || (strlen(res) + 4 + i + 1 > max_chars);
      err_msg = CSR_smprintf4(
        ALIKEC_MAX_CHAR, "%s in:%s%s", res, with_nl ? "\n" : " ", err_dep, ""
      );
    } else {
      err_msg = res;
    }
  }
  UNPROTECT(2);
  return err_msg;
}

SEXP ALIKEC_lang_alike_ext(
  SEXP target, SEXP current, SEXP match_env
) {
  const char * res = ALIKEC_lang_alike_internal(target, current, match_env);
  if(strlen(res)) return mkString(res);
  return ScalarLogical(1);
}
