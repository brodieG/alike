#include "alike.h"

/*
Initialize return object
*/
struct ALIKEC_res_lang ALIKEC_rec_lang_init() {
  return (struct ALIKEC_res_lang) {
    .success = 0,
    .rec = ALIKEC_rec_init(),
    .message = ""
  }
}
/*
Moves pointer on language object to skip any `(` calls since those are
already accounted for in parsing and as such don't add anything.

Returns a list/vector with a pointer to the updated lanaguage object position
and an integer indicating how many parentheses were skipped
*/

SEXP ALIKEC_skip_paren(SEXP lang) {
  int i = 0;
  SEXP res = PROTECT(allocVector(VECSXP, 2));
  if(TYPEOF(lang) == LANGSXP) {
    while(
      CAR(lang) == ALIKEC_SYM_paren_open && CDR(CDR(lang)) == R_NilValue
    ) {
      lang = CADR(lang);
      i++;
      if(i < 0) error(
        "Logic Error: %s; contact maintainer.",
        "exceeded language recursion depth when skipping parens"
      );
  } }
  SET_VECTOR_ELT(res, 0, lang);
  SET_VECTOR_ELT(res, 1, ScalarInteger(i);
  UNPROTECT(1);
  return(res);
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
  if(obj != R_NilValue) {
    SEXPTYPE obj_type = TYPEOF(obj);
    if(obj_type != LANGSXP && obj_type != LISTSXP) error("Unexpected argument");

    const char * car_dep = ALIKEC_deparse_chr(CAR(obj), -1);
    SETCAR(
      obj,
      install(CSR_smprintf4(ALIKEC_MAX_CHAR, "{%s}", car_dep, "", "", ""))
    );
  }
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
  // remember, match_call is pre-defined as: match.call(def, quote(call))
  SETCADR(match_call, fun);
  UNPROTECT(1);
  SETCADR(CADDR(match_call), call);
  int tmp = 0;
  int * err =& tmp;
  SEXP res = R_tryEvalSilent(match_call, env, err);
  if(* err) return call; else return res;
}
/*
Handle language object comparison

Note that we always pass cur_par instead of current so that we can modify the
original call (mostly by using `match.call` on it)
*/
const char * ALIKEC_lang_obj_compare(
  SEXP target, SEXP cur_par, pfHashTable * tar_hash,
  pfHashTable * cur_hash, pfHashTable * rev_hash, size_t * tar_varnum,
  size_t * cur_varnum, int formula, SEXP match_call, SEXP match_env,
  struct ALIKEC_settings set, struct ALIKEC_rec_track rec
) {
  SEXP current = CAR(cur_par);
  struct ALIKEC_res_lang res = ALIKEC_res_lang_init();
  res.rec = rec;

  SEXP cur_skip_paren = PROTECT(ALIKEC_skip_paren(current));
  SEXP tar_skip_paren = PROTECT(ALIKEC_skip_paren(target));

  // Skip parens and increment recursion; not we don't track recursion level
  // for target

  current = VECTOR_ELT(cur_skip_paren, 0);
  int i, i_max = asInteger(VECTOR_ELT(cur_skip_paren, 1));
  for(i = 0; i < i_max; i++) res.rec = ALIKEC_rec_inc(res.rec);

  if(target == R_NilValue) return ""; // NULL matches anything
  SEXPTYPE tsc_type = TYPEOF(target), csc_type = TYPEOF(current);

  res.success = 0;  // assume fail until shown otherwise

  if(tsc_type == SYMSXP && csc_type == SYMSXP) {
    char * tar_abs = ALIKEC_symb_abstract(target, tar_hash, tar_varnum);
    char * cur_abs = ALIKEC_symb_abstract(current, cur_hash, cur_varnum);
    // reverse hash to get what symbol should be in case of error
    char * rev_symb = pfHashFind(rev_hash, tar_abs);
    const char * csc_text = CHAR(PRINTNAME(current));
    if(rev_symb == NULL) {
      rev_symb = (char *) csc_text;
      pfHashSet(rev_hash, cur_abs, rev_symb);
    }
    if(strcmp(tar_abs, cur_abs)) {
      if(*tar_varnum > *cur_varnum) {
        res.message = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "not be `%s`", csc_text, "",
          "", ""
        );
      } else {
        res.message = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "be `%s` (is `%s`)",
          rev_symb, csc_text, "", ""
        );
      }
    };
  } else if (tsc_type == LANGSXP && csc_type != LANGSXP) {
    res.message =  CSR_smprintf4(
      ALIKEC_MAX_CHAR, "be a call to `%s` (is \"%s\")",
      ALIKEC_deparse_chr(CAR(target), -1), type2char(csc_type),
      "", ""
    );
  } else if (tsc_type != LANGSXP && csc_type == LANGSXP) {
    res.message = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "be \"%s\" (is \"%s\")",
      type2char(tsc_type), type2char(csc_type), "", ""
    );
  } else if (tsc_type == LANGSXP) {
    // Note how we pass cur_par and not current so we can modify cur_par
    // this should be changed since we don't use that feature any more
    res = ALIKEC_lang_alike_rec(
      target, cur_par, tar_hash, cur_hash, rev_hash, tar_varnum,
      cur_varnum, formula, match_call, match_env, set, rec
    );
  } else if(tsc_type == SYMSXP || csc_type == SYMSXP) {
    res.message = CSR_smprintf4(
      ALIKEC_MAX_CHAR,
      "be \"%s\" (is \"%s\")", type2char(tsc_type), type2char(csc_type), "", ""
    );
  } else if (formula && !R_compute_identical(target, current, 16)) {
    // Maybe this shouldn't be "identical", but too much of a pain in the butt
    // to do an all.equals type comparison

    // could have constant vs. language here, right?

    res.message =  "have identical constant values";
  } else res.success = 1;

  // Deal with index implications of skiping parens

  if(!res.success) {
    for(i = 0; i < i_max; i++) {
      res.rec = ALIKEC_rec_ind_num(res.rec, i + 1);
      res.rec = ALIKEC_rec_dec(res.rec);
    }
  }
  return res;
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

struct ALIKEC_rec_lang ALIKEC_lang_alike_rec(
  SEXP target, SEXP cur_par, pfHashTable * tar_hash, pfHashTable * cur_hash,
  pfHashTable * rev_hash, size_t * tar_varnum, size_t * cur_varnum, int formula,
  SEXP match_call, SEXP match_env, struct ALIKEC_settings set,
  struct ALIKEC_rec_track rec
) {
  SEXP current = CAR(cur_par);

  target = VECTOR_ELT(tar_skip_paren, 0);

  // If not language object, run comparison

  struct ALIKEC_res_lang res = ALIKEC_res_lang_init();
  res.rec = rec;

  if(TYPEOF(target) != LANGSXP || TYPEOF(current) != LANGSXP) {
    res =  ALIKEC_lang_obj_compare(
      target, cur_par, tar_hash, cur_hash, rev_hash, tar_varnum,
      cur_varnum, formula, match_call, match_env, set, rec.rec
    );
  } else {
    // If language object, then recurse

    res.rec = ALIKEC_rec_inc(res.rec);

    SEXP tar_fun = CAR(target), cur_fun = CAR(current);

    // Actual fun call must match exactly, unless NULL

    if(tar_fun != R_NilValue && tar_fun != cur_fun) {
      res.success = 0;
      res.rec = ALIKEC_rec_ind_num(res.rec, 1);
      res.message = CSR_smprintf4(
        ALIKEC_MAX_CHAR,
        "be a call to `%s` (is a call to `%s`)",
        ALIKEC_deparse_chr(CAR(target), -1),
        ALIKEC_deparse_chr(CAR(current), -1), "", ""
      );
    } else if (CDR(target) != R_NilValue) {
      // Zero length calls match anything, so only come here if target is not
      // Nil

      // Match the calls before comparison; small inefficiency below since we
      // know that target and current must be the same fun; we shouldn't need
      // to retrieve it twice as we do now

      if(match_env != R_NilValue && set.lang_mode != 1) {
        target = PROTECT(ALIKEC_match_call(target, match_call, match_env));
        current = PROTECT(ALIKEC_match_call(current, match_call, match_env));
        SETCAR(cur_par, current);  // ensures original call is matched
      } else {
        PROTECT(PROTECT(R_NilValue)); // stack balance
      }
      SEXP tar_sub, cur_sub, cur_sub_tag, tar_sub_tag, prev_tag = R_UnboundValue;
      R_xlen_t arg_num, arg_num_prev = 0;

      for(
        tar_sub = CDR(target), cur_sub = CDR(current), arg_num = 0;
        tar_sub != R_NilValue && cur_sub != R_NilValue;
        tar_sub = CDR(tar_sub), cur_sub = CDR(cur_sub), prev_tag = cur_sub_tag,
        arg_num++
      ) {
        if(arg_num_prev > arg_num)
          error(
            "Logic Error: %s; contact maintainer.",
            "exceeded allowable call length"
          );
        // Check tags are compatible; NULL tag in target allows any tag in current

        cur_sub_tag = TAG(cur_sub);
        tar_sub_tag = TAG(tar_sub);

        if(tar_sub_tag != R_NilValue && tar_sub_tag != cur_sub_tag) {
          char * prev_tag_msg = "as first argument";
          if(prev_tag != R_UnboundValue) {
            if(prev_tag == R_NilValue) {
              prev_tag_msg = CSR_smprintf4(
                ALIKEC_MAX_CHAR, "after argument %s", CSR_len_as_chr(arg_num),
                "", "", ""
              );
            } else {
              prev_tag_msg = CSR_smprintf4(
                ALIKEC_MAX_CHAR, "after argument `%s`", CHAR(PRINTNAME(prev_tag)),
                "", "", ""
          );} }
          res.success = 0;
          res.message = CSR_smprintf4(
            ALIKEC_MAX_CHAR, "have argument `%s` %s",
            CHAR(PRINTNAME(TAG(tar_sub))),
            prev_tag_msg, "", ""
        );}
        SEXP tar_sub_car = CAR(tar_sub), cur_sub_car = CAR(cur_sub);

        // Note that `lang_obj_compare` kicks off recursion as well, and
        // skips parens

        struct ALIKEC_res_lang res = ALIKEC_lang_obj_compare(
          tar_sub_car, cur_sub, tar_hash, cur_hash, rev_hash,
          tar_varnum, cur_varnum, formula, match_call, match_env, set
        );
        // Update recursion indices

        if(!res.success) {
          if(tar_sub_tag != R_NilValue)
            res.rec =
              ALIKEC_rec_ind_chr(res.rec, CHAR(PRINTNAME(tar_sub_tag)));
          else
            res.rec =
              ALIKEC_rec_ind_num(res.rec, arg_num + 1);
        }
        arg_num_prev = arg_num;
      }
      if(res.success) {
        // Make sure that we compared all items; missing R_NilValue here means
        // one of the calls has more items

        R_xlen_t tar_len, cur_len;
        tar_len = cur_len = arg_num;
        if(tar_sub != R_NilValue || cur_sub != R_NilValue) {
          while(tar_sub != R_NilValue) {
            tar_len++;
            tar_sub = CDR(tar_sub);
          }
          while(cur_sub != R_NilValue) {
            cur_len++;
            cur_sub = CDR(cur_sub);
          }
          res.success = 0;
          res.message = CSR_smprintf4(
            ALIKEC_MAX_CHAR, "be length %s (is %s)",
            CSR_len_as_chr(tar_len), CSR_len_as_chr(cur_len),  "", ""
        );}
      }
      UNPROTECT(2);
    }
    res.rec = ALIKEC_rec_dec(rec);
  }
  UNPROTECT(2);
  return res;
}
/*
Determine whether objects should be compared as calls or as formulas; the main
difference in treatment is that calls are match-called if possible, and also
that for calls constants need not be the same
*/

const char * ALIKEC_lang_alike_internal(
  SEXP target, SEXP current, struct ALIKEC_settings set
) {
  SEXP match_env = set.env;
  SEXPTYPE tar_type = TYPEOF(target), cur_type = TYPEOF(current);
  if(
    !
    (tar_type == LANGSXP || tar_type == SYMSXP || tar_type == NILSXP) &&
    (cur_type == LANGSXP || cur_type == SYMSXP || cur_type == NILSXP)
  )
    error("Arguments must be LANGSXP, SYMSXP, or R_NilValue");

  if(TYPEOF(match_env) != ENVSXP && match_env != R_NilValue)
    error("Argument `match.call.env` must be an environment or NULL");

  // Create persistent objects for use throught recursion

  pfHashTable * tar_hash = pfHashCreate(NULL);
  pfHashTable * cur_hash = pfHashCreate(NULL);
  pfHashTable * rev_hash = pfHashCreate(NULL);
  size_t tartmp = 0, curtmp=0;
  size_t * tar_varnum = &tartmp;
  size_t * cur_varnum = &curtmp;

  // Can't figure out how to do this on init; cost ~60ns
  SEXP match_call = PROTECT(
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
  // Check if alike; originally we would modify a copy of current, which is
  // why we send curr_cpy_par; in the future we will no longer do that

  SEXP curr_cpy_par = PROTECT(list1(duplicate(current)));
  struct ALIKEC_rec_track = ALIKEC_rec_init();
  const char * res = ALIKEC_lang_alike_rec(
    target, curr_cpy_par, tar_hash, cur_hash, rev_hash, tar_varnum, cur_varnum,
    formula, match_call, match_env, set, rec
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
      int max_chars_net =
        max_chars - (strlen(res) + strlen(set.prepend) + 4 + 1);
      int err_dep_len, has_nl = 0;
      const char * err_dep =
        ALIKEC_deparse_chr(CAR(curr_cpy_par), -1);
      for(err_dep_len = 0; err_dep[err_dep_len]; err_dep_len++) // calc dep length
        if(err_dep[err_dep_len] == '\n') has_nl++;

      int with_nl = has_nl || err_dep_len > max_chars_net;
      /*
      Add prompt looking stuff to front of deparsed call if it is multi-line;
      really should be done elsewhere, also, we're not being super effcient
      about this since we could record nl positions earlier
      */

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
  struct ALIKEC_settings set = ALIKEC_set_def("");
  set.env = match_env;
  const char * res = ALIKEC_lang_alike_internal(target, current, set);
  if(strlen(res)) return mkString(res);
  return ScalarLogical(1);
}
