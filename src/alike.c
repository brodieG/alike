#include "alike.h"

/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Initialize settings.

We provide only the prepend argument as an input since that is the only one that
requires differnet treatment between external and internal functions.

Note settings are effectively global for the entire duration of a `.Call`
across all these functions (i.e. they are all refering to the same settings
in memory) so we have to think carefully when modifying them.

An alternative would have been to just instantiate new settings objects for each
recursion, but that seems potentially wasteful (though we haven't tested).
*/
struct ALIKEC_settings * ALIKEC_set_def(const char * prepend) {
  struct ALIKEC_settings * ALIKEC_set_tmp_val =
    (struct ALIKEC_settings *) R_alloc(1, sizeof(struct ALIKEC_settings));

  ALIKEC_set_tmp_val->type_mode = 0;
  ALIKEC_set_tmp_val->attr_mode = 0;
  ALIKEC_set_tmp_val->lang_mode = 0;
  ALIKEC_set_tmp_val->fuzzy_int_max_len = 100;
  ALIKEC_set_tmp_val->env = R_NilValue;
  ALIKEC_set_tmp_val->prepend = prepend;
  ALIKEC_set_tmp_val->env_set = 0;
  ALIKEC_set_tmp_val->no_rec = 0;
  ALIKEC_set_tmp_val->in_attr = 0;
  ALIKEC_set_tmp_val->rec_lvl = 0;
  ALIKEC_set_tmp_val->rec_lvl_last = 0;

  return ALIKEC_set_tmp_val;
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Object Check

This will not recurse directly on recursive objects, though recursive attributes
will get recursed into.

Check:
- type
- length
- attributes
*/
struct ALIKEC_res ALIKEC_alike_obj(
  SEXP target, SEXP current, struct ALIKEC_settings * set
) {
  int is_df = 0, err_lvl = 6;
  SEXPTYPE tar_type, cur_type;

  int err = 0, err_attr = 0;
  const char * err_base = "", * err_tok1, * err_tok2, * err_tok3, * err_tok4,
    * err_type, * err_lang, * err_fun;
  err_tok1 = err_tok2 = err_tok3 = err_tok4 = "";

  tar_type = TYPEOF(target);
  cur_type = TYPEOF(current);
  int s4_cur, s4_tar;
  s4_tar = ((IS_S4_OBJECT)(target) != 0);
  s4_cur = ((IS_S4_OBJECT)(current) != 0);
  if(!err && (s4_cur || s4_tar)) {  // don't run length or attribute checks on S4
    if(s4_tar + s4_cur == 1) {
      err = 1;
      err_base = "%%%%s%%%%s%%s%%%%s should %sbe S4";
      err_tok1 = (s4_tar ? "" : "not ");
    } else {
      SEXP klass, klass_attrib;
      SEXP s, t;

      klass = getAttrib(target, R_ClassSymbol);
      if(xlength(klass) != 1 || TYPEOF(klass) != STRSXP)
        error("Logic Error: unexpected S4 class \"class\" attribute of length != 1 or type not character vector; contact package maintainer");
      klass_attrib = getAttrib(klass, ALIKEC_SYM_package);
      if(xlength(klass_attrib) != 1 || TYPEOF(klass_attrib) != STRSXP)
        error("Logic Error: unexpected S4 class \"class\" attribute does not have `package` attribute in expected structure");

      // Construct call to `inherits`; we evaluate in base env since class
      // definitions should still be visible and this way unlikely that
      // inherits gets overwritten

      t = s = PROTECT(allocList(3));
      SET_TYPEOF(s, LANGSXP);
      SETCAR(t, ALIKEC_SYM_inherits); t = CDR(t);
      SETCAR(t, current); t = CDR(t);
      SETCAR(t, klass);
      if(!asLogical(eval(s, R_BaseEnv))) {
        err = 1;
        err_base = "%%%%s%%%%s%%s%%%%s should inherit from S4 class \"%s\" (package: %s)";
        err_tok1 = CHAR(asChar(klass));
        err_tok2 = CHAR(asChar(klass_attrib));
      }
      UNPROTECT(1);
    }
  } else if(target != R_NilValue) {  // Nil objects match anything when nested
    // - Attributes ------------------------------------------------------------
    /*
    Attributes must be run first to figure out whether we are dealing with a
    data frame or some such, but other than that error priority is lowest unless
    it is a class error any attribute error will get over-written by subsequent
    errors (except class errors)
    */
    struct ALIKEC_res_attr res_attr = ALIKEC_compare_attributes_internal(
      target, current, set
    );
    is_df = res_attr.df;
    err_lvl = res_attr.lvl;
    if(!res_attr.success) {
      // If top level error (class), make sure not overriden by others
      if(res_attr.lvl <= 2) err = 1;
      else err_attr = 1;
      err_base = res_attr.message;
    }
    // - Special Language Objects && Funs --------------------------------------

    int is_lang = 0;

    if(
      !err &&
      (
        is_lang = (
          (tar_type == LANGSXP || tar_type == SYMSXP) &&
          (cur_type == LANGSXP || cur_type == SYMSXP)
      ) )
    ) {
      err_lang = ALIKEC_lang_alike_internal(target, current, set);
      if(strlen(err_lang)) {
        err = 1;
        err_base = err_lang;
    } }
    int is_fun = 0;

    if(!err && (is_fun = tar_type == CLOSXP && cur_type == CLOSXP)) {
      err_fun = ALIKEC_fun_alike_internal(target, current);
      if(strlen(err_fun)) {
        err = 1;
        err_base = err_fun;
    } }
    // - Type ------------------------------------------------------------------

    if(
      !err && !is_lang && // lang excluded because we can have symbol-lang comparisons that resolve to symbol symbol
      strlen(
        err_type = ALIKEC_type_alike_internal(
          target, current, set->type_mode, set->fuzzy_int_max_len
      ) )
    ) {
      err = 1;
      err_base = err_type;
    }
    // - Length ----------------------------------------------------------------

    /*
    Note length is not checked explicilty for language objects and functions
    since parens or dots allow for different length objects to be alike, and
    for environments since rules for alikeness are different for environments
    */

    if(!is_lang && !is_fun && tar_type != ENVSXP) {
      SEXP tar_first_el, cur_first_el;
      R_xlen_t tar_len, cur_len, tar_first_el_len, cur_first_el_len;
      // if attribute error is not class, override with col count error
      // zero lengths match any length
      if(
        (!err || (is_df && err_lvl > 0)) && (tar_len = xlength(target)) > 0 &&
        tar_len != (cur_len = xlength(current))
      ) {
        err = 1;
        if(is_df) {
          err_base = CSR_smprintf4(
            ALIKEC_MAX_CHAR,
            "%%%%%%%%s%%%%%%%%s%%%%s%%%%%%%%s should have %%s column%s (has %%s)",
            err_tok2 = tar_len == (R_xlen_t) 1 ? "" : "s", "", "", ""
          );
        } else {
          err_base = "%%%%s%%%%s%%s%%%%s should be length %s (is %s)";
        }
        err_tok1 = CSR_len_as_chr(tar_len);
        err_tok2 = CSR_len_as_chr(cur_len);
      } else if (
        is_df && err_lvl > 0 && tar_type == VECSXP && XLENGTH(target) &&
        TYPEOF(current) == VECSXP && XLENGTH(current) &&
        isVectorAtomic((tar_first_el = VECTOR_ELT(target, 0))) &&
        isVectorAtomic((cur_first_el = VECTOR_ELT(current, 0))) &&
        (tar_first_el_len = XLENGTH(tar_first_el)) && tar_first_el_len &&
        tar_first_el_len != (cur_first_el_len = XLENGTH(cur_first_el))
      ) {
        // check for row count error, note this isn't a perfect check since we
        // check the first column only

        err_base = "%%%%s%%%%s%%s%%%%s should have %s row%s (has %s)";
        err_tok1 = CSR_len_as_chr(tar_first_el_len);
        err_tok2 = tar_first_el_len == (R_xlen_t) 1 ? "" : "s";
        err_tok3 = CSR_len_as_chr(cur_first_el_len);
    } }
  }
  // - Known Limitations -------------------------------------------------------

  if(!set->suppress_warnings) {
    switch(tar_type) {
      case NILSXP:
      case LGLSXP:
      case INTSXP:
      case REALSXP:
      case CPLXSXP:
      case STRSXP:
      case VECSXP:
      case S4SXP:
      case ENVSXP:
      case LISTSXP:
      case LANGSXP:
      case CLOSXP:
      case BUILTINSXP:
      case SPECIALSXP:
      case EXPRSXP:
      case SYMSXP:
      case EXTPTRSXP:
      case WEAKREFSXP:
        break;
      default:
        warning(
          "`alike` behavior for objects of type \"%s\" is not well defined and may change in the future",
          type2char(tar_type)
        );
    }
  }
  struct ALIKEC_res res;
  res.df = is_df;
  if(err || err_attr) {
    res.success = 0;
    res.message = CSR_smprintf4(
      ALIKEC_MAX_CHAR, err_base, err_tok1, err_tok2, err_tok3, err_tok4
    );
  } else {
    res.success = 1;
  }
  res.indices = 0; // initialize to null pointer
  return res;
}
/*
Utility functions for updating index lest for error reporting.  General logic
is to track depth of recursion, and when an error occurs, allocate enough
space for as many ALIKEC_index structs as there is recursion depth.
*/
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Recursion functions

The first set here manages tracking the indices during recursion
*/
void ALIKEC_res_ind_set(
  struct ALIKEC_res res, struct ALIKEC_index ind, size_t lvl
) {
  // Find correct spot in previously allocated indices spaces, clearly relies on
  // lvl being exactly correct...

  struct ALIKEC_index * cur_ind = res.indices + lvl - 1;
  *cur_ind = ind;
}
void ALIKEC_res_ind_chr(struct ALIKEC_res res, const char * ind, size_t lvl) {
  union ALIKEC_index_raw ind_u = {.chr = ind};
  ALIKEC_res_ind_set(res, (struct ALIKEC_index) {ind_u, 1}, lvl);
}
void ALIKEC_res_ind_num(struct ALIKEC_res res, R_xlen_t ind, size_t lvl) {
  union ALIKEC_index_raw ind_u = {.num = ind};
  ALIKEC_res_ind_set(res, (struct ALIKEC_index) {ind_u, 0}, lvl);
}
struct ALIKEC_res ALIKEC_res_ind_init(
  struct ALIKEC_res res, struct ALIKEC_settings * set
) {
  if(set->rec_lvl <= set->rec_lvl_last)
    error(
      "Logic Error: negative recursion level detected %d %d",
      set->rec_lvl, set->rec_lvl_last
    );
  set->rec_lvl--;
  size_t rec_off = set->rec_lvl - set->rec_lvl_last;
  res.rec_lvl = rec_off;
  if(rec_off) {
    res.indices = (struct ALIKEC_index *)
      R_alloc(rec_off, sizeof(struct ALIKEC_index));
  }
  return res;
}
/*
Handle recursive types; these include VECSXP, environments, and pair lists.
*/
struct ALIKEC_res ALIKEC_alike_rec(
  SEXP target, SEXP current, struct ALIKEC_settings * set
) {
  /*
  Recurse through various types of recursive structures.
  VERY IMPORTANT: there are several return points throughout here that don't
  actually imply recursion; make sure to initialize the index tracking when
  that happens.  Note that exit points to ALIKEC_alike_rec don't require this.

  Side note: probably don't need to generate full error report unless we're on
  outermost `ALIKEC_alike_internal` call since we don't display the inner
  reports anyway, so could be a bit more efficient there.

  General logic here is to check object for alikeness; if not initialize index
  and return error structure, if so then recurse using specialized logic for
  each type of recursive structure.  After recursion, check if recursion
  failed and if so record current index.  Since this happens at every level of
  the recursion we can recreate the full index to the location of the error.
  */
  void R_CheckUserInterrupt(void);

  size_t rec_lvl_old = set->rec_lvl;
  set->rec_lvl++;  //Mark recursion depth
  if(rec_lvl_old >= set->rec_lvl)
    error("Logic Error: recursion stack depth exceeded 264 - shouldn't happen");

  // normal logic, which will have checked length and attributes, etc.

  struct ALIKEC_res res0 = ALIKEC_alike_obj(target, current, set);

  if(!res0.success) {
    res0 = ALIKEC_res_ind_init(res0, set);  // Initialize index tracking
    return res0;
  }
  struct ALIKEC_res res1 = {1, "", res0.df, 0};
  R_xlen_t tar_len = xlength(target);
  SEXPTYPE tar_type = TYPEOF(target);

  if(tar_type == VECSXP || tar_type == EXPRSXP) {
    R_xlen_t i;

    for(i = 0; i < tar_len; i++) {
      res1 = ALIKEC_alike_rec(
        VECTOR_ELT(target, i), VECTOR_ELT(current, i), set
      );
      if(!res1.success) {
        SEXP vec_names = getAttrib(target, R_NamesSymbol);
        const char * ind_name;
        if(
          vec_names == R_NilValue ||
          !((ind_name = CHAR(STRING_ELT(vec_names, i))))[0]
        )
          ALIKEC_res_ind_num(res1, i + 1, set->rec_lvl);
        else
          ALIKEC_res_ind_chr(res1, ind_name, set->rec_lvl);
        break;
      }
    }
  } else if (tar_type == ENVSXP && !set->in_attr) {
    // Need to guard against possible circular reference in the environments
    if(!set->env_set) {
      set->env_set = ALIKEC_env_set_create(16);
    }
    if(!set->no_rec) set->no_rec = !ALIKEC_env_track(target, set->env_set);
    if(set->no_rec < 0 && !set->suppress_warnings) {
      warning(
        "`alike` environment stack exhausted; %s.",
        "unable to recurse any further into environments"
      );
      set->no_rec = 1; // so we only get warning once
    }
    if(set->no_rec || target == current) {
      res1.success = 1;
    } else {
      if(target == R_GlobalEnv && current != R_GlobalEnv) {
        res1.success = 0;
        res1.message = "be the global environment";
        return(res1);
      }
      SEXP tar_names = PROTECT(R_lsInternal(target, TRUE));
      R_xlen_t tar_name_len = XLENGTH(tar_names), i;

      if(tar_name_len != tar_len)
        error("Logic Error: mismatching name-env lengths; contact maintainer");
      for(i = 0; i < tar_len; i++) {
        const char * var_name_chr = CHAR(STRING_ELT(tar_names, i));
        SEXP var_name = PROTECT(install(var_name_chr));
        SEXP var_cur_val = findVarInFrame(current, var_name);
        if(var_cur_val == R_UnboundValue) {
          res1.success = 0;
          res1.message = CSR_smprintf4(
            ALIKEC_MAX_CHAR, "contain variable `%s`",
            CHAR(asChar(STRING_ELT(tar_names, i))), "", "", ""
          );
          // Initialize index tracking since this is not a recursion error

          res1 = ALIKEC_res_ind_init(res1, set);
          UNPROTECT(2);
          return(res1);
        }
        res1 = ALIKEC_alike_rec(
          findVarInFrame(target, var_name), var_cur_val, set
        );
        UNPROTECT(1);
        if(!res1.success) {
          ALIKEC_res_ind_chr(res1, var_name_chr, set->rec_lvl);
          break;
        }
      }
      UNPROTECT(1);
    }
  } else if (tar_type == LISTSXP) {
    SEXP tar_sub, cur_sub;
    R_xlen_t i = 0;
    for(
      tar_sub = target, cur_sub = current; tar_sub != R_NilValue;
      tar_sub = CDR(tar_sub), cur_sub = CDR(cur_sub), i++
    ) {
      // Check tag names; should be in same order??  Probably

      SEXP tar_tag = TAG(tar_sub);
      SEXP tar_tag_chr = PRINTNAME(tar_tag);
      if(tar_tag != R_NilValue && tar_tag != TAG(cur_sub)) {
        res1.message = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "have name \"%s\" at pairlist index [[%s]]",
          CHAR(asChar(tar_tag_chr)), CSR_len_as_chr(i + 1), "", ""
        );
        res1.success = 0;
        res1 = ALIKEC_res_ind_init(res1, set);  // Initialize index tracking
        return res1;
      }
      res1 = ALIKEC_alike_rec(CAR(tar_sub), CAR(cur_sub), set);
      if(!res1.success) {
        if(tar_tag != R_NilValue)
          ALIKEC_res_ind_chr(res1, CHAR(asChar(tar_tag_chr)), set->rec_lvl);
        else
          ALIKEC_res_ind_num(res1, i + 1, set->rec_lvl);
        break;
      }
    }
  } else {
    res1 = res0;
  }
  res1.df = res0.df;
  set->rec_lvl--;
  return res1;
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Run alike calculation, and in particular, compose error message if relevant.

Return value is a character value that starts with %s and follows with
something like "should be ...".
*/
const char * ALIKEC_alike_internal(
  SEXP target, SEXP current, struct ALIKEC_settings * set
) {
  if(set->type_mode < 0 || set->type_mode > 2)
    error("Argument `type.mode` must be in 0:2");
  if(set->attr_mode < 0 || set->attr_mode > 2)
    error("Argument `attr.mode` must be in 0:2");
  char * err_base;
  size_t rec_lvl_last_prev = set->rec_lvl_last;

  // Allows us to keep track of recursion depth between calls to
  // ALIKEC_alike_internal

  set->rec_lvl_last = set->rec_lvl;

  struct ALIKEC_res res;

  if(TYPEOF(target) == NILSXP && TYPEOF(current) != NILSXP) {
    // Handle NULL special case at top level

    err_base = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "%%%%s%%%%s%%s%%%%s should be \"NULL\" (is \"%s\")",
      type2char(TYPEOF(current)), "", "", ""
    );
  } else {
    // Recursively check object

    res = ALIKEC_alike_rec(target, current, set);
    if(res.success) {
      set->rec_lvl_last = rec_lvl_last_prev;
      return "";
    }
    err_base = res.message;
  }
  // - Contruct Error ----------------------------------------------------------

  char * err_final, * err_msg;
  err_msg = err_base;
  /*
  Compute the part of the error that gives the index where the discrepancy
  occurred.
  */
  if(!res.rec_lvl) {  // No recursion occurred
    err_final = CSR_smprintf4(ALIKEC_MAX_CHAR, err_msg, "", "", "", "");
  } else {
    // Scan through all indices to calculate size of required vector

    char * err_chr_index, * err_chr_indices;
    const char * err_chr_index_val;
    size_t err_size = 0, ind_size_max = 0, ind_size;

    for(size_t i = 0; i < res.rec_lvl; i++) {
      switch(res.indices[i].type) {
        case 0:
          ind_size = CSR_len_chr_len(res.indices[i].ind.num);
          break;
        case 1:
          ind_size = CSR_strmlen(res.indices[i].ind.chr, ALIKEC_MAX_CHAR) + 2;
          break;
        default: {
          error(
            "Logic Error: unexpected index type %d; contact maintainer.",
            res.indices[i].type
          );
        }
      }
      if(ind_size > ind_size_max) ind_size_max = ind_size;
      err_size += ind_size;
    }
    // Allways alloacate as if index will be in [[index]] form as that is
    // worst case
    err_chr_indices = (char *) R_alloc(
      err_size + 4 * res.rec_lvl + 1, sizeof(char)
    );
    err_chr_index = (char *) R_alloc(ind_size_max + 4 + 1, sizeof(char));
    err_chr_indices[0] = '\0';

    for(size_t i = 0; i < res.rec_lvl; i++) {
      const char * index_tpl = "[[%s]]";
      switch(res.indices[i].type) {
        case 0:
          {
            err_chr_index_val =
              (const char *) CSR_len_as_chr(res.indices[i].ind.num);
          }
          break;
        case 1:
          {
            err_chr_index_val = res.indices[i].ind.chr;
            if(!ALIKEC_is_valid_name(err_chr_index_val)){
              index_tpl = "$`%s`";
            } else {
              index_tpl = "$%s";
            }
          }
          break;
        default:
          error(
            "Logic Error: unexpected index type (2) %d", res.indices[i].type
          );
      }
      // Used to leave off last index for possible different treatment if
      // dealing with a DF, but giving that up for now

      sprintf(err_chr_index, index_tpl, err_chr_index_val);
      if(i < res.rec_lvl) {  // all these chrs should be terminated...
        strcat(err_chr_indices, err_chr_index);
      }
    }
    int has_nl = 0;
    for(int i = 0; err_msg[i] && i < ALIKEC_MAX_CHAR; i++) {
      if(err_msg[i] == '\n') {
        has_nl = 1;
        break;
    } }
    // err_msg: error message produce by ALIKEC_alike_rec
    // has_nl: whether `err_msg` contains a new line
    // %s used to preserve the %s for use in _wrap

    err_final = CSR_smprintf4(
      ALIKEC_MAX_CHAR, err_msg, err_chr_indices, "", "", ""
    );
  }
  set->rec_lvl_last = rec_lvl_last_prev;
  return (const char *) err_final;
}
/*
Outermost alike function, handles full rendering including the leading
substituted expression

Note that width is only really used to control the deparse wrapping; rest of
text is not wrapped.  Negative width will use the getOption("width");
*/
const char * ALIKEC_alike_wrap(
  SEXP target, SEXP current, SEXP curr_sub, struct ALIKEC_settings * set,
  int width
) {
  if(
    TYPEOF(curr_sub) != LANGSXP && TYPEOF(curr_sub) != SYMSXP &&
    !(isVectorAtomic(curr_sub) && XLENGTH(curr_sub) == 1)
  )
    error(
      "Logic Error; `curr_sub` must be language."
    );
  const char * res = ALIKEC_alike_internal(target, current, set);

  // Have an error, need to populate the object by deparsing the relevant
  // expression.  One issue here is we want different treatment depending on
  // how wide the error is; if the error is short enough we can include it
  // inline; otherwise we need to modify how we display it

  if(res[0]) {
    // Handle case where expression is a binary operator; in these cases we need
    // to wrap calls in parens so that any subsequent indices we use make sense,
    // though in reality we need to improve this so that we only use parens when
    // strictly needed

    // one branch below creates SEXP, so pre-emptively protect

    SEXP curr_fin = PROTECT(curr_sub);

    if(TYPEOF(curr_sub) == LANGSXP) {
      SEXP call = CAR(curr_sub);
      if(TYPEOF(call) == SYMSXP) {
        const char * call_sym = CHAR(PRINTNAME(call));
        int is_an_op = 0, i = 1;
        if(
          !strcmp("+", call_sym) || !strcmp("-", call_sym) ||
          !strcmp("*", call_sym) || !strcmp("/", call_sym) ||
          !strcmp("^", call_sym) || !strcmp("|", call_sym) ||
          !strcmp("||", call_sym) || !strcmp("&", call_sym) ||
          !strcmp("&&", call_sym)
        ) is_an_op = 1;
        if(!is_an_op && call_sym[0] == '%') {
          // check for %xx% operators
          while(call_sym[i] && i < 1024) i++;
          if(i < 1024 && i > 1 && call_sym[i - 1] == '%') is_an_op = 1;
        }
        if(is_an_op) {
          curr_fin = list2(ALIKEC_SYM_paren_open, curr_sub);
          SET_TYPEOF(curr_fin, LANGSXP);
    } } }
    // Now deparse

    if(width < 0) width = asInteger(ALIKEC_getopt("width"));
    if(width < 10 || width > 1000) width = 80;

    int dep_cutoff;

    if(width < 62) dep_cutoff = width - 2;
    else dep_cutoff = 60;
    if(dep_cutoff < 20) dep_cutoff = 60;

    SEXP curr_sub_dep = PROTECT(ALIKEC_deparse(curr_fin, dep_cutoff));

    // Handle the different deparse scenarios

    int multi_line = 1;
    const char * dep_chr = CHAR(asChar(curr_sub_dep));

    if(XLENGTH(curr_sub_dep) == 1) {
      if(CSR_strmlen(dep_chr, ALIKEC_MAX_CHAR) <= width - 2) multi_line = 0;
    }
    const char * call_char, * call_pre = "", * call_post = "";
    if(multi_line) {
      call_pre = "Expression:\n%s";
      call_char = ALIKEC_pad(curr_sub_dep, -1, 2);
    } else {
      call_pre = "`";
      call_post = "`";
      call_char = dep_chr;
    }
    UNPROTECT(2);
    return CSR_smprintf4(
      ALIKEC_MAX_CHAR, res, call_pre, call_char, call_post, ""
    );
  }
  return "";
}

/*
"fast" version doesn't allow messing with optional parameters to avoid arg
evaluations in R; this is basically deprecated now though still accessible
through the non-exported `.alike2` R function
*/
SEXP ALIKEC_alike_fast2(SEXP target, SEXP current) {
  struct ALIKEC_settings * set = ALIKEC_set_def("should ");
  return ALIKEC_string_or_true(ALIKEC_alike_internal(target, current, set));
}
/*
Originally the "fast" version, but is now the version that allows us to specify
settings
*/
SEXP ALIKEC_alike_fast1(SEXP target, SEXP current, SEXP settings) {
  if(settings == R_NilValue) {
    return ALIKEC_alike_fast2(target, current);
  } else if (TYPEOF(settings) == VECSXP && XLENGTH(settings) == 6) {
    return ALIKEC_alike(
      target, current, VECTOR_ELT(settings, 0), VECTOR_ELT(settings, 1),
      VECTOR_ELT(settings, 2), VECTOR_ELT(settings, 3), VECTOR_ELT(settings, 4),
      VECTOR_ELT(settings, 5)
    );
  }
  error("Argument `settings` is not a length 5 list as expected");
  return R_NilValue;
}
/*
Main external interface, no settings
*/
SEXP ALIKEC_alike_ext(
  SEXP target, SEXP current, SEXP curr_sub, SEXP env
) {
  if(TYPEOF(env) != ENVSXP)
    error(
      "Logic Error; `env` argument should be environment; contact maintainer."
    );

  if(
    TYPEOF(curr_sub) != LANGSXP && TYPEOF(curr_sub) != SYMSXP &&
    !(isVectorAtomic(curr_sub) && XLENGTH(curr_sub) == 1)
  )
    error(
      "Logic Error; `curr_sub` must be language."
    );
  struct ALIKEC_settings * set = ALIKEC_set_def("");
  set->env = env;
  int width = -1;
  return ALIKEC_string_or_true(
    ALIKEC_alike_wrap(target, current, curr_sub, set, width)
  );
}
/*
Semi-internal interface; used to be the main external one but no longer as we
changed the interface
*/
SEXP ALIKEC_alike (
  SEXP target, SEXP current, SEXP type_mode, SEXP attr_mode, SEXP env,
  SEXP fuzzy_int_max_len, SEXP suppress_warnings, SEXP lang_mode
) {
  SEXPTYPE int_mod_type, fuzzy_type, attr_mod_type, lang_mod_type;
  int supp_warn = 0, type_int = 0, attr_int = 0, lang_int = 0;
  R_xlen_t fuzzy_int_max_len_int;

  int_mod_type = TYPEOF(type_mode);
  attr_mod_type = TYPEOF(attr_mode);
  lang_mod_type = TYPEOF(lang_mode);
  fuzzy_type = TYPEOF(fuzzy_int_max_len);

  if(
    (int_mod_type != INTSXP && int_mod_type != REALSXP) ||
    XLENGTH(type_mode) != 1 || (type_int = asInteger(type_mode)) == NA_INTEGER ||
    type_int < 0 || type_int > 2
  )
    error("Argument `type.mode` must be a one length numeric between 0 and 2");
  if(
    (attr_mod_type != INTSXP && attr_mod_type != REALSXP) ||
    XLENGTH(attr_mode) != 1 || (attr_int = asInteger(attr_mode)) == NA_INTEGER ||
    attr_int < 0 || attr_int > 2
  )
    error("Argument `attr.mode` must be a one length numeric");
  if(
    (lang_mod_type != INTSXP && lang_mod_type != REALSXP) ||
    XLENGTH(lang_mode) != 1 || (lang_int = asInteger(lang_mode)) == NA_INTEGER ||
    lang_int < 0 || lang_int > 1
  )
    error("Argument `lang.mode` must be a one length numeric between 0 and 1");
  if(
    (fuzzy_type != INTSXP && fuzzy_type != REALSXP) ||
    XLENGTH(fuzzy_int_max_len) != 1 ||
    (fuzzy_int_max_len_int = asInteger(fuzzy_int_max_len)) == NA_INTEGER
  )
    error("Argument `fuzzy.int.max.len` must be an integer one length vector");
  if(
    TYPEOF(suppress_warnings) != LGLSXP || XLENGTH(suppress_warnings) != 1 ||
    (supp_warn = asLogical(suppress_warnings)) == NA_LOGICAL
  )
    error("Argument `suppress.warnings` must be TRUE or FALSE");
  if(env != R_NilValue && TYPEOF(env) != ENVSXP)
    error("Argument `env` must be NULL or an environment");

  struct ALIKEC_settings * set = ALIKEC_set_def("should ");
  set->type_mode = type_int;
  set->attr_mode = attr_int;
  set->lang_mode = lang_int;
  set->fuzzy_int_max_len = fuzzy_int_max_len_int;
  set->suppress_warnings = supp_warn;
  set->env = env;

  return ALIKEC_string_or_true(ALIKEC_alike_internal(target, current, set));
}
