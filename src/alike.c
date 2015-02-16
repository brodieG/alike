#include "alike.h"

/*
Construct settings objects; these are really only used for testing sub-functions
or for the `fast` alike function.

We provide only the prepend argument as an input since that is the only one that
requires differnet treatment between fast and internal functions
*/
struct ALIKEC_settings * ALIKEC_set_def(const char * prepend) {
  struct ALIKEC_settings * ALIKEC_set_tmp_val =
    (struct ALIKEC_settings *) R_alloc(1, sizeof(struct ALIKEC_settings));

  ALIKEC_set_tmp_val->type_mode = 0;
  ALIKEC_set_tmp_val->attr_mode = 0;
  ALIKEC_set_tmp_val->int_tolerance = 0.0;
  ALIKEC_set_tmp_val->match_env = R_NilValue;
  ALIKEC_set_tmp_val->prepend = prepend;

  return ALIKEC_set_tmp_val;
}
/*
non recursive check (well, except for attributes will recurse if needed in
final implementation)
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
      err_base = "%sbe S4";
      err_tok1 = (s4_tar ? "" : "not ");
    } else {
      // Here we pull the class symbol, install "inherits" from R proper, and
      // evaluate it in the base environment to avoid conflicts with other
      // symbols

      SEXP klass, klass_attrib;
      SEXP s, t;

      klass = getAttrib(target, R_ClassSymbol);
      if(xlength(klass) != 1 || TYPEOF(klass) != STRSXP)
        error("Logic Error: unexpected S4 class \"class\" attribute of length != 1 or type not character vector; contact package maintainer");
      klass_attrib = getAttrib(klass, ALIKEC_SYM_package);
      if(xlength(klass_attrib) != 1 || TYPEOF(klass_attrib) != STRSXP)
        error("Logic Error: unexpected S4 class \"class\" attribute does not have `package` attribute in expected structure");

      t = s = PROTECT(allocList(3));
      SET_TYPEOF(s, LANGSXP);
      SETCAR(t, ALIKEC_SYM_inherits); t = CDR(t);
      SETCAR(t, current); t = CDR(t);
      SETCAR(t, klass);
      if(!asLogical(eval(s, R_BaseEnv))) {
        err = 1;
        err_base = "inherit from S4 class \"%s\" (package: %s)";
        err_tok1 = CHAR(asChar(klass));
        err_tok2 = CHAR(asChar(klass_attrib));
      }
      UNPROTECT(1);
    }
  } else if(target != R_NilValue) {  // Nil objects match anything when nested (should also be true for envs?)
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
      if(res_attr.lvl == 0) err = 1;   // If top level error (class), make sure not overriden by others
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
      err_lang = ALIKEC_lang_alike_internal(target, current, set->match_env);
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
          target, current, set->type_mode, set->int_tolerance
      ) )
    ) {
      err = 1;
      err_base = err_type;
    }
    // - Length ----------------------------------------------------------------

    // Note length is not checked explicilty for language objects and functions
    // since parens or dots allow for different length objects to be alike

    if(!is_lang && !is_fun) {
      SEXP tar_first_el, cur_first_el;
      R_xlen_t tar_len, cur_len, tar_first_el_len, cur_first_el_len;
      if(
        (!err || (is_df && err_lvl > 0))  &&   // if attribute error is not class, override with col count error
        (tar_len = xlength(target)) > 0 &&       /* zero lengths match any length */
        tar_len != (cur_len = xlength(current))
      ) {
        err = 1;
        if(is_df) {
          err_base = CSR_smprintf4(
            ALIKEC_MAX_CHAR, "have %%s column%s (has %%s)",
            err_tok2 = tar_len == (R_xlen_t) 1 ? "" : "s", "", "", ""
          );
        } else {
          err_base = "be length %s (is %s)";
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

        err_base = "have %s row%s (has %s)";
        err_tok1 = CSR_len_as_chr(tar_first_el_len);
        err_tok2 = tar_first_el_len == (R_xlen_t) 1 ? "" : "s";
        err_tok3 = CSR_len_as_chr(cur_first_el_len);
    } }
  }
  // - Known Limitations -----------------------------------------------------

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
  return res;
}
/*
Handle recursive types; these include VECSXP, environments, and pair lists.
One possible slow step here is that we keep create new CONS for each new
index element; not sure if this is meanifully slow or not
*/

struct ALIKEC_res ALIKEC_alike_rec(
  SEXP target, SEXP current, SEXP index, struct ALIKEC_settings * set
) {
  // normal logic, which will have checked length and attributes, etc.

  struct ALIKEC_res res0 = ALIKEC_alike_obj(target, current, set);

  if(!res0.success) {
    return (struct ALIKEC_res) {0, res0.message, res0.df};
  }
  // Recurse

  struct ALIKEC_res res1 = {1, "", res0.df};
  R_xlen_t tar_len = xlength(target);
  SEXPTYPE tar_type = TYPEOF(target);

  if(tar_type == VECSXP || tar_type == EXPRSXP) {
    R_xlen_t i;
    SEXP vec_names = getAttrib(target, R_NamesSymbol);

    for(i = 0; i < tar_len; i++) {
      if(vec_names == R_NilValue || !CHAR(STRING_ELT(vec_names, i))[0])
        SETCDR(index, list1(ScalarInteger(i)));
      else SETCDR(index, list1(STRING_ELT(vec_names, i)));
      res1 = ALIKEC_alike_rec(
        VECTOR_ELT(target, i), VECTOR_ELT(current, i), CDR(index), set
      );
      // Rprintf("Loop: %d success: %d", i, res1.success);
      if(!res1.success) break;
    }
  } else if (tar_type == ENVSXP) {
    SEXP tar_names = PROTECT(R_lsInternal(target, TRUE));
    R_xlen_t tar_name_len = XLENGTH(tar_names), i;
    if(tar_name_len != tar_len)
      error("Logic Error: mismatching env lengths; contact maintainer");
    for(i = 0; i < tar_len; i++) {
      SEXP var_name = PROTECT(install(CHAR(STRING_ELT(tar_names, i))));
      SEXP var_cur_val = findVarInFrame(current, var_name);
      if(var_cur_val == R_UnboundValue) {
        res1.success = 0;
        res1.message = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "contain variable `%s`",
          CHAR(asChar(STRING_ELT(tar_names, i))), "", "", ""
        );
        UNPROTECT(2);
        break;
      }
      SETCDR(index, list1(STRING_ELT(tar_names, i)));
      res1 = ALIKEC_alike_rec(
        findVarInFrame(target, var_name), var_cur_val, CDR(index), set
      );
      UNPROTECT(1);
      if(!res1.success) break;
    }
    UNPROTECT(1);
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
        SETCDR(index, R_NilValue);
        res1.success = 0;
        break;
      }
      if(tar_tag != R_NilValue) SETCDR(index, list1(asChar(tar_tag_chr)));
      else SETCDR(index, list1(ScalarInteger(i)));
      res1 = ALIKEC_alike_rec(CAR(tar_sub), CAR(cur_sub), CDR(index), set);
      if(!res1.success) break;
    }
  } else {
    return res0;
  }
  res1.df = res0.df;  // Indicate whether parent object was a data frame
  return res1;
}
/*
Run alike calculation
*/
const char * ALIKEC_alike_internal(
  SEXP target, SEXP current, struct ALIKEC_settings * set
) {
  if(set->type_mode < 0 || set->type_mode > 2)
    error("Argument `type.mode` must be in 0:2");
  if(set->attr_mode < 0 || set->attr_mode > 2)
    error("Argument `attr.mode` must be in 0:2");
  char * err_base;
  char * err_prepend = CSR_strmcpy(set->prepend, ALIKEC_MAX_CHAR);
  set->prepend = "";  // Prepend only applies to outer most loop
  SEXP index = PROTECT(list1(R_NilValue));

  struct ALIKEC_res res;

  if(TYPEOF(target) == NILSXP && TYPEOF(current) != NILSXP) {
    // Handle NULL special case at top level

    err_base = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "be type \"NULL\" (is \"%s\")", type2char(TYPEOF(current)), "", "", ""
    );
  } else {
    // Recursively check object

    res = ALIKEC_alike_rec(target, current, index, set);
    if(res.success) {
      UNPROTECT(1);
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
  if(CDR(index) == R_NilValue) {  // No recursion occurred
    err_final = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "%s%s%s%s", err_prepend, err_msg, "", ""
    );
  } else {
    // Scan through all indices to calculate size of required vector

    char * err_chr_index, * err_chr_indeces;
    const char * err_chr_index_val;
    SEXP ind_sub;
    SEXP ind_sub_val;
    size_t err_size = 0, levels = 0, ind_size_max = 0, ind_size;

    for(ind_sub = CDR(index); ind_sub != R_NilValue; ind_sub = CDR(ind_sub), levels++) {
      ind_sub_val = CAR(ind_sub);
      switch(TYPEOF(ind_sub_val)) {
        case INTSXP:
          ind_size = CSR_len_chr_len(asInteger(ind_sub_val));
          break;
        case CHARSXP:
          ind_size = CSR_strmlen(CHAR(ind_sub_val), ALIKEC_MAX_CHAR) + 2;
          break;
        default: {
          error("Logic Error: unexpected index type 1 \"%s\"; contact maintainer.", type2char(TYPEOF(ind_sub_val)));
        }
      }
      if(ind_size > ind_size_max) ind_size_max = ind_size;
      err_size += ind_size;
    }
    err_chr_indeces = (char *) R_alloc(err_size + 4 * levels + 1, sizeof(char));
    err_chr_index = (char *) R_alloc(ind_size_max + 4 + 1, sizeof(char));
    err_chr_indeces[0] = '\0';

    size_t i;

    for(ind_sub = CDR(index), i = 0; i < levels; ind_sub = CDR(ind_sub), i++) {
      ind_sub_val = CAR(ind_sub);
      const char * index_tpl = "[[%s]]";
      switch(TYPEOF(ind_sub_val)) {
        case INTSXP:
          err_chr_index_val = (const char *) CSR_len_as_chr(asInteger(ind_sub_val) + 1);
          break;
        case CHARSXP:
          err_chr_index_val = CHAR(ind_sub_val);
          index_tpl = "[[\"%s\"]]";
          break;
        default:
          error("Logic Error: unexpected index type 2; contact maintainer.");
      }
      // leave off last index as treated differently if it is a DF column vs not
      // that will be dealt in the next step

      sprintf(err_chr_index, index_tpl, err_chr_index_val);
      if(i < levels - 1) {  // all these chrs should be terminated...
        strcat(err_chr_indeces, err_chr_index);
      }
    }
    char * err_interim;

    if(levels == 1) {
      if(res.df) {
        err_interim = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "column `%s`", err_chr_index_val, "", "", ""
        );
      } else {
        err_interim = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "index %s", err_chr_index, "", "", ""
        );
      }
    } else {
      if(res.df) {
        err_interim = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "column `%s` at index %s", err_chr_index_val,
          err_chr_indeces, "", ""
        );
      } else {
        err_interim = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "index %s%s", err_chr_indeces, err_chr_index, "", ""
    );} }
    err_final = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "%s%s at %s", err_prepend, err_msg, err_interim, ""
    );
  }
  UNPROTECT(1);
  return (const char *) err_final;
}

/* "fast" version doesn't allow messing with optional parameters to avoid arg
evaluations in R; note that S4 tests will be done by evaluating `inherits` in
R_GlobalEnv, which should be fine since all the other arguments to `alike` have
already been evaluated, but the standard `alike` function evaluates it in the
calling environment */

SEXP ALIKEC_alike_fast(SEXP target, SEXP current) {
  struct ALIKEC_settings * set = ALIKEC_set_def("should ");
  return ALIKEC_string_or_true(ALIKEC_alike_internal(target, current, set));
}
/* Normal version, a little slower but more flexible */

SEXP ALIKEC_alike (
  SEXP target, SEXP current, SEXP type_mode, SEXP int_tolerance, SEXP attr_mode,
  SEXP suppress_warnings, SEXP match_env
) {
  SEXPTYPE int_mod_type, tol_type, attr_mod_type;
  int supp_warn;

  int_mod_type = ALIKEC_typeof_internal(type_mode, sqrt(DOUBLE_EPS));
  attr_mod_type = ALIKEC_typeof_internal(attr_mode, sqrt(DOUBLE_EPS));
  tol_type = ALIKEC_typeof_internal(int_tolerance, sqrt(DOUBLE_EPS));

  if(int_mod_type != INTSXP || XLENGTH(type_mode) != 1)   /* borrowed code from type_alike, maybe needs to be function */
    error("Argument `type.mode` must be a one length integer like vector");
  if(attr_mod_type != INTSXP || XLENGTH(attr_mode) != 1)   /* borrowed code from type_alike, maybe needs to be function */
    error("Argument `attr.mode` must be a one length integer like vector");
  if((tol_type != INTSXP && tol_type != REALSXP) || XLENGTH(int_tolerance) != 1)
    error("Argument `int.tol` must be a one length numeric vector");
  if(
    TYPEOF(suppress_warnings) != LGLSXP || XLENGTH(suppress_warnings) != 1 ||
    (supp_warn = asLogical(suppress_warnings)) == NA_LOGICAL
  )
    error("Argument `suppress.warnings` must be TRUE or FALSE");

  struct ALIKEC_settings * set = &(struct ALIKEC_settings) {
    asInteger(type_mode), asReal(int_tolerance),
    asInteger(attr_mode), "should ", supp_warn, match_env
  };
  return ALIKEC_string_or_true(ALIKEC_alike_internal(target, current, set));
}
