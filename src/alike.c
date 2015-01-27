#include "alike.h"

struct ALIKEC_res {
  int success;
  const char * message;
  int df;
};
/*
non recursive check
*/
ALIKEC_res ALIKEC_alike_obj(
  SEXP target, SEXP current, SEXP index, int int_mode, double int_tolerance,
  int attr_mode, int suppress_warning
) {
  int tmp = 0;
  int * is_df = &tmp;
  SEXPTYPE tar_type;
  SEXP tar_first_el, cur_first_el;

  int err = 0;
  const char * err_base, * err_tok1, * err_tok2, * err_tok3, * err_tok4,
    * err_type, * err_attr;

  tar_type = TYPEOF(target);
  int s4_cur, s4_tar;
  s4_tar = ((IS_S4_OBJECT)(target) != 0);
  s4_cur = ((IS_S4_OBJECT)(current) != 0);
  if(!err && (s4_cur || s4_tar)) {  // don't run length or attribute checks on S4
    if(s4_tar + s4_cur == 1) {
      err = 1;
      err_base = "%sbe S4";
      err_tok1 = (s4_tar ? "" : "not ");
      err_tok2 = "";
      err_tok3 = err_tok4 = "";
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
        err_tok3 = err_tok4 = "";
      }
      UNPROTECT(1);
    }
  } else if(target != R_NilValue) {  // Nil objects match anything when nested (should also be true for envs?)
  // - Attributes ------------------------------------------------------------
    int tmp = -1;
    int * err_lvl =& tmp;

    if (
      !err &&
      strlen(
        err_attr = ALIKEC_compare_attributes_internal(
          target, current, attr_mode, is_df, err_lvl
      ) )
    ) {
      err = 1;
      err_base = err_attr;
      err_tok1 = err_tok2 = err_tok3 = err_tok4 = "";
    }
  // - Type ------------------------------------------------------------------

    if(
      !err &&
      strlen(
        err_type = ALIKEC_type_alike_internal(
          target, current, int_mode, int_tolerance
      ) )
    ) {
      err = 1;
      err_base = err_type;
      err_tok1 = err_tok2 = err_tok3 = err_tok4 = "";
    }

  // - Length ----------------------------------------------------------------

    if(
      (!err || (*is_df && *err_lvl > 0))  &&   // if attribute error is not class, override with col count error
      (tar_len = xlength(target)) > 0 &&       /* zero lengths match any length */
      tar_len != (cur_len = xlength(current))
    ) {
      err = 1;
      if(*is_df) {
        err_base = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "have %%s column%s (has %%s)",
          err_tok2 = tar_len == (R_xlen_t) 1 ? "" : "s", "", "", ""
        );
      } else {
        err_base = "be length %s (is %s)";
      }
      err_tok1 = CSR_len_as_chr(tar_len);
      err_tok2 = CSR_len_as_chr(cur_len);
      err_tok3 = err_tok4 = "";
    } else if (
      *is_df && *err_lvl > 0 && tar_type == VECSXP && XLENGTH(target) &&
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
    }
  }
  // - Known Limitations -----------------------------------------------------

  if(!suppress_warning) {
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
        break;
      default:
        warning(
          "`alike` behavior for objects of type \"%s\" is not well defined and may change in the future",
          type2char(tar_type)
        );
    }
  }
  ALIKEC_res res;
  res.is_df = *is_df;
  if(err) {
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

ALIKEC_res ALIKEC_alike_rec(
  SEXP target, SEXP current, SEXP index, int int_mode, double int_tolerance,
  int attr_mode
) {
  // normal logic, which will have checked length and attributes, etc.

  ALIKEC_res res0 = ALIKEC_alike_obj(
    target, current, int_mode, int_tolerance, attr_mode
  );
  if(!res_obj.success) {
    return (ALIKEC_alike_rec) {0, res_obj.message, 0};
  }
  // Recurse

  ALIKEC_res1 res1;

  if(tar_type == VECSXP) {
    R_xlen_t i;
    SEXP vec_names = GET_NAMES(target);

    for(i = 0; i < tar_len; i++) {
      if(vec_names == R_NilValue) SETCDR(index, CONS(ScalarInteger(i)));
      else SETCDR(index, CONS(STRING_ELT(vec_names, i)));
      res1 = ALIKEC_alike_rec(
        VECTOR_ELT(target, i), VECTOR_ELT(current, i), CDR(index), int_mode,
        int_tolerance, attr_mode
      )
      if(!res1.success) return(res1);
    }
  } else if (tar_type == ENVSXP) {
    SEXP tar_names = PROTECT(R_lsInternal(target));
    R_xlen_t = tar_name_len = XLENTGH(tar_names);
    if(tar_name_len != tar_len)
      error("Logic Error: mismatching env lengths; contact maintainer")
    for(i = 0; i < tar_len; i++) {
      SEXP var_name = PROTECT(install(STRING_ELT(tar_names, i)));
      SEXP var_cur_val = findVarInFrame(current, var_name);
      if(var_cur_val == R_UnboundValue) {
        res1.success = 0;
        res1.message = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "contain variable `%s`",
          asChar(STRING_ELT(tar_names, i)), "", "", ""
        );
        break;
      }
      SETCDR(index, CONS(STRING_ELT(tar_names, i)));
      res1 = ALIKEC_alike_rec(
        findVarInFrame(target, var_name), var_cur_val, CDR(index), int_mode,
        int_tolerance, attr_mode
      )
      if(!res1.success) break;
    }
  } else if (tar_type == LISTSXP) {
    SEXP tar_sub, cur_sub;
    R_xlen_t i = 0;
    for(
      tar_sub = target, cur_sub = current; tar_sub != R_NilValue;
      tar_sub = CDR(tar_sub), car_sub = CDR(tar_sub), i++
    ) {
      // Check tag names; should be in same order??  Probably

      SEXP tar_tag = TAG(tar_sub);
      SEXP tar_tag_chr = PRINTNAME(tar_tag);
      if(tar_tag != R_NilValue && tar_tag != TAG(cur_sub)) {
        res1.message = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "contain element `%s`", asChar(tar_tag_chr),
          "", "", ""
        );
        res1.success = 0;
        break;
      }
      if(tar_tag != R_NilValue) SETCDR(index, CONS(ScalarString(tar_tag_chr)));
      else SETCDR(index, CONS(ScalarInteger(i)));
      res1 = ALIKEC_alike_rec(
        CAR(tar_sub), CAR(cur_sub), CDR(index), int_mode,
        int_tolerance, attr_mode
      )
      if(!res1.success) break;
    }
  }
  res1.df = res0.df;  // Indicate whether parent object was a data frame
  return res1;
}
/*

*/

SEXP ALIKEC_alike_internal(
  SEXP target, SEXP current, int int_mode, double int_tolerance, int attr_mode,
  const char * prepend
) {
  if(int_mode < 0 || int_mode > 2)
    error("Argument `int.mode` must be in 0:2");
  if(attr_mode < 0 || attr_mode > 2)
    error("Argument `attr.mode` must be in 0:2");
  const char * err_base;
  SEXP index = CONS(R_NilValue);

  if(TYPEOF(target) == NILSXP && TYPEOF(current) != NILSXP) {
    // Handle NULL special case at top level

    err_base = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "be NULL is %s", type2char(TYPEOF(current))
    )

  } else {
    // Recursively check object

    ALIKEC_res res = ALIKEC_alike_rec(
      target, current, index, int_mode, int_tolerance, attr_mode
    )
    if(res.success) return(ScalarLogical(1));
  }
  char * err_final, * err_chr_index;
  char * err_msg;
  err_msg = CSR_smprintf4(
    ALIKEC_MAX_CHAR, err_base, err_tok1, err_tok2, err_tok3,
    err_tok4
  );
  /*
  Compute the part of the error that gives the index where the discrepancy
  occurred.
  */

  if(CDR(index) == R_NilValue) {  // No recursion occurred
    err_final = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "%s%s%s%s", prepend, (const char *) err_msg, "", ""
    );
  } else {
    // Scan through all indices to calculate size of required vector

    SEXP ind_sub;
    SEXP ind_sub_val;
    size_t err_size = 0;
    size_t levels = 0;

    for(ind_sub = index; ind_sub != R_NilValue; ind_sub = CDR(ind_sub), levels++) {
      ind_sub_val = CAR(ind_sub)
      switch(TYPEOF(ind_sub_val)) {
        INTSXP:
          err_size += CSR_len_chr_len(asInteger(ind_sub_val));
          break;
        STRSXP:
          err_size += CSR_strmlen(CHAR(asChar(ind_sub)), ALIKEC_MAX_CHAR) + 2;
          break;
        default:
          error("Logic Error: unexpected index type; contact maintainer.");
    } }
    err_chr_index = (char *) R_alloc(err_size + 2 * levels + 1, sizeof(char));

    for(i = 0; i < ind_lvl - 1; i++) {
      sprintf(err_chr_index, "[[%d]]", ind_stk[i] + 1);
      strcat(err_chr_indeces, err_chr_index);
    }




  }

  int ind_len = 0;
  int ind_sz = 0;
  int ind_sz_max = 0;

  for(i = 0; i < ind_lvl; i++) {
    if((ind_sz = CSR_len_chr_len((R_xlen_t)ind_stk[i])) > ind_sz_max)
      ind_sz_max = ind_sz;  /* we will use to allocate a char vec of appropriate size */
    ind_len = ind_len + ind_sz;
  }
  char * err_chr_indeces = R_alloc(ind_len + 4 * (ind_lvl + 1) + 1, sizeof(char));  //err_chr_indeces should be over-allocated by one element due to some changes we made
  char * err_chr_index = R_alloc(ind_sz_max + 4 + 1, sizeof(char));
  err_chr_indeces[0] = err_chr_index[0] = '\0';


  for(i = 0; i < ind_lvl - 1; i++) {
    sprintf(err_chr_index, "[[%d]]", ind_stk[i] + 1);
    strcat(err_chr_indeces, err_chr_index);
  }
  /* Create final error and store in STRSXP */

  if(ind_lvl > 0) {
    const char * err_interim, * err_col_type;

    if(  // Dealing with the column of a data frame like object
      was_df && TYPEOF(sxp_stk_tar[ind_lvl - 1]) == VECSXP
    ) {
      err_col_type = "column";
      SEXP col_names = getAttrib(sxp_stk_tar[ind_lvl - 1], R_NamesSymbol);
      if(
        col_names != R_NilValue && XLENGTH(col_names) > ind_stk[i]
      ) {
        err_interim = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "`%s`",
          CHAR(asChar(STRING_ELT(col_names, ind_stk[i]))), "", "", ""
        );
      } else {
        err_interim = CSR_len_as_chr(ind_stk[i] + 1);
      }
    } else {
      err_col_type = "index";
      err_interim = CSR_smprintf4(
        ALIKEC_MAX_CHAR, "%s[[%s]]", err_chr_indeces,
        CSR_len_as_chr(ind_stk[i] + 1), "", ""
    );}
    err_final = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "%s%s at %s %s", prepend,
      err_msg, err_col_type, err_interim
    );
  } else {

  }
  SEXP res;
  res = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(res, 0, mkChar(err_final));
  UNPROTECT(1);
  return res;



  return R_NilValue;
}


SEXP ALIKEC_alike_internal(
  SEXP target, SEXP current, int int_mode, double int_tolerance, int attr_mode,
  const char * prepend
) {
  if(int_mode < 0 || int_mode > 2)
    error("Argument `int.mode` must be in 0:2");
  if(attr_mode < 0 || attr_mode > 2)
    error("Argument `attr.mode` must be in 0:2");

  /* General algorithm here is to:
      - Check whether objects are equal
      1. If they are, and one is list, dive into first element
      - Keep diving until hit something that isn't a list
      2. Walk to next item in item before, and go back to 1.
      - If no more items in list, then go to .2
    This should guarantee that we visit every element.

    Note, need to check performance of this vs. recursive version as it is
    now clearly apparent that the bulk of the overhead here is associated with the
    C/R interface rather than the recursion, and getting the iterative version
    to work when exploring the tree was definitely an exercise in pretzel making.

    Flipside here is that we would need to recurse in parallel over two objects,
    so maybe that adaptation is not that simple.
  */

  int ind_lvl = 0;          /* how deep in the stack we are */
  int ind_lvl_max = -1;     /* deepest we've gone in current stack */
  int ind_stk_sz = 32;      /* current size of stack */
  int ind_stk[ind_stk_sz];  /* track the stack */
  R_xlen_t tar_len, cur_len, tar_first_el_len, cur_first_el_len;
  SEXP sxp_stk_tar[ind_stk_sz]; /* track the parent objects */
  SEXP sxp_stk_cur[ind_stk_sz]; /* track the parent objects */
  int emr_brk = 0;
  int i;
  int was_df;    // whether prior loop was in a data.frame
  int tmp = 0;
  int * is_df = &tmp;
  SEXPTYPE tar_type;
  SEXP tar_first_el, cur_first_el;

  int err = 0;
  const char * err_base, * err_tok1, * err_tok2, * err_tok3, * err_tok4,
    * err_type, * err_attr;

  /* Initialize Object Tracking Stack */

  sxp_stk_tar[0] = target;
  sxp_stk_cur[0] = current;

  while(TRUE) {

    if(ind_lvl_max < ind_lvl) { /* need to initialize current stack position */
      ind_stk[ind_lvl] = 0;
      ind_lvl_max = ind_lvl;
    }
    if(ind_lvl >= ind_stk_sz) {
      error("Exceeded Stack Size");  /* placeholder, should re-allocate stack */
    }
    if(ind_lvl < 0)
      error("Logic Error, level drop not detected");
    if(emr_brk++ > 50)
      error("Infininte Loop?");
    if (ind_lvl < 0) {
      error("Negative Stack Index"); /* genuine error */
    }
    /*
    Generate error messages to return as character vector; error message
    structure will be to have some base line message and then two components
    showing the target expectation and the actual value.  Most of what is going
    on here is getting everything in the right format for the final error
    message.  Things to know:

    err_msgs is a constant array with all the error messages, indexed by
    `err_type`
    */

    // - S4 --------------------------------------------------------------------

    // Don't run custom checks for S4 objects, just use inherits

    tar_type = TYPEOF(target);
    int s4_cur, s4_tar;
    s4_tar = ((IS_S4_OBJECT)(target) != 0);
    s4_cur = ((IS_S4_OBJECT)(current) != 0);
    if(!err && (s4_cur || s4_tar)) {  // don't run length or attribute checks on S4
      if(s4_tar + s4_cur == 1) {
        err = 1;
        err_base = "%sbe S4";
        err_tok1 = (s4_tar ? "" : "not ");
        err_tok2 = "";
        err_tok3 = err_tok4 = "";
      } else {
        // Here we pull the class symbol, install "inherits" from R proper, and
        // evaluate it in the base environment to avoid conflicts with other
        // symbols

        SEXP klass, klass_attrib;
        SEXP s, t;

        klass = getAttrib(target, R_ClassSymbol);
        if(xlength(klass) != 1L || TYPEOF(klass) != STRSXP)
          error("Logic Error: unexpected S4 class \"class\" attribute of length != 1 or type not character vector; contact package maintainer");
        klass_attrib = getAttrib(klass, ALIKEC_SYM_package);
        if(xlength(klass_attrib) != 1L || TYPEOF(klass_attrib) != STRSXP)
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
          err_tok3 = err_tok4 = "";
        }
        UNPROTECT(1);
      }
    } else if(target != R_NilValue ||ind_lvl < 1) {  // Nil objects match anything when nested
    // - Attributes ------------------------------------------------------------
      int tmp = -1;
      int * err_lvl =& tmp;

      if (
        !err &&
        strlen(
          err_attr = ALIKEC_compare_attributes_internal(
            target, current, attr_mode, is_df, err_lvl
        ) )
      ) {
        err = 1;
        err_base = err_attr;
        err_tok1 = err_tok2 = err_tok3 = err_tok4 = "";
      }
    // - Type ------------------------------------------------------------------

      if(
        !err &&
        strlen(
          err_type = ALIKEC_type_alike_internal(
            target, current, int_mode, int_tolerance
        ) )
      ) {
        err = 1;
        err_base = err_type;
        err_tok1 = err_tok2 = err_tok3 = err_tok4 = "";
      }

    // - Length ----------------------------------------------------------------

      if(
        (!err || (*is_df && *err_lvl > 0))  &&   // if attribute error is not class, override with col count error
        (tar_len = xlength(target)) > 0 &&       /* zero lengths match any length */
        tar_len != (cur_len = xlength(current))
      ) {
        err = 1;
        if(*is_df) {
          err_base = CSR_smprintf4(
            ALIKEC_MAX_CHAR, "have %%s column%s (has %%s)",
            err_tok2 = tar_len == (R_xlen_t) 1 ? "" : "s", "", "", ""
          );
        } else {
          err_base = "be length %s (is %s)";
        }
        err_tok1 = CSR_len_as_chr(tar_len);
        err_tok2 = CSR_len_as_chr(cur_len);
        err_tok3 = err_tok4 = "";
      } else if (
        *is_df && *err_lvl > 0 && tar_type == VECSXP && XLENGTH(target) &&
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
      }
    }
    // - Known Limitations -----------------------------------------------------

    switch(tar_type) {
      case NILSXP:
      case LGLSXP:
      case INTSXP:
      case REALSXP:
      case CPLXSXP:
      case STRSXP:
      case VECSXP:
      case S4SXP:
        break;
      default:
          warning(
            "`alike` behavior for objects of type \"%s\" is not well defined and may change in the future",
            type2char(tar_type)
          );
    }
    // - Handle Errors ---------------------------------------------------------

    if(err) {
      const char * err_final;
      char * err_msg;
      err_msg = CSR_smprintf4(
        ALIKEC_MAX_CHAR, err_base, err_tok1, err_tok2, err_tok3,
        err_tok4
      );
      /*
      Compute the part of the error that gives the index where the discrepancy
      occurred.  Note that last level is meaningless as it has not been dived
      into yet so we purposefully ignore it with `i < ind_lvl`
      */
      int ind_len = 0;
      int ind_sz = 0;
      int ind_sz_max = 0;

      for(i = 0; i < ind_lvl; i++) {
        if((ind_sz = CSR_len_chr_len((R_xlen_t)ind_stk[i])) > ind_sz_max)
          ind_sz_max = ind_sz;  /* we will use to allocate a char vec of appropriate size */
        ind_len = ind_len + ind_sz;
      }
      char * err_chr_indeces = R_alloc(ind_len + 4 * (ind_lvl + 1) + 1, sizeof(char));  //err_chr_indeces should be over-allocated by one element due to some changes we made
      char * err_chr_index = R_alloc(ind_sz_max + 4 + 1, sizeof(char));
      err_chr_indeces[0] = err_chr_index[0] = '\0';
      for(i = 0; i < ind_lvl - 1; i++) {
        sprintf(err_chr_index, "[[%d]]", ind_stk[i] + 1);
        strcat(err_chr_indeces, err_chr_index);
      }
      /* Create final error and store in STRSXP */

      if(ind_lvl > 0) {
        const char * err_interim, * err_col_type;

        if(  // Dealing with the column of a data frame like object
          was_df && TYPEOF(sxp_stk_tar[ind_lvl - 1]) == VECSXP
        ) {
          err_col_type = "column";
          SEXP col_names = getAttrib(sxp_stk_tar[ind_lvl - 1], R_NamesSymbol);
          if(
            col_names != R_NilValue && XLENGTH(col_names) > ind_stk[i]
          ) {
            err_interim = CSR_smprintf4(
              ALIKEC_MAX_CHAR, "`%s`",
              CHAR(asChar(STRING_ELT(col_names, ind_stk[i]))), "", "", ""
            );
          } else {
            err_interim = CSR_len_as_chr(ind_stk[i] + 1);
          }
        } else {
          err_col_type = "index";
          err_interim = CSR_smprintf4(
            ALIKEC_MAX_CHAR, "%s[[%s]]", err_chr_indeces,
            CSR_len_as_chr(ind_stk[i] + 1), "", ""
        );}
        err_final = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "%s%s at %s %s", prepend,
          err_msg, err_col_type, err_interim
        );
      } else {
        err_final = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "%s%s%s%s", prepend, (const char *) err_msg, "", ""
        );
      }
      SEXP res;
      res = PROTECT(allocVector(STRSXP, 1));
      SET_STRING_ELT(res, 0, mkChar(err_final));
      UNPROTECT(1);
      return res;
    }
    // - Get Next Elements -----------------------------------------------------

    /* If object list, then dive in */

    if(tar_type == VECSXP) {
      if(ind_stk[ind_lvl] + 1 > length(target)) { /* no sub-items to check */
        if(ind_lvl <= 0)
          break;
        target = sxp_stk_tar[ind_lvl - 1];
        current = sxp_stk_cur[ind_lvl - 1];
        ind_stk[ind_lvl - 1]++;     /* Since we completed list, parent pointer should be moved forward to review next */
        ind_stk[ind_lvl] = 0;       /* Need to reset pointer before we leave this level as next time we get here it will be with a fresh list */
        ind_lvl--;
        ind_lvl_max--;
      } else {
        sxp_stk_tar[ind_lvl] = target;
        sxp_stk_cur[ind_lvl] = current;
        target = VECTOR_ELT(target, ind_stk[ind_lvl]);
        current = VECTOR_ELT(current, ind_stk[ind_lvl]);
        ind_lvl++;
      }
    } else if (tar_type == ENVSXP) {


    } else {
      if(ind_lvl <= 0)
        break;
      target = sxp_stk_tar[ind_lvl - 1];
      current = sxp_stk_cur[ind_lvl - 1];
      ind_stk[ind_lvl - 1]++;
      ind_lvl--;
    }
    was_df = *is_df;
  }
  // - Finalize ----------------------------------------------------------------

  SEXP res;
  res = PROTECT(allocVector(LGLSXP, 1));
  LOGICAL(res)[0] = 1;
  UNPROTECT(1);
  return res;
}
/* "fast" version doesn't allow messing with optional parameters to avoid arg
evaluations in R; note that S4 tests will be done by evaluating `inherits` in
R_GlobalEnv, which should be fine since all the other arguments to `alike` have
already been evaluated, but the standard `alike` function evaluates it in the
calling environment */

SEXP ALIKEC_alike_fast(SEXP target, SEXP current) {
  return ALIKEC_alike_internal(
    target, current, 0, sqrt(DOUBLE_EPS), 0, "should "
);}
/* Normal version, a little slower but more flexible */

SEXP ALIKEC_alike (
  SEXP target, SEXP current, SEXP int_mode, SEXP int_tolerance, SEXP attr_mode
) {
  SEXPTYPE int_mod_type, tol_type, attr_mod_type;

  int_mod_type = ALIKEC_typeof_internal(int_mode, sqrt(DOUBLE_EPS));
  attr_mod_type = ALIKEC_typeof_internal(attr_mode, sqrt(DOUBLE_EPS));
  tol_type = ALIKEC_typeof_internal(int_tolerance, sqrt(DOUBLE_EPS));

  if(int_mod_type != INTSXP || XLENGTH(int_mode) != 1)   /* borrowed code from type_alike, maybe needs to be function */
    error("Argument `int_mode` must be a one length integer like vector");
  if(attr_mod_type != INTSXP || XLENGTH(attr_mode) != 1)   /* borrowed code from type_alike, maybe needs to be function */
    error("Argument `attr_mode` must be a one length integer like vector");
  if((tol_type != INTSXP && tol_type != REALSXP) || XLENGTH(int_tolerance) != 1)
    error("Argument `int_tolerance` must be a one length numeric vector");

  return ALIKEC_alike_internal(
    target, current, asInteger(int_mode), asReal(int_tolerance),
    asInteger(attr_mode), "should "
  );
}
