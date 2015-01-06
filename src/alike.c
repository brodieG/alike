#include "alike.h"

SEXP ALIKEC_alike_internal(
  SEXP target, SEXP current, int int_mode, double int_tolerance, int attr_mode
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
        err_base = "target %s S4 but current %s";
        err_tok1 = (s4_tar ? "is" : "isn't");
        err_tok2 = (s4_cur ? "is" : "isn't");
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
          err_base = "current does not inherit from class \"%s\" (package: %s)";
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
        err_base = "Type mismatch, %s";
        err_tok1 = err_type;
        err_tok2 = err_tok3 = err_tok4 = "";
      }

    // - Length ----------------------------------------------------------------

      if(
        (!err || (*is_df && *err_lvl > 0))  &&   // if attribute error is not class, override with col count error
        (tar_len = xlength(target)) > 0 &&  /* zero lengths match any length */
        tar_len != (cur_len = xlength(current))
      ) {
        err = 1;
        if(*is_df) {
          err_base = "Column count mismatch: expected %s but got %s%s%s";
        } else {
          err_base =  "Length mismatch: expected %s but got %s%s%s";
        }
        err_tok1 = ALIKEC_xlen_to_char(tar_len);
        err_tok2 = ALIKEC_xlen_to_char(cur_len);
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

        err_base = "Row count mismatch: expected %s but got %s%s%s";
        err_tok1 = ALIKEC_xlen_to_char(tar_first_el_len);
        err_tok2 = ALIKEC_xlen_to_char(cur_first_el_len);
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
      const char * err_final, * err_msg;

      err_msg = (const char *) ALIKEC_sprintf((char *) err_base, err_tok1, err_tok2, err_tok3, err_tok4);

      /*
      Compute the part of the error that gives the index where the discrepancy
      occurred.  Note that last level is meaningless as it has not been dived
      into yet so we purposefully ignore it with `i < ind_lvl`
      */
      int ind_len = 0;
      int ind_sz = 0;
      int ind_sz_max = 0;

      for(i = 0; i < ind_lvl; i++) {
        if((ind_sz = ALIKEC_int_charlen((R_xlen_t)ind_stk[i])) > ind_sz_max)
          ind_sz_max = ind_sz;  /* we will use to allocate a char vec of appropriate size */
        ind_len = ind_len + ind_sz;
      }
      char err_chr_indeces[ind_len + 4 * (ind_lvl + 1) + 1];  //err_chr_indeces should be over-allocated by one element due to some changes we made
      char err_chr_index[ind_sz_max + 4 + 1];
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
            err_interim = ALIKEC_sprintf(
              "`%s`", CHAR(asChar(STRING_ELT(col_names, ind_stk[i]))), "", "", ""
            );
          } else {
            err_interim = ALIKEC_xlen_to_char(ind_stk[i] + 1);
          }
        } else {
          err_col_type = "index";
          err_interim = ALIKEC_sprintf(
            "%s[[%s]]", err_chr_indeces,
            ALIKEC_xlen_to_char(ind_stk[i] + 1), "", ""
        );}
        err_final = ALIKEC_sprintf(
          "Mismatch at %s %s: %s%s", err_col_type, err_interim, err_msg, ""
        );
      } else {
        err_final = ALIKEC_sprintf("%s%s%s%s", err_msg, "", "", "");
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
  return ALIKEC_alike_internal(target, current, 0, sqrt(DOUBLE_EPS), 0);
}
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

  return
    ALIKEC_alike_internal(
      target, current, asInteger(int_mode), asReal(int_tolerance),
      asInteger(attr_mode)
    );
}
