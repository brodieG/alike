#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* - Setup ------------------------------------------------------------------ */

SEXP ALIKEC_alike (SEXP target, SEXP current, SEXP int_mode, SEXP int_tol, SEXP class_mode, SEXP attr_mode);
SEXP ALIKEC_alike_fast (SEXP target, SEXP current);
SEXP ALIKEC_typeof(SEXP object, SEXP tolerance);
SEXP ALIKEC_typeof_fast(SEXP object);
SEXP ALIKEC_type_alike(SEXP target, SEXP current, SEXP mode, SEXP tolerance);
SEXP ALIKEC_type_alike_fast(SEXP target, SEXP current);
SEXPTYPE ALIKEC_typeof_internal(SEXP object, double tolerance);
SEXP ALIKEC_type_alike_internal(SEXP target, SEXP current, int mode, double tolerance);

static const
R_CallMethodDef callMethods[] = {
  {"alike2", (DL_FUNC) &ALIKEC_alike, 6},
  {"alike2_fast", (DL_FUNC) &ALIKEC_alike_fast, 2},
  {"typeof2", (DL_FUNC) &ALIKEC_typeof, 2},
  {"typeof2_fast", (DL_FUNC) &ALIKEC_typeof_fast, 1},
  {"type_alike2", (DL_FUNC) &ALIKEC_type_alike, 4},
  {"type_alike2_fast", (DL_FUNC) &ALIKEC_type_alike_fast, 2},
  NULL 
};

void R_init_alike(DllInfo *info)
{
 /* Register the .C and .Call routines.
    No .Fortran() or .External() routines,
    so pass those arrays as NULL.
  */
  R_registerRoutines(info,
  NULL, callMethods,
  NULL, NULL);
}
/* - type_alike ------------------------------------------------------------- */


SEXP ALIKEC_type_alike_internal(SEXP target, SEXP current, int mode, double tolerance) {
  SEXPTYPE tar_type, cur_type;
  int res = 0;
  switch(mode) {
    case 0:
      tar_type = ALIKEC_typeof_internal(target, tolerance);
      cur_type = ALIKEC_typeof_internal(current, tolerance);
      break;
    case 1:
    case 2:
      tar_type = TYPEOF(target);
      cur_type = TYPEOF(current);
      break;
    default:
      error("Logic Error: unexpected type comparison mode %d\n", mode);
  }
  if(
    cur_type == INTSXP && (mode == 0 || mode == 1) && 
    (tar_type == INTSXP || tar_type == REALSXP)
  ) {
    res = 1;
  } else {
    res = cur_type == tar_type;
  }
  return ScalarLogical(res);
}
SEXP ALIKEC_type_alike(SEXP target, SEXP current, SEXP mode, SEXP tolerance) {
  SEXPTYPE mod_type, tol_type;
  int mode_val;
  double tol_val;
  
  mod_type = ALIKEC_typeof_internal(mode, sqrt(DOUBLE_EPS));
  tol_type = ALIKEC_typeof_internal(tolerance, sqrt(DOUBLE_EPS));
  
  if(mod_type != INTSXP || XLENGTH(mode) != 1) 
    error("Argument `mode` must be a one length integer like vector");
  if(tol_type != INTSXP && tol_type != REALSXP || XLENGTH(tolerance) != 1) 
    error("Argument `tolerance` must be a one length numeric vector");
  
  return ALIKEC_type_alike_internal(target, current, asInteger(mode), asReal(tolerance));
}
SEXP ALIKEC_type_alike_fast(SEXP target, SEXP current) {
  return ALIKEC_type_alike_internal(target, current, 0, sqrt(DOUBLE_EPS));
}

/* - typeof ----------------------------------------------------------------- */

SEXPTYPE ALIKEC_typeof_internal(SEXP object, double tolerance) {
  int mode_val, obj_len = XLENGTH(object), i, * obj_int, items=0, finite;
  double * obj_real, int_tol_val, obj_len_x=XLENGTH(object), diff_abs=0, val=0, flr;
  SEXPTYPE obj_type;

  if((obj_type = TYPEOF(object)) == REALSXP) {    
    obj_real = REAL(object);

    for(i = 0; i < obj_len; i++) {
      if(
        !isnan(obj_real[i]) && (finite = isfinite(obj_real[i])) && 
        obj_real[i] != (flr = floor(obj_real[i]))
      ) {
        items = items + 1;
        diff_abs = diff_abs + fabs((obj_real[i] - flr) / obj_real[i]);
        val = val + fabs(obj_real[i]);
      } else if (!finite) return REALSXP;
    }
    if(items > 0 && val / items > tolerance && diff_abs / items > tolerance) {
      return REALSXP;
    } else {
      return INTSXP;
  } }
  return(obj_type);
}
/* 
External interface for typeof, here mostly so we don't have to deal with the
SEXP return in the internal use case
*/

SEXP ALIKEC_typeof(SEXP object, SEXP tolerance) {
  SEXPTYPE obj_type;
  SEXP res;

  if(TYPEOF(tolerance) != REALSXP || XLENGTH(tolerance) != 1L)
    error("Argument tolerance should be a one length numeric vector");

  return mkString(type2char(ALIKEC_typeof_internal(object, REAL(tolerance)[0])));
}
SEXP ALIKEC_typeof_fast(SEXP object) {
  return mkString(type2char(ALIKEC_typeof_internal(object, sqrt(DOUBLE_EPS))));
}

// - Helper Functions ----------------------------------------------------------

/*
Returns a character pointer to the string representation of the integer; allocates
with R_alloc so in theory don't need to worry about freeing memory
*/

char * ALIKEC_int_to_char(int a) {
  int int_len = (int) ceil(log10(abs(a) + 1.00001));  // + 1.00001 to account for 0
  char * res;
  res = R_alloc(int_len + 1, sizeof(char));
  sprintf(res, "%d", a);
  return res;
}
/* Returns a character pointer containing the results of using `a` as the parent
string and all the others a substrings with `sprintf`

note:
- `a` must contain exactly four unescaped "%s"; this will break ugly if it doesn't
*/

char * ALIKEC_sprintf(*char a, *char b, *char c, *char d, *char e) {
  int full_len = strlen(a) + strlen(b) + strlen(c) + strlen(d) + strlen(e) - 8 + 1;
  char * res;
  res = R_alloc(full_len, sizeof(char));
  sprintf(res, a, b, c, d, e);
  res;
}

/* - alike ------------------------------------------------------------------ */

/* Estimate how many characters an integer can be represented with */

int ALIKEC_int_charlen (int a) {
  return (int) ceil(log10(a + 1.1));
}

/* Initial benchmarks (before attr checking):

> microbenchmark(alike2(lst, lst.2), alike(lst, lst.2), .alike2(lst, lst.2))

> lst <-   list(list( 1,  2), list( 3, list( 4, list( 5, list(6, 6.1, 6.2)))))
> lst.2 <- list(list(11, 21), list(31, list(41, list(51, list(61          )))))

Unit: microseconds
                expr      min        lq    median       uq      max neval
  alike2(lst, lst.2)    5.644    6.7105    8.4745   11.085   19.995   100
   alike(lst, lst.2) 1106.624 1120.6535 1133.5815 1159.470 2245.159   100
 .alike2(lst, lst.2)    4.012    4.5560    5.4650    7.905   66.953   100
> microbenchmark(alike2(lst, lst), alike(lst, lst), .alike2(lst, lst))
Unit: microseconds
              expr      min        lq    median        uq      max neval
  alike2(lst, lst)    3.850    4.7085    6.7295    9.8175   22.973   100
   alike(lst, lst) 2762.135 2823.9385 2865.7025 2957.9535 5773.793   100
 .alike2(lst, lst)    2.235    2.7315    3.3835    5.8075   12.835   100
*/



SEXP ALIKEC_alike_internal(
  SEXP target, SEXP current, int int_mode, double int_tolerance, attr_mode
) {

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
 
  int ind_val = 0;          /* current index value */
  int ind_lvl = 0;          /* how deep in the stack we are */
  int ind_lvl_max = -1;     /* deepest we've gone in current stack */
  int ind_stk_sz = 32;      /* current size of stack */
  int ind_stk[ind_stk_sz];  /* track the stack */
  int tar_len;
  SEXP sxp_stk_tar[ind_stk_sz]; /* track the parent objects */
  SEXP sxp_stk_cur[ind_stk_sz]; /* track the parent objects */
  int emr_brk = 0;
  int i, k=0;
  SEXPTYPE cur_type, tar_type;
  SEXP sxp_err;

  /* Define error message; need to figure out how to move this out of here */

  const char * err_msgs[2];
  err_msgs[0] = "Type mismatch, expected %s but got %s";
  err_msgs[1] = "Length mismatch, expected %s but got %s";
  int err_type = -1;
  int err_len = 0;
  char * err_tar;
  char * err_cur;
  
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
    
    // - Type ------------------------------------------------------------------

    if(
      (tar_type = ALIKEC_typeof_internal(target, int_tolerance)) != 
      (cur_type = ALIKEC_typeof_internal(current, int_tolerance))
    ) {      
      err_type = 0;
      err_tar = R_alloc(strlen(type2char(tar_type)) + 1, sizeof(char));
      err_cur = R_alloc(strlen(type2char(cur_type)) + 1, sizeof(char));
      strcpy(err_tar, type2char(tar_type));
      strcpy(err_cur, type2char(cur_type));

    // - Length ----------------------------------------------------------------

    } else if(
      tar_type == VECSXP && (tar_len = length(target)) > 0 &&  /* zero lengths match any length */
      tar_len != length(current)
    ) {
      err_type = 1;
      err_tar = R_alloc(ALIKEC_int_charlen(length(target)) + 1, sizeof(char));
      err_cur = R_alloc(ALIKEC_int_charlen(length(current)) + 1, sizeof(char));
      sprintf(err_tar, "%d", length(target));
      sprintf(err_cur, "%d", length(current));      

    // - Attributes ------------------------------------------------------------
    //
    // Code lifted from `R_compute_identical`

    } else {


      if(tar_attr != R_NilValue || mode == 2) {


        if(TYPE(tar_attr) != )  

      }

      if(
      (tar_attr = ATTRIB(target)) != R_NilValue && mode == 0
    ) {  
      cur_attr = ATTRIB(current);



      ax = ATTRIB(x); ay = ATTRIB(y);
      if (!ATTR_AS_SET) {
    if(!R_compute_identical(ax, ay, flags)) return FALSE;
      }
      /* Attributes are special: they should be tagged pairlists.  We
         don't test them if they are not, and we do not test the order
         if they are.

         This code is not very efficient, but then neither is using
         pairlists for attributes.  If long attribute lists become more
         common (and they are used for S4 slots) we should store them in
         a hash table.
      */
      else if(ax != R_NilValue || ay != R_NilValue) {
    if(ax == R_NilValue || ay == R_NilValue)
        return FALSE;
    if(TYPEOF(ax) != LISTSXP || TYPEOF(ay) != LISTSXP) {
        warning(_("ignoring non-pairlist attributes"));
    } else {
        SEXP elx, ely;
        if(length(ax) != length(ay)) return FALSE;
        /* They are the same length and should have
           unique non-empty non-NA tags */



    }
    // - Handle Errors ---------------------------------------------------------

    if(err_type > -1) {
      err_len = strlen(err_msgs[err_type]) + strlen(err_tar) + strlen(err_cur) - 4; /* -4 because we have 2 %s in the message */
      char err_base[err_len + 1];
      sprintf(err_base, err_msgs[err_type], err_tar, err_cur);

      int ind_len = 0;
      int ind_sz = 0; 
      int ind_sz_max = 0; 

      /*
      Compute the part of the error that gives the index where the discrepancy
      occurred.  Note that last level is meaningless as it has not been dived 
      into yet so we purposefully ignore it with `i < ind_lvl`
      */

      for(i = 0; i < ind_lvl; i++) { 
        if((ind_sz = ALIKEC_int_charlen(ind_stk[i])) > ind_sz_max)
          ind_sz_max = ind_sz;  /* we will use to allocate a char vec of appropriate size */
        ind_len = ind_len + ind_sz;
      }
      char err_chr_indeces[ind_len + 4 * (ind_lvl + 1) + 1];
      char err_chr_index[ind_sz_max + 4 + 1];
      err_chr_indeces[0] = err_chr_index[0] = '\0';
      for(i = 0; i < ind_lvl; i++) {
        sprintf(err_chr_index, "[[%d]]", ind_stk[i] + 1);
        strcat(err_chr_indeces, err_chr_index);
      }
      /* Create final error and store in STRSXP */

      sxp_err = PROTECT(allocVector(STRSXP, 1));
      char err_final[err_len + strlen(err_chr_indeces) + 10]; 
      if(ind_lvl > 0) {
        sprintf(err_final, "%s at index %s", err_base, err_chr_indeces);
      } else {
        sprintf(err_final, "%s", err_base);
      }
      SET_STRING_ELT(sxp_err, 0, mkChar(err_final));
      UNPROTECT(1);
      return sxp_err;
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
  }
  // - Finalize ----------------------------------------------------------------

  SEXP res;
  res = PROTECT(allocVector(LGLSXP, 1));
  LOGICAL(res)[0] = 1;
  UNPROTECT(1);
  return res;  
}
SEXP ALIKEC_alike_fast(SEXP target, SEXP current) {
  return ALIKEC_alike_internal(target, current, 0, sqrt(DOUBLE_EPS), 0, 0);
}
SEXP ALIKEC_alike (
  SEXP target, SEXP current, SEXP int_mode, SEXP int_tolerance, 
  SEXP class_mode, SEXP attr_mode 
) {
  SEXPTYPE int_mod_type, tol_type, class_mod_type, attr_mod_type;
  
  int_mod_type = ALIKEC_typeof_internal(int_mode, sqrt(DOUBLE_EPS));
  class_mod_type = ALIKEC_typeof_internal(class_mode, sqrt(DOUBLE_EPS));
  attr_mod_type = ALIKEC_typeof_internal(attr_mode, sqrt(DOUBLE_EPS));
  tol_type = ALIKEC_typeof_internal(int_tolerance, sqrt(DOUBLE_EPS));
  
  if(int_mod_type != INTSXP || XLENGTH(int_mode) != 1)   /* borrowed code from type_alike, maybe needs to be function */
    error("Argument `int_mode` must be a one length integer like vector");
  if(class_mod_type != INTSXP || XLENGTH(class_mode) != 1)   /* borrowed code from type_alike, maybe needs to be function */
    error("Argument `class_mode` must be a one length integer like vector");
  if(attr_mod_type != INTSXP || XLENGTH(attr_mode) != 1)   /* borrowed code from type_alike, maybe needs to be function */
    error("Argument `attr_mode` must be a one length integer like vector");
  if(tol_type != INTSXP && tol_type != REALSXP || XLENGTH(int_tolerance) != 1) 
    error("Argument `int_tolerance` must be a one length numeric vector");

  return 
    ALIKEC_alike_internal(
      target, current, asInteger(int_mode), asReal(int_tolerance), 
      asInteger(class_mode), asInteger(attr_mode)
    );
}

/* Used by alike to compare attributes; returns pointer to zero length character
string if successful, an error message otherwise*/

char * ALIKEC_compare_attributes(target SEXP, current SEXP, int attr_mode) {
  
  SEXP tar_attr, cur_attr, tar_attr_el, cur_attr_el, tar_attr_el_val, cur_attr_el_val;
  SEXPTYPE tar_attr_el_val_type;
  int curr_attr_len, tar_attr_len, tar_attr_el_val_len, attr_i, tar_dim_val,
    attr_match;
  
  tar_attr = ATTRIB(target);
  cur_attr = ATTRIB(current);

  if(
    (tar_attr != R_NilValue || attr_mode == 2) && 
    !(tar_attr == R_NilValue && cur_attr == R_NilValue)
  ) {
    if(attr_mode == 2 && tar_attr == R_NilValue && cur_attr != R_NilValue)
      return "target and current must have identical attributes";
    // There must be attributes on both target and current
    if(TYPEOF(tar_attr) != LISTSXP || TYPEOF(tar_attr) != LISTSXP) {
      warning(_("ignoring non-pairlist attributes"));
    } else {
      tar_attr_len = length(tar_attr);
      curr_attr_len = length(cur_attr);
      
      if(attr_mode == 2 && tar_attr_len != curr_attr_len) {
        return "target and current must have same number of attributes";
      } else if (tar_attr_len > curr_attr_len) {
        return "current must have all the attributes that target has";
      } else {
        // Loop through all attr combinations; maybe could be made faster by
        // reducing the second loop each time a match is found

        for(tar_attr_el = tar_attr; tar_attr_el != R_NilValue; tar_attr_el = CDR(tar_attr_el)) {
          const char *tx = CHAR(PRINTNAME(TAG(tar_attr_el)));
          
          attr_match = 0;  // Track whether an attribute was matched or not
          for(cur_attr_el = cur_attr; cur_attr_el != R_NilValue; cur_attr_el = CDR(cur_attr_el)) {
            if(streql(tx, CHAR(PRINTNAME(TAG(cur_attr_el))))) {

              attr_match = 1;  // Attribute was matched
              tar_attr_el_val = CAR(tar_attr_el);
              cur_attr_el_val = CAR(cur_attr_el);

              /* We need to treat row.names specially here because of the 
              totally bullshit, well, not truly, way data.frame row names 
              are stored c(NA, n) where "n" is the number of rows; the `getAttrib`
              access expands that to the full sequence; entirely unclear why
              we can't just compare the two `c(NA, n)` and just be done; do
              we really want to allow two row.names attributes that are stored
              differently but compute to the same value to be identical???
              */

              /* not entirely sure what the "default" flag is for `identical`,
              seems to be 16 by the convention that all the flags that are TRUE
              get set to zero, but very confusing why `ignore.environment` basically
              is the opposite of all the others.  Otherwise it would have made
              sense to have the default flag be 0  NEED TO TEST CLOSURE
              COMPARISON!!!*/
              
              /*if(streql(tx, "row.names")) {
                PROTECT(atrx = getAttrib(target, R_RowNamesSymbol));
                PROTECT(atry = getAttrib(current, R_RowNamesSymbol));
                if(!R_compute_identical(atrx, atry, 16)) {
                  UNPROTECT(2);
                  return FALSE;
                } else UNPROTECT(2);
              } else */
              

              // - class ---------------------------------------------------

              /* see R documentation for explanations of how the special 
              attributes class, dim, and dimnames are compared */

              if(streql(tx, "class") && attr_mode == 0) {
                TRUE;
                TRUE;
              // - dim -----------------------------------------------------
              } else if (streql(tx, "dim") && attr_mode == 0) {
                if(
                  (tar_attr_el_val_type = TYPEOF(tar_attr_el_val)) != TYPEOF(tar_attr_el_val) ||
                  tar_attr_el_val_type != INTSXP || 
                  (tar_attr_el_val_len = XLENGTH(tar_attr_el_val)) != XLENGTH(cur_attr_el_val)
                ) {
                  return "`dim` mismatch or non integer dimensions";
                }
                for(attr_i = 0; attr_i < tar_attr_el_val_len; attr_i++) {
                  if(
                    (tar_dim_val = INTEGER(tar_attr_el_val)[attr_i]) && 
                    tar_dim_val != INTEGER(cur_attr_el_val)[attr_i]
                  ) {
                    return ALIKEC_sprintf(
                      "`dim` mismatch at dimension %s: expected %s but got %s%s",
                      ALIKEC_int_to_char(attr_i), ALIKEC_int_to_char(tar_dim_val), 
                      ALIKEC_int_to_char(INTEGER(cur_attr_el_val)[attr_i]), ""
                    );
                } }
              // - dimnames ------------------------------------------------

              } else if (streql(tx, "dimnames") && attr_mode == 0) {
                SEXP tar_attr_el_val_dimnames, tar_attr_el_val_dimnames,
                  tar_attr_el_val_dimname_obj, cur_attr_el_val_dimname_obj;
                SEXPTYPE type_tmp;

                if(
                  (tar_attr_el_val_type = TYPEOF(tar_attr_el_val)) != TYPEOF(tar_attr_el_val) ||
                  tar_attr_el_val_type != VECSXP || 
                  (tar_attr_el_val_len = xlength(tar_attr_el_val)) != xlength(cur_attr_el_val)
                ) {
                  return "`dimnames` mismatch or non-list dimnames";
                } else if (
                  (tar_attr_el_val_dimnames_names = getAttrib(tar_attr_el_val, R_NamesSymbol)) != R_NilValue
                ) {
                  cur_attr_el_val_dimnames_names = getAttrib(cur_attr_el_val, R_NamesSymbol);
                  if(
                    TYPEOF(tar_attr_el_val_dimnames_names) != STRSXP ||
                    !(
                      (type_tmp = TYPEOF(cur_attr_el_val_dimnames_names) == STRSXP) ||
                      type_tmp == R_NilValue
                    )
                  ) {
                    return "Unexpected `dimnames` values; if you are using custom `dimnames` attributes please set `attr_mode` to 1L or 2L";
                  } else if(
                    cur_attr_el_val_dimnames_names == R_NilValue || 
                    XLENGTH(cur_attr_el_val_dimnames_names) != 
                    (tar_attr_el_val_dimnames_names_len = XLENGTH(tar_attr_el_val_dimnames_names))
                  ) {
                    return ALIKEC_sprintf(
                      "`dimnames` mismatch, `target` dimnames does not have expected length %d%s%s%s",
                      ALIKEC_int_to_char(XLENGTH(cur_attr_el_val_dimnames_names)), "", "", ""
                    )
                  } else {
                    for(attr_i = 0; attr_i < tar_attr_el_val_dimnames_names_len; attr_i++) {
                      const char * dimnames_name = CHAR(STRING_ELT(tar_attr_el_val_dimnames_names, attr_i));
                      if(         // check dimnames names match
                        !streql(dimnames_name, "") && 
                        !streql(dimnames_name, CHAR(STRING_ELT(cur_attr_el_val_dimnames_names, attr_i)))
                      ) { 
                        return ALIKEC_sprintf(
                          "`dimnames` name mismatch at dimension %s, expected %s but got %s%s",
                          ALIKEC_int_to_char(attr_i + 1), dimnames_name, 
                          CHAR(STRING_ELT(cur_attr_el_val_dimnames_names, attr_i)),
                          ""
                        );
                      } else if ( // check dimnames match 
                        (
                          tar_attr_el_val_dimname_obj = 
                            VECTOR_ELT(tar_attr_el_val_dimnames, attr_i)
                        ) != R_NilValue
                      ) {
                        cur_attr_el_val_dimname_obj = VECTOR_ELT(cur_attr_el_val_dimnames, attr_i);
                        if(
                          !R_compute_identical(
                            tar_attr_el_val_dimname_obj,
                            cur_attr_el_val_dimname_obj, 16
                          )
                        ) {
                          return ALIKEC_sprintf(
                            "`dimnames` mismatch at dimension %s%s%s%s", 
                            ALIKEC_int_to_char(attr_i), "", "", ""
                          );
                } } } } }
              } else {
                // - Other Attributes --------------------------------------

                if(!R_compute_identical(CAR(tar_attr_el), CAR(cur_attr_el), 16))
                  return ALIKEC_sprintf(
                    "attribute mismatch for attribute `%s`%s%s%s", tx, "", "", ""
                  );
              }
            } 
          }
          if(!attr_match) {
            return ALIKEC_sprintf("attribute %s missing from target%s%s%s\n", tx, "", "", "")
          }
        }
      }
    }
  }
  return "";
}



    /**************************************************************************

    Rprintf(\"At Level %d, index %d, length %d, type %d, length.2 %d, type2 %d\n\", ind_lvl, ind_stk[ind_lvl], length(target), TYPEOF(target), length(current), TYPEOF(current));
    
    for(i=0; i <= ind_lvl_max; i++) {
      Rprintf(\"%d \", ind_stk[i]);
    }
    if(TYPEOF(target) == 14)
      Rprintf(\"Value: %f\", REAL(target)[0]);
    Rprintf(\"\n\");

    /**************************************************************************/
