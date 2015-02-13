#include "alike.h"

/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Compare class attribute

Note that expectation is that unclassed objects will get sent here with their
implicit class defined by ALIKEC_mode.

Will set prim_is_df to 1 if prim is data frame (or sec if ref==1)
*/
const char * ALIKEC_compare_class(
  SEXP prim, SEXP sec, int rev, int * prim_is_df
) {
  if(rev == NA_INTEGER || rev > 1 || rev < 0) error("Logic Error: `rev` should be 0 or 1.");
  if(prim == R_NilValue || sec == R_NilValue)
    error("Logic Error: NULL classes should not be possible; contact maintainer.");
  if(TYPEOF(sec) != STRSXP || TYPEOF(prim) != STRSXP) {
    return "Class attribute not character vector for both `target` and `current`; if you are using custom \"class\" attributes please set `attr_mode` to 1L or 2L";
  }
  int tar_class_len, cur_class_len, len_delta, tar_class_i, cur_class_i;
  const char * cur_class;
  const char * tar_class;

  if(rev) { // Flip args
    SEXP tmp = prim; prim = sec; sec = tmp;
  }
  tar_class_len = XLENGTH(prim);
  cur_class_len = XLENGTH(sec);
  R_xlen_t class_stop =
    tar_class_len > cur_class_len ? cur_class_len : tar_class_len;
  const char * err_msg = "";
  int err_found = 0;

  len_delta = cur_class_len - class_stop;

  for(
    cur_class_i = len_delta, tar_class_i = 0;
    cur_class_i < cur_class_len;
    cur_class_i++, tar_class_i++
  ) {
    cur_class = CHAR(STRING_ELT(sec, cur_class_i));
    tar_class = CHAR(STRING_ELT(prim, tar_class_i));
    if(!*prim_is_df && !strcmp(tar_class, "data.frame")) *prim_is_df = 1;

    if(!err_found && strcmp(cur_class, tar_class)) { // class mismatch
      err_found = 1;
      if(cur_class_len > 1) {
        char * err_ind = CSR_len_as_chr((R_xlen_t)(cur_class_i + 1));
        err_msg =  CSR_smprintf4(
          ALIKEC_MAX_CHAR,
          "have class \"%s\" at class vector index [[%s]] (is \"%s\", check `class(.)[[%s]]`)",
          tar_class, err_ind, cur_class, err_ind
        );
      } else {
        err_msg =  CSR_smprintf4(
          ALIKEC_MAX_CHAR, "be class \"%s\" (is \"%s\")",
          tar_class, cur_class, "", ""
  );} } }
  if(err_found) return err_msg;
  if(tar_class_len > cur_class_len) {
    return CSR_smprintf4(
      ALIKEC_MAX_CHAR, "inherit from class \"%s\"",
      CHAR(STRING_ELT(prim, tar_class_i)), "", "", ""
  );}
  if(!R_compute_identical(ATTRIB(prim), ATTRIB(sec), 16)) {
    return "have identical \"class\" attributes (check `attributes(class(.))`)";
  }
  return "";
}
SEXP ALIKEC_compare_class_ext(SEXP prim, SEXP sec, SEXP rev) {
  SEXP res = PROTECT(allocVector(VECSXP, 2));
  int tmp = 0;
  int * is_df =& tmp;
  const char * err_msg = ALIKEC_compare_class(prim, sec, asInteger(rev), is_df);
  SET_VECTOR_ELT(res, 0, mkString(err_msg));
  SET_VECTOR_ELT(res, 1, ScalarInteger(*is_df));
  UNPROTECT(1);
  return res;
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Compares dimensions, but detects implicit classes by checking if atomic and
having dimensions and reports error as such if that is the case.  In those
conditions the function will set the `class_err` pointer to 1.
*/
const char * ALIKEC_compare_dims(
  SEXP prim, SEXP sec, SEXP target, SEXP current, int rev, int * class_err
) {
  // Invalid dims

  int type_err = 0;
  if(TYPEOF(prim) != INTSXP) type_err = 1;
  else if(TYPEOF(sec) != INTSXP && sec != R_NilValue) type_err = 2;
  if(type_err) {
    return CSR_smprintf4(
      ALIKEC_MAX_CHAR,
      "have identical \"dim\" attributes since `dim(%s)` is not an integer vector; if you are using custom \"dim\" attributes please set `attr_mode` to 1L or 2L",
      rev ? (type_err == 1 ? "current" : "target") : (type_err == 2 ? "current" : "target"), "", "", ""
  );}
  SEXP prim_obj = rev ? current : target, sec_obj = rev ? target : current;

  // Dims -> implicit class

  R_xlen_t prim_len = XLENGTH(prim), prim_len_cap;
  prim_len_cap = prim_len > (R_xlen_t) 3 ? (R_xlen_t) 3 : prim_len;
  R_xlen_t sec_len = XLENGTH(sec), sec_len_cap;
  sec_len_cap = sec_len > (R_xlen_t) 3 ? (R_xlen_t) 3 : sec_len;

  const char * class_err_string = "";
  const char * class_err_base = "be class \"%s\" (is \"%s\")";

  if(prim_len_cap > 1 && isVectorAtomic(prim_obj)) {
    if(sec == R_NilValue) {  // current is matrix/array
      class_err_string = CSR_smprintf4(
        ALIKEC_MAX_CHAR, class_err_base,
        rev ? CHAR(asChar(ALIKEC_mode(sec_obj))) :
          prim_len_cap > 2 ? "array" : "matrix",
        !rev ? CHAR(asChar(ALIKEC_mode(sec_obj))) :
          prim_len_cap > 2 ? "array" : "matrix", "", ""
      );
    } else if(isVectorAtomic(sec_obj) && sec_len_cap != prim_len_cap) {  // target is matrix/array
      class_err_string = CSR_smprintf4(
        ALIKEC_MAX_CHAR, class_err_base, prim_len_cap > 2 ? "array" : "matrix",
        sec_len_cap == 2 ? "matrix" : (sec_len_cap == 1 ? "vector" : "array"),
        "", ""
      );
    } else if(!isVectorAtomic(sec_obj)) {
      class_err_string = CSR_smprintf4(
        ALIKEC_MAX_CHAR, class_err_base,
        CHAR(asChar(ALIKEC_mode(prim_obj))), type2char(TYPEOF(sec_obj)), "", ""
    );}
  } else if (sec_len_cap > 1 && isVectorAtomic(sec_obj)) {
    if(isVectorAtomic(prim_obj)) {
      class_err_string = CSR_smprintf4(
        ALIKEC_MAX_CHAR, class_err_base, CHAR(asChar(ALIKEC_mode(prim_obj))),
        sec_len_cap == 2 ? "matrix" : "array", "", ""
      );
    } else {
      class_err_string = CSR_smprintf4(
        ALIKEC_MAX_CHAR, class_err_base, CHAR(asChar(ALIKEC_mode(prim_obj))),
        CHAR(asChar(ALIKEC_mode(sec_obj))), "", ""
    );}
  }
  if(strlen(class_err_string)) {
    *class_err = 1;
    return(class_err_string);
  }
  // Normal dim checking

  if(sec == R_NilValue)
    return "have a \"dim\" attribute";

  if(prim_len != sec_len)
    return CSR_smprintf4(
      ALIKEC_MAX_CHAR, "have %s dimension%s (has %s)",
      CSR_len_as_chr(prim_len), prim_len == (R_xlen_t) 1 ? "" : "s",
      CSR_len_as_chr(sec_len), ""
    );

  R_xlen_t attr_i;
  int tar_dim_val;

  for(attr_i = (R_xlen_t)0; attr_i < prim_len; attr_i++) {

    tar_dim_val = INTEGER(prim)[attr_i];
    const char * tar_dim_chr = CSR_len_as_chr((R_xlen_t)tar_dim_val);
    char * err_dim1, * err_dim2;

    if(tar_dim_val && tar_dim_val != INTEGER(sec)[attr_i]) {
      if(prim_len == 2) {  // Matrix
        err_dim1 = "";
        const char * err_dimtmp;
        switch(attr_i) {
          case (R_xlen_t) 0: err_dimtmp = "row%s"; break;
          case (R_xlen_t) 1: err_dimtmp = "column%s"; break;
          default:
            error("Logic error: inconsistent matrix dimensions; contact maintainer.");
        }
        err_dim2 = (char *) CSR_smprintf4(
          ALIKEC_MAX_CHAR, err_dimtmp, tar_dim_val == 1 ? "" : "s", "", "", ""
        );
      } else {
        err_dim1 = "size ";
        err_dim2 = (char *) CSR_smprintf4(
          ALIKEC_MAX_CHAR, "at dimension %s",
          CSR_len_as_chr((R_xlen_t)(attr_i + 1)), "", "", ""
      );}
      return CSR_smprintf4(
        ALIKEC_MAX_CHAR, "have %s%s %s (has %s)",
        (const char *) err_dim1, tar_dim_chr, (const char *) err_dim2,
        CSR_len_as_chr((R_xlen_t)(INTEGER(sec)[attr_i]))
      );
  } }
  if(!R_compute_identical(ATTRIB(prim), ATTRIB(sec), 16)) {
    return "matching \"dim\" attributes (check `attributes(dim(obj))`)";
  }
  return "";
}
SEXP ALIKEC_compare_dim_ext(
  SEXP prim, SEXP sec, SEXP target, SEXP current, SEXP rev
) {
  int tmp = 0;
  int * class_err =& tmp;

  SEXP err_msg = mkString(
    ALIKEC_compare_dims(prim, sec, target, current, asInteger(rev), class_err)
  );
  SEXP res = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(res, 0, err_msg);
  SET_VECTOR_ELT(res, 1, ScalarInteger(*class_err));

  UNPROTECT(1);
  return res;
}

/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Implements comparing character vectors element for element:
  - allowing zero length strings in `target` to match any length string in `current`
  - zero length `target` to match any `current`

This is used to compare names, row.names, etc.
*/
const char * ALIKEC_compare_special_char_attrs_internal(
  SEXP target, SEXP current
) {
  SEXPTYPE cur_type, tar_type;
  R_xlen_t cur_len, tar_len, i;

  if(target == R_NilValue)
    error("Logic Error: target may not be NULL in compare_special_chars");

  /* From now on, `target` really is `target` (didn't have to be before) because
  we switch arguments in calling loop when `target` is missing*/

  tar_type = TYPEOF(target);
  cur_type = TYPEOF(current);

  if(tar_type != cur_type) {
    return CSR_smprintf4(
      ALIKEC_MAX_CHAR, "have type \"%s\" for %%s (is \"%s\")",
      type2char(tar_type), type2char(cur_type), "", ""
    );
  } else if (!(tar_len = XLENGTH(target))) { // zero len match to anything
    return "";
  } else if ((cur_len = XLENGTH(current)) != tar_len) {
    return CSR_smprintf4(
      ALIKEC_MAX_CHAR, "have length %s for %%s (is %s)",
      CSR_len_as_chr(tar_len), CSR_len_as_chr(cur_len), "", ""
    );
  } else if (tar_type == INTSXP) {
    if(!R_compute_identical(target, current, 16)){
      return "`target` and `current` identical since they are both integer";
    } else {
      return "";
    }
  } else if (tar_type == STRSXP) {
    for(i = (R_xlen_t) 0; i < tar_len; i++) {
      const char * cur_name_val, * tar_name_val = CHAR(STRING_ELT(target, i));
      if(         // check dimnames names match
        strcmp(tar_name_val, "") != 0 &&
        strcmp(tar_name_val, cur_name_val = CHAR(STRING_ELT(current, i))) != 0
      ) {
        return CSR_smprintf4(
          ALIKEC_MAX_CHAR,
          "be \"%s\" at index [[%s]] for %%s (is \"%s\")",
          tar_name_val, CSR_len_as_chr((R_xlen_t)(i + 1)), cur_name_val, ""
        );
    } }
    return "";
  } else if (tar_type != STRSXP && tar_type != INTSXP) {
    return CSR_smprintf4(
      ALIKEC_MAX_CHAR,
      "be a known attribute type (is \"%s\", which is unexpected); if you are using custom attributes consider setting `attr_mode=1`",
      type2char(tar_type), "", "", ""
    );
  }
  error("Logic Error in compare_special_char_attrs; contact maintainer");
}
// External version for unit testing

SEXP ALIKEC_compare_special_char_attrs(SEXP target, SEXP current) {
  return mkString(ALIKEC_compare_special_char_attrs_internal(target, current));
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Compare dimnames
*/
const char * ALIKEC_compare_dimnames(SEXP prim, SEXP sec) {
  if(sec == R_NilValue) return "have a \"dimnames\" attribute";

  /* The following likely doesn't need to be done for every dimnames so there
  probably is some optimization to be had here, should look into it if it
  seems slow; for example, `xlength` will cycle through all values, and so will
  the checking all attributes; also, do we really need to check whether dimnames
  has attributes other than names?*/

  SEXP prim_names = getAttrib(prim, R_NamesSymbol);
  SEXP sec_names = getAttrib(sec, R_NamesSymbol);
  SEXP prim_attr = ATTRIB(prim), sec_attr = ATTRIB(sec);

  /* Check that all `dimnames` attributes other than `names` that are both in
  target and current are identical*/

  SEXP prim_attr_cpy, sec_attr_cpy;
  for(
    prim_attr_cpy = prim_attr; prim_attr_cpy != R_NilValue;
    prim_attr_cpy = CDR(prim_attr_cpy)
  ) {
    const char * prim_tag = CHAR(PRINTNAME(TAG(prim_attr_cpy)));
    int do_continue = 0;
    if(strcmp(prim_tag, "names") == 0) continue;
    for(
      sec_attr_cpy = sec_attr; sec_attr_cpy != R_NilValue;
      sec_attr_cpy = CDR(sec_attr_cpy)
    ) {
      if(strcmp(prim_tag, CHAR(PRINTNAME(TAG(sec_attr_cpy)))) == 0) {
        if(!R_compute_identical(CAR(prim_attr_cpy), CAR(sec_attr_cpy), 16)) {
          return CSR_smprintf4(
            ALIKEC_MAX_CHAR,
            "have identical \"dimnames\" attribute \"%s\" (check `attr(dimnames(.), \"%s\")`)",
            prim_tag, prim_tag, "", ""
          );
        } else {
          do_continue = 1;
          break;
    } } }
    if(do_continue) continue;
    return CSR_smprintf4(
      ALIKEC_MAX_CHAR,
      "have a \"dimnames\" attribute \"%s\" (check `attr(dimnames(.), \"%s\")`)",
      prim_tag, prim_tag, "", ""
    );
  }
  // Compare actual dimnames attr

  R_xlen_t prim_len = XLENGTH(prim), sec_len = XLENGTH(sec);
  SEXPTYPE prim_type = TYPEOF(prim);

  if(
    prim_type != TYPEOF(sec) || prim_type != VECSXP
  ) {
    return CSR_smprintf4(
      ALIKEC_MAX_CHAR,
      "have \"dimnames\" type \"list\" for both `target` and `current`, but got \"%s\" and \"%s\" respectively; if you are using custom dimnames attributes please set `attr_mode` to 1L or 2L",
      type2char(prim_type), type2char(TYPEOF(sec)), "", ""
  );}
  if(!prim_len) return "";  // zero length list matches anything
  if(prim_len != sec_len) {
    return CSR_smprintf4(
      ALIKEC_MAX_CHAR, "have \"dimnames\" length %s (is %s)",
      CSR_len_as_chr(prim_len), CSR_len_as_chr(sec_len), "", ""
  );}
  // dimnames names

  if(prim_names != R_NilValue) {
    const char * dimnames_name_comp = ALIKEC_compare_special_char_attrs_internal(
      prim_names, sec_names
    );
    if(strlen(dimnames_name_comp)) {
      return CSR_smprintf4(
        ALIKEC_MAX_CHAR, dimnames_name_comp, "\"dimnames\" _names_", "", "", ""
      );
  } }
  // look at dimnames themselves

  SEXP prim_obj, sec_obj;
  R_xlen_t attr_i;

  for(attr_i = (R_xlen_t) 0; attr_i < prim_len; attr_i++) {
    if((prim_obj = VECTOR_ELT(prim, attr_i)) != R_NilValue) {
      sec_obj = VECTOR_ELT(sec, attr_i);
      const char * dimnames_comp = ALIKEC_compare_special_char_attrs_internal(
        prim_obj, sec_obj
      );
      if(strlen(dimnames_comp)) {
        const char * err_msg;
        if(prim_len == 2) { // matrix like
          switch(attr_i) {
            case (R_xlen_t) 0: err_msg = "row names"; break;
            case (R_xlen_t) 1: err_msg = "column names"; break;
            default: error("Logic Error: dimnames dimension mismatch; contact maintainer.");
          }
        } else {
          err_msg = CSR_smprintf4(
            ALIKEC_MAX_CHAR, "\"dimnames\" at dimension %s",
            CSR_len_as_chr(attr_i + (R_xlen_t) 1), "", "", ""
        );}
        return CSR_smprintf4(
          ALIKEC_MAX_CHAR, dimnames_comp, err_msg, "" , "", ""
        );
  } } }
  return "";
}
SEXP ALIKEC_compare_dimnames_ext(SEXP prim, SEXP sec) {
  return(mkString(ALIKEC_compare_dimnames(prim, sec)));
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
normal attribute comparison; must be identical with some exceptions for
reference attributes.

Note that we feed missing attributes as R_NilValue, which is unambiguous since
`attr(x, y) <- NULL` unsets attributes so there shouldn't be an actual attribute
with a NULL value
*/

const char * ALIKEC_compare_attributes_internal_simple(
  SEXP target, SEXP current, const char * attr_name,
  const struct ALIKEC_settings * set
) {
  R_xlen_t tae_val_len, cae_val_len;
  SEXPTYPE tae_type = TYPEOF(target), cae_type = TYPEOF(current);

  if(tae_type == NILSXP && cae_type == NILSXP) return "";
  else if(tae_type == NILSXP)
    return CSR_smprintf4(
      ALIKEC_MAX_CHAR,
      "not have attribute \"%s\"",
      attr_name, "", "", ""
    );
  else if(cae_type == NILSXP)
    return CSR_smprintf4(
      ALIKEC_MAX_CHAR, "have attribute \"%s\"",
      attr_name, "", "", ""
    );
  else if(tae_type != cae_type) {
    return CSR_smprintf4(
      ALIKEC_MAX_CHAR,
      "have type \"%s\" (is \"%s\") for attribute `%s`",
      type2char(tae_type), type2char(cae_type), attr_name, ""
    );
  } else if (
    (
      tae_type == EXTPTRSXP || tae_type == WEAKREFSXP ||
      tae_type == BCODESXP || tae_type == ENVSXP
    ) && set->attr_mode
  ) {
    // Because these attributes are references to other objects that
    // we cannot directly compare, and could for all intents and
    // purposes be "identical" in the typical R sense, i.e. not
    // pointing to exact same memory location, but otherwise the
    // same, we consider the fact that they are of the same type
    // a match for alike purposes.  This is a bit of a cop out,
    // but the situations where these attributes alone would cause
    // a mismatch seem pretty rare
    return "";
  } else if (
    (tae_val_len = xlength(target)) != (cae_val_len = xlength(current))
  ) {
    if(set->attr_mode || tae_val_len) {
      return CSR_smprintf4(
        ALIKEC_MAX_CHAR, "have length %s (is %s) for attribute \"%s\"",
        CSR_len_as_chr(tae_val_len), CSR_len_as_chr(cae_val_len), attr_name, ""
    );}
  } else if (!set->attr_mode && !tae_val_len) {
    return "";
  } else {
    const char * res = ALIKEC_alike_internal(target, current, set);
    if(res[0]) {
      return CSR_smprintf4(
        ALIKEC_MAX_CHAR, "have alike attributes for attribute `%s` (check alikeness of `attributes(attr(., \"%s\"))`)",
        attr_name, attr_name, "", ""
    );}
  }
  return "";
}
/* Used by alike to compare attributes; returns pointer to zero length character
string if successful, an error message otherwise

Code heavily inspired by `R_compute_identical` (thanks R CORE)

Problems to resolve:
- Different dimnames don't seem to trigger errors
- zero length row.names don't match row.names
- zero length attributes generally don't match attributes
- class matching seems buggy

Other notes:
- Special attributes are class, dim, dimnames
- dimnames may have at most a names attribute if in special attr mode
- all other attribute attributes must be identical

lst <-   list(list( 1,  2), list( 3, list( 4, list( 5, list(6, 6.1, 6.2)))))
mx.1 <- matrix(integer(), 3, 3, dimnames = list(NULL, letters[2:4]))
mx.2 <- matrix(integer(), 3, 3, dimnames = list(LETTERS[1:3], letters[1:3]))

microbenchmark(
  attr_compare(lst, lst),   # no attrs
  attr_compare(mx.2, mx.2), # attrs but no error
  attr_compare(mx.1, mx.2)  # attrs and error
)
Unit: microseconds
                     expr   min     lq median     uq    max neval
   attr_compare(lst, lst) 1.278 1.3755 1.4635 1.5510 23.470   100
 attr_compare(mx.2, mx.2) 1.539 1.6380 1.7905 1.8885  2.426   100
 attr_compare(mx.1, mx.2) 2.406 2.5380 2.6100 2.7150 13.482   100
*/

const char * ALIKEC_compare_attributes_internal(
  SEXP target, SEXP current, const struct ALIKEC_settings * set, int * is_df,
  int * err_lvl
) {
  /*
  Array to store major errors from, in order:
    0. class,
    1. names/rownames,
    2. dim
    3. dimnames
    4. other
    5. missing*/

  const char * err_major[6] = {"", "", "", "", "", ""};

  // Note we don't protect these because target and curent should come in
  // protected so every SEXP under them should also be protected

  SEXP tar_attr, cur_attr, prim_attr, sec_attr;
  int rev = 0;

  tar_attr = ATTRIB(target);
  cur_attr = ATTRIB(current);

  if(tar_attr == R_NilValue && cur_attr == R_NilValue) return "";
  else if(tar_attr == R_NilValue) {
    rev = 1;
    prim_attr = cur_attr;
    sec_attr = tar_attr;
    if(set->attr_mode == 2) {
      err_major[5] = "have attributes";
    }
  } else {
    prim_attr = tar_attr;
    sec_attr = cur_attr;
    if(cur_attr == R_NilValue) {
      err_major[5] = CSR_smprintf4(
        ALIKEC_MAX_CHAR, "not have attributes (has %s attributes)",
        CSR_len_as_chr(xlength(cur_attr)), "", "", ""
  );} }
  /*
  Loop through all attr combinations; maybe could be made faster by
  reducing the second loop each time a match is found, though this would require
  duplication of the attributes (likely faster for items with lots of attributes,
  but slower for those with few; ideally need a way to duplicate the LISTSXP but
  not the contents - I guess wouldn't be too hard to implement).
  */
  SEXP prim_attr_el, sec_attr_el, prim_attr_el_val, sec_attr_el_val;
  int sec_attr_counted = 0, sec_attr_count = 0, prim_attr_count = 0;

  for(
    prim_attr_el = prim_attr; prim_attr_el != R_NilValue;
    prim_attr_el = CDR(prim_attr_el)
  ) {
    const char * tx = CHAR(PRINTNAME(TAG(prim_attr_el)));
    prim_attr_count++;

    for(
      sec_attr_el = sec_attr; sec_attr_el != R_NilValue;
      sec_attr_el = CDR(sec_attr_el)
    ) {
      if(!sec_attr_counted) sec_attr_count++;
      if(strcmp(tx, CHAR(PRINTNAME(TAG(sec_attr_el)))) == 0) break;
    }
    sec_attr_counted = 1;
    // No match only matters if target has attrs or in strict mode

    if(sec_attr_el == R_NilValue && (!rev || set->attr_mode == 2)) {
      if(!strlen(err_major[5])) {             // first no match
        err_major[5] = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "have attribute \"%s\"%s", tx, rev ? " missing" : "",
          "", ""
    );} }
    sec_attr_el_val = sec_attr_el != R_NilValue ? CAR(sec_attr_el) : R_NilValue;
    prim_attr_el_val = CAR(prim_attr_el);

    if(prim_attr_el == R_NilValue) { // NULL attrs shouldn't be possible
      error(
        "Logic Error: attribute %s is NULL for `%s`", tx,
        rev ? "current" : "target"
    );}
    // = Baseline Check ========================================================

    if(set->attr_mode && sec_attr_el_val != R_NilValue && !strlen(err_major[4])) {
      err_major[4] = ALIKEC_compare_attributes_internal_simple(
        prim_attr_el_val, sec_attr_el_val, tx, set
      );
    // = Custom Checks =========================================================

    /* see alike documentation for explanations of how the special
    attributes class, dim, and dimnames are compared */

    } else {
      // - Class ---------------------------------------------------------------

      /* Class errors always trump all others so no need to calculate further;
      for every other error we have to keep going in case we eventually find a
      class error*/

      if(!strcmp(tx, "class")) {
        SEXP sec_attr_el_val_tmp;
        if(sec_attr_el_val == R_NilValue) { // Implicit classes
          sec_attr_el_val_tmp = PROTECT(ALIKEC_mode(rev ? target : current));
        } else {
          sec_attr_el_val_tmp = PROTECT(sec_attr_el_val);
        }
        const char * class_comp = ALIKEC_compare_class(
          prim_attr_el_val, sec_attr_el_val_tmp, rev, is_df
        );
        UNPROTECT(1);
        if(strlen(class_comp)) {
          err_major[0] = class_comp;
          break;
        }
      // - Names ---------------------------------------------------------------

      } else if (strcmp(tx, "names") == 0 || strcmp(tx, "row.names") == 0) {
        const char * name_comp = ALIKEC_compare_special_char_attrs_internal(
          prim_attr_el_val, sec_attr_el_val
        );
        if(strlen(name_comp))
          err_major[1] = CSR_smprintf4(ALIKEC_MAX_CHAR, name_comp, tx, "", "", "");
        continue;
      // - Dims ----------------------------------------------------------------

      } else if (strcmp(tx, "dim") == 0 && set->attr_mode == 0) {
        int tmp = 0;
        int * class_mode = &tmp;

        const char * dim_comp = ALIKEC_compare_dims(
          prim_attr_el_val, sec_attr_el_val, target, current, rev, class_mode
        );
        if(class_mode) { // implicit class error
          err_major[0] = dim_comp;
        } else {
          err_major[2] = dim_comp;
        }
      // - dimnames ------------------------------------------------------------

      } else if (strcmp(tx, "dimnames") == 0) {
        err_major[3] = ALIKEC_compare_dimnames(prim_attr_el_val, sec_attr_el_val);
      // - normal attrs --------------------------------------------------------

      } else {
        err_major[4] = ALIKEC_compare_attributes_internal_simple(
          prim_attr_el_val, sec_attr_el_val, tx, set
      );}
  } }
  // If in strict mode, must have the same number of attributes

  if(set->attr_mode == 2 && prim_attr_count != sec_attr_count) {
    err_major[5] = CSR_smprintf4(
      ALIKEC_MAX_CHAR,
      "have %s attribute%s (has %s)", CSR_len_as_chr(prim_attr_count),
      prim_attr_count != 1 ? "s" : "", CSR_len_as_chr(sec_attr_count), ""
  );}
  // Now determine which error to throw, if any

  int i;
  for(i = 0; i < 6; i++) {
    if(strlen(err_major[i]) && (!rev || (rev && set->attr_mode == 2))) {
      *err_lvl = i;
      return err_major[i];
    }
  }
  // Passed

  return "";
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
external interface for compare attributes
*/
SEXP ALIKEC_compare_attributes(SEXP target, SEXP current, SEXP attr_mode) {
  SEXPTYPE attr_mode_type = ALIKEC_typeof_internal(attr_mode, sqrt(DOUBLE_EPS));
  const char * comp_res;
  int tmp = 0, tmp2 = -1;
  int * is_df =& tmp, * err_lvl =& tmp2;

  if(attr_mode_type != INTSXP || XLENGTH(attr_mode) != 1)
    error("Argument `mode` must be a one length integer like vector");

  const struct ALIKEC_settings * set = &(struct ALIKEC_settings) {
    0, sqrt(DOUBLE_EPS), asInteger(attr_mode), "", 0, R_NilValue
  };
  comp_res = ALIKEC_compare_attributes_internal(
    target, current, set, is_df, err_lvl
  );
  if(strlen(comp_res)) {
    return mkString(comp_res);
  } else {
    return ScalarLogical(1);
  }
}
