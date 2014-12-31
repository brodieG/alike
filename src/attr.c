#include "alike.h"

/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Compare class attribute
*/
const char * ALIKEC_compare_class(SEXP prim, SEXP sec, int rev) {
  if(
    (TYPEOF(sec) != STRSXP && sec != R_NilValue) || TYPEOF(prim) != STRSXP
  ) {
    return "`class` attribute not character vector for both `target` and `current`; if you are using custom `class` attributes please set `attr_mode` to 1L or 2L";
  }
  if(sec == R_NilValue) {
    return ALIKEC_sprintf(
      "`%s` is of S3 class \"%s\", but `%s` is unclassed",
      rev ? "current" : "target",
      CHAR(STRING_ELT(prim, 1)), !rev ? "current" : "target", ""
    );
  } else if(rev)
    error("Logic Error: should never get here 19; contact maintainer.");
  if(rev > 1 || rev < 0) error("Logic Error: `rev` should be 0 or 1.");

  int tar_class_len, cur_class_len, len_delta, tar_class_i, cur_class_i;
  const char * cur_class;
  const char * tar_class;

  tar_class_len = XLENGTH(prim);
  cur_class_len = XLENGTH(sec);
  R_xlen_t class_stop =
    tar_class_len > cur_class_len ? cur_class_len : tar_class_len;

  len_delta = cur_class_len - class_stop;

  for(
    cur_class_i = len_delta, tar_class_i = 0;
    cur_class_i < cur_class_len;
    cur_class_i++, tar_class_i++
  ) {
    if(
      strcmp(
        cur_class = CHAR(STRING_ELT(sec, cur_class_i)),
        tar_class = CHAR(STRING_ELT(prim, tar_class_i))
      ) != 0
    ) {
      return ALIKEC_sprintf(
        "`class` mismatch at class #%s: expected \"%s\" but got \"%s\"%s",
        ALIKEC_xlen_to_char((R_xlen_t)(cur_class_i + 1)),
        tar_class, cur_class, ""
  );} }
  if(tar_class_len > cur_class_len) {
    return ALIKEC_sprintf(
      "`target` inherits from \"%s\" but `current` does not",
      CHAR(STRING_ELT(prim, tar_class_i)), "", "", ""
  );}
  if(!R_compute_identical(ATTRIB(prim), ATTRIB(sec), 16)) {
    return "attribute `class` has mismatching attributes (check `attributes(class(obj))`)";
  }
  return "";
}
SEXP ALIKEC_compare_class_ext(SEXP prim, SEXP sec, SEXP rev) {
  return mkString(ALIKEC_compare_class(prim, sec, asInteger(rev)));
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
    return ALIKEC_sprintf(
      "`dim` attribute for `%s` is not an integer vector; if you are using custom dim attributes please set `attr_mode` to 1L or 2L",
      rev ? (type_err == 1 ? "current" : "target") : (type_err == 2 ? "current" : "target"), "", "", ""
  );}
  SEXP prim_obj = rev ? current : target, sec_obj = rev ? target : current;

  // Dims -> implicit class

  R_xlen_t prim_len = XLENGTH(prim), prim_len_cap;
  prim_len_cap = prim_len > (R_xlen_t) 3 ? (R_xlen_t) 3 : prim_len;
  R_xlen_t sec_len = XLENGTH(sec), sec_len_cap;
  sec_len_cap = sec_len > (R_xlen_t) 3 ? (R_xlen_t) 3 : sec_len;

  const char * class_err_string = "";

  if(prim_len_cap > 1 && isVectorAtomic(prim_obj)) {
    if(sec == R_NilValue) {  // current is matrix/array
      class_err_string = ALIKEC_sprintf(
        "`%s` is \"%s\" but `%s` does not have a \"dim\" attribute",
        rev ? "current" : "target", prim_len_cap > 2 ? "array" : "matrix",
        !rev ? "current" : "target", ""
      );
    } else if(isVectorAtomic(sec_obj) && sec_len_cap != prim_len_cap) {  // target is matrix/array
      class_err_string = ALIKEC_sprintf(
        "`target` is \"%s\" but `current` is \"%s\"",
        prim_len_cap > 2 ? "array" : "matrix",
        sec_len_cap == 2 ? "matrix" : (sec_len_cap == 1 ? "vector" : "array"), "", ""
    );}
  } else if (sec_len_cap > 1 && isVectorAtomic(sec_obj)) {
    if(isVectorAtomic(prim_obj)) {
      class_err_string = ALIKEC_sprintf(
        "`target` is \"vector\" but `current` is \"%s\"",
        sec_len_cap == 2 ? "matrix" : "array", "", "", ""
      );
    } else {
      class_err_string = "`target` is \"vector\" but `current` is not";
    }
  }
  if(strlen(class_err_string)) {
    *class_err = 1;
    return(class_err_string);
  }
  // Normal dim checking

  if(sec == R_NilValue)
    return "`target` has a \"dim\" attribute but `current` does not";

  if(prim_len != sec_len)
    return ALIKEC_sprintf(
      "`target` has %s dimensions but `current` has %s",
      ALIKEC_xlen_to_char(prim_len), ALIKEC_xlen_to_char(sec_len), "", ""
    );

  R_xlen_t attr_i;
  int tar_dim_val;
  for(attr_i = (R_xlen_t)0; attr_i < prim_len; attr_i++) {
    if(
      (tar_dim_val = INTEGER(prim)[attr_i]) &&
      tar_dim_val != INTEGER(sec)[attr_i]
    ) {
      return ALIKEC_sprintf(
        "`dim` size mismatch at dimension %s: expected %s but got %s%s",
        ALIKEC_xlen_to_char((R_xlen_t)(attr_i + 1)), ALIKEC_xlen_to_char((R_xlen_t)tar_dim_val),
        ALIKEC_xlen_to_char((R_xlen_t)(INTEGER(sec)[attr_i])), ""
      );
  } }
  if(!R_compute_identical(ATTRIB(prim), ATTRIB(sec), 16)) {
    return "attribute `dim` has mismatching attributes (check `attributes(dim(obj))`)";
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
const char * ALIKEC_compare_special_char_attrs_internal(SEXP target, SEXP current) {

  SEXPTYPE cur_type, tar_type;
  R_xlen_t cur_len, tar_len, i;

  if(target == R_NilValue)
    error("Logic Error: target may not be NULL in compare_special_chars");

  /* From now on, `target` really is `target` (didn't have to be before) because
  we switch arguments in calling loop when `target` is missing*/

  tar_type = TYPEOF(target);
  cur_type = TYPEOF(current);

  if(tar_type != cur_type) {
    return ALIKEC_sprintf(
      "type mismatch between `target` and `current` (%s vs %s)",
      type2char(tar_type), type2char(cur_type), "", ""
    );
  } else if (!(tar_len = XLENGTH(target))) { // zero len match to anything
    return "";
  } else if ((cur_len = XLENGTH(current)) != tar_len) {
    return ALIKEC_sprintf(
      "length mismatch between `target` and `current` (%s vs %s)",
      ALIKEC_xlen_to_char(tar_len), ALIKEC_xlen_to_char(cur_len), "", ""
    );
  } else if (tar_type == INTSXP && !R_compute_identical(target, current, 16)) {
    return "`target` and `current` are integer and not identical";
  } else if (tar_type == STRSXP) {
    for(i = (R_xlen_t) 0; i < tar_len; i++) {
      const char * cur_name_val, * tar_name_val = CHAR(STRING_ELT(target, i));
      if(         // check dimnames names match
        strcmp(tar_name_val, "") != 0 &&
        strcmp(tar_name_val, cur_name_val = CHAR(STRING_ELT(current, i))) != 0
      ) {
        return ALIKEC_sprintf(
          "mismatch at item #%s, expected \"%s\" but got \"%s\"",
          ALIKEC_xlen_to_char((R_xlen_t)(i + 1)), tar_name_val, cur_name_val, ""
        );
    } }
    return "";
  } else if (tar_type != STRSXP || tar_type != INTSXP) {
    return ALIKEC_sprintf(
      "unexpected attribute type %s; if you are using custom attributes consider setting `attr_mode=1`",
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
  if(sec == R_NilValue) return "`target` has \"dimnames\" but `current` does not";

  /* The following likely doesn't need to be done for every dimnames so there
  probably is some optimization to be had here, should look into it if it
  seems slow; for example, `xlength` will cycle through all values, and so will
  the checking all attributes; also, do we really need to check whether dimnames
  has attributes other than names?*/

  SEXP prim_names = getAttrib(prim, R_NamesSymbol);
  SEXP sec_names = getAttrib(sec, R_NamesSymbol);

  int prim_has_names = (prim_names != R_NilValue);
  int sec_has_names = (sec_names != R_NilValue);

  SEXP prim_attr = ATTRIB(prim), sec_attr = ATTRIB(sec);

  if(
    xlength(prim_attr) - (R_xlen_t) prim_has_names !=
    xlength(sec_attr) - (R_xlen_t) sec_has_names
  ) {
    return ALIKEC_sprintf(
      "\"dimnames\" has a different number of attributes in `target` and `current` (%s vs %s) (note count excludes \"dimnames\" `names` attribute)",
      ALIKEC_xlen_to_char((R_xlen_t)(xlength(prim_attr) - prim_has_names)),
      ALIKEC_xlen_to_char((R_xlen_t)(xlength(sec_attr) - sec_has_names)), "", ""
    );
  }
  /* Check that all `dimnames` attributes other than `names` are identical; in
  practice this loop should never have to do anything but this is here for
  consistency with other special attr treatment*/

  SEXP prim_attr_cpy, sec_attr_cpy;
  for(
    prim_attr_cpy = prim_attr; prim_attr_cpy != R_NilValue;
    prim_attr_cpy = CDR(prim_attr_cpy)
  ) {
    const char * prim_tag = CHAR(PRINTNAME(TAG(prim_attr_cpy)));
    if(strcmp(prim_tag, "names") == 0) continue;
    for(
      sec_attr_cpy = sec_attr; sec_attr_cpy != R_NilValue;
      sec_attr_cpy = CDR(sec_attr_cpy)
    ) {
      if(strcmp(prim_tag, CHAR(PRINTNAME(TAG(sec_attr_cpy)))) == 0) {
        if(!R_compute_identical(CAR(prim_attr_cpy), CAR(sec_attr_cpy), 16)) {
          return ALIKEC_sprintf(
            "\"dimnames\" attribute `%s` is not identical in `target` and `current` (check `attr(dimnames(obj), \"%s\")`)",
            prim_tag, prim_tag, "", ""
          );
        } else continue;
    } }
    return ALIKEC_sprintf(
      "\"dimnames\" attribute `%s` is missing from `current` but `present` in target (check `attr(dimnames(obj), \"%s\")`)",
      prim_tag, prim_tag, "", ""
    );
  }
  // Compare actual dimnames attr

  R_xlen_t prim_len = xlength(prim);
  SEXPTYPE prim_type = TYPEOF(prim);

  if(
    prim_type != TYPEOF(sec) || prim_type != VECSXP || prim_len != xlength(sec)
  ) {
    return "\"dimnames\" size mismatch or non-list dimnames";
  }
  // Now look at dimnames names

  const char * dimnames_name_comp = ALIKEC_compare_special_char_attrs_internal(
    prim_names, sec_names
  );
  if(strlen(dimnames_name_comp))
    return ALIKEC_sprintf("\"dimnames\" _names_ mismatch: %s", dimnames_name_comp, "", "", "");

  // Now look at dimnames themselves

  SEXP prim_obj, sec_obj;
  R_xlen_t attr_i;

  for(attr_i = (R_xlen_t) 0; attr_i < prim_len; attr_i++) {
    if((prim_obj = VECTOR_ELT(prim, attr_i)) != R_NilValue) {
      sec_obj = VECTOR_ELT(sec, attr_i);
      if(!R_compute_identical(prim_obj, sec_obj, 16)) {
        return ALIKEC_sprintf(
          "\"dimnames\" mismatch at dimension %s%s%s%s",
          ALIKEC_xlen_to_char((R_xlen_t)(attr_i + 1)), "", "", ""
        );
  } } }
  return "";
}
SEXP ALIKEC_compare_dimnames_ext(SEXP prim, SEXP sec) {
  return(mkString(ALIKEC_compare_dimnames(prim, sec)));
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/

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

const char * ALIKEC_compare_attributes_internal(SEXP target, SEXP current, int attr_mode) {

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
    if(attr_mode == 2) {
      err_major[5] = "`target` has no attributes, but `current` does.";
    }
  } else {
    prim_attr = tar_attr;
    sec_attr = cur_attr;
    if(cur_attr == R_NilValue) {
      err_major[5] = "`target` has attributes, but `current` does not.";
    }
  }
  /*
  Loop through all attr combinations; maybe could be made faster by
  reducing the second loop each time a match is found, though this would require
  duplication of the attributes (likely faster for items with lots of attributes,
  but slower for those with few; ideally need a way to duplicate the LISTSXP but
  not the contents - I guess wouldn't be too hard to implement).
  */
  SEXP prim_attr_el, sec_attr_el, prim_attr_el_val, sec_attr_el_val;

  for(
    prim_attr_el = prim_attr; prim_attr_el != R_NilValue;
    prim_attr_el = CDR(prim_attr_el)
  ) {
    const char * tx = CHAR(PRINTNAME(TAG(prim_attr_el)));

    for(
      sec_attr_el = sec_attr; sec_attr_el != R_NilValue;
      sec_attr_el = CDR(sec_attr_el)
    ) {
      if(strcmp(tx, CHAR(PRINTNAME(TAG(sec_attr_el)))) == 0) break;
    }
    // No match only matters if target has attrs or in strict mode

    if(sec_attr_el == R_NilValue && (!rev || attr_mode == 2)) {
      if(!strlen(err_major[5])) {             // first no match
        err_major[5] = ALIKEC_sprintf(
          "`%s` has attribute `%s`, but `%s` does not.",
          rev ? "current" : "target", tx, !rev ? "current" : "target", ""
    );} }
    sec_attr_el_val = sec_attr_el != R_NilValue ? CAR(sec_attr_el) : R_NilValue;
    prim_attr_el_val = CAR(prim_attr_el);

    if(prim_attr_el == R_NilValue) { // NULL attrs shouldn't be possible
      error(
        "Logic Error: attribute %s is NULL for `%s`", tx,
        rev ? "current" : "target"
    );}
    // = Baseline Check ========================================================

    if(attr_mode && sec_attr_el_val != R_NilValue && !strlen(err_major[4])) {
      R_xlen_t tae_val_len, cae_val_len;
      SEXPTYPE tae_type, cae_type;

      if((tae_type = TYPEOF(prim_attr_el_val)) != (cae_type = TYPEOF(sec_attr_el_val))) {
        err_major[4] = ALIKEC_sprintf(
          "attribute `%s` is not of same type in current and target%s%s%s", tx, "", "", ""
        );
      } else if (
        attr_mode == 0 && (
          tae_type == EXTPTRSXP || tae_type == WEAKREFSXP ||
          tae_type == BCODESXP || tae_type == ENVSXP
        )
      ) {
        // Because these attributes are references to other objects that
        // we cannot directly compare, and could for all intents and
        // purposes be "identical" in the typical R sense, i.e. not
        // pointing to exact same memory location, but otherwise the
        // same, we consider the fact that they are of the same type
        // a match for alike purposes.  This is a bit of a cop out,
        // but the situations where these attributes alone would cause
        // a mismatch seem pretty rare
        continue;
      } else if (
        ((tae_val_len = xlength(prim_attr_el_val)) != 0 || attr_mode != 0) &&
        !R_compute_identical(prim_attr_el_val, sec_attr_el_val, 16)
      ) {
        err_major[4] = ALIKEC_sprintf(
          "attribute value mismatch for attribute `%s`%s%s%s", tx, "", "", ""
        );
      } else if (
        attr_mode == 0 && tae_val_len != (R_xlen_t) 0 &&
        tae_val_len != (cae_val_len = xlength(sec_attr_el_val))
      ) {
        // target is always prim in this logic branch as both target and current
        // attr must be set

        err_major[4] = ALIKEC_sprintf(
          "attribute `%s` is not of the same length in target and current (%s vs %s)%s",
          tx, ALIKEC_xlen_to_char(tae_val_len), ALIKEC_xlen_to_char(cae_val_len), ""
        );
      }
    // = Custom Checks =========================================================

    /* see alike documentation for explanations of how the special
    attributes class, dim, and dimnames are compared */

    } else {
      // - Class ---------------------------------------------------------------

      // Class errors always trump all others so no need to calculate further; for
      // every other error we have to keep going in case we eventually find a
      // class error

      if(!strcmp(tx, "class")) {
        const char * class_comp = ALIKEC_compare_class(
          prim_attr_el_val, sec_attr_el_val, rev
        );
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
          err_major[1] = ALIKEC_sprintf(
            "`%s` mismatch: %s", tx, name_comp, "", ""
          );
        continue;
      // - Dims ----------------------------------------------------------------

      } else if (strcmp(tx, "dim") == 0 && attr_mode == 0) {
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
      }
    }
  }
  // Now determine which error to throw, if any

  int i;
  for(i = 0; i < 5; i++)
    if(strlen(err_major[i]) && (!rev || (rev && attr_mode == 2)))
      return err_major[i];

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

  if(attr_mode_type != INTSXP || XLENGTH(attr_mode) != 1)
    error("Argument `mode` must be a one length integer like vector");

  comp_res = ALIKEC_compare_attributes_internal(target, current, asInteger(attr_mode));

  if(strlen(comp_res)) {
    return mkString(comp_res);
  } else {
    return ScalarLogical(1);
  }
}
