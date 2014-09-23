#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                     SETUP                                    |
|                                                                              |
\* -------------------------------------------------------------------------- */

SEXP ALIKEC_alike (SEXP target, SEXP current, SEXP int_mode, SEXP int_tol, SEXP attr_mode);
SEXP ALIKEC_alike_fast (SEXP target, SEXP current);
SEXP ALIKEC_typeof(SEXP object, SEXP tolerance);
SEXP ALIKEC_typeof_fast(SEXP object);
SEXP ALIKEC_type_alike(SEXP target, SEXP current, SEXP mode, SEXP tolerance);
SEXP ALIKEC_type_alike_fast(SEXP target, SEXP current);
SEXPTYPE ALIKEC_typeof_internal(SEXP object, double tolerance);
const char *  ALIKEC_type_alike_internal(SEXP target, SEXP current, int mode, double tolerance);
SEXP ALIKEC_compare_attributes(SEXP target, SEXP current, SEXP attr_mode);
SEXP ALIKEC_test(SEXP obj1, SEXP obj2, SEXP env);

static const
R_CallMethodDef callMethods[] = {
  {"alike", (DL_FUNC) &ALIKEC_alike, 5},
  {"alike_fast", (DL_FUNC) &ALIKEC_alike_fast, 2},
  {"typeof", (DL_FUNC) &ALIKEC_typeof, 2},
  {"typeof_fast", (DL_FUNC) &ALIKEC_typeof_fast, 1},
  {"type_alike", (DL_FUNC) &ALIKEC_type_alike, 4},
  {"type_alike_fast", (DL_FUNC) &ALIKEC_type_alike_fast, 2},
  {"compare_attributes", (DL_FUNC) &ALIKEC_compare_attributes, 3},
  {"test", (DL_FUNC) &ALIKEC_test, 3},
  {NULL, NULL, 0} 
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
// - Helper Functions ----------------------------------------------------------

/*
Returns a character pointer to the string representation of the integer; allocates
with R_alloc so in theory don't need to worry about freeing memory
*/

const char * ALIKEC_xlen_to_char(R_xlen_t a) {
  if(a < 0)
    error("Logic Error: unexpected negative length value.");
  int int_len = (int) ceil(log10(a + 1.00001));  // + 1.00001 to account for 0
  char * res;
  res = R_alloc(int_len + 1, sizeof(char));
  sprintf(res, "%td", a);    // Correct type of R_xlen_t?
  return (const char *) res;
}
/* Returns a character pointer containing the results of using `a` as the parent
string and all the others a substrings with `sprintf`

note:
- `a` must contain exactly four unescaped "%s"; this will break ugly if it doesn't
*/

char * ALIKEC_sprintf(char * a, const char * b, const char * c, const char * d, const char * e) {
  int full_len = strlen(a) + strlen(b) + strlen(c) + strlen(d) + strlen(e) - 8 + 1;
  char * res;
  //Rprintf("%s %s %s %s%s; allocating %d\n", a, b, c, d, e, full_len);
  res = R_alloc(full_len, sizeof(char));
  sprintf(res, a, b, c, d, e);
  return res;
}

/* Estimate how many characters an integer can be represented with */

int ALIKEC_int_charlen (R_xlen_t a) {
  if(a < 0)
    error("Logic Error: unexpected negative length value.");
  return (int) ceil(log10(a + 1.1));
}


// - Testing Function ----------------------------------------------------------

SEXP ALIKEC_test(SEXP obj1, SEXP obj2, SEXP rho) {

  return ScalarReal(log10(4503599627370495));

  // klass = getAttrib(obj1, R_ClassSymbol);

  // t = s = PROTECT(allocList(3));
  // SET_TYPEOF(s, LANGSXP);
  // SETCAR(t, install("inherits")); t = CDR(t);
  // SETCAR(t,  obj2); t = CDR(t);
  // SETCAR(t, klass);
  // UNPROTECT(1);
  // return eval(s, rho);
  // CHAR(asChar(klass))
  // // UNPROTECT(1);

  // Rprintf("class: %s\n", CHAR(asChar(klass)));
  // return ScalarLogical(inherits(obj2, CHAR(asChar(klass))));
}
/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                     TYPE                                     |
|                                                                              |
\* -------------------------------------------------------------------------- */

/*
compare types, accounting for "integer like" numerics; empty string means success,
otherwise outputs an a character string explaining why the types are not alike
*/

const char * ALIKEC_type_alike_internal(SEXP target, SEXP current, int mode, double tolerance) {
  SEXPTYPE tar_type, cur_type, tar_type_raw, cur_type_raw;
  tar_type_raw = TYPEOF(target);
  cur_type_raw = TYPEOF(current);
  
  if(tar_type_raw == cur_type_raw)
    return "";
  
  switch(mode) {
    case 0:
      tar_type = ALIKEC_typeof_internal(target, tolerance);
      cur_type = ALIKEC_typeof_internal(current, tolerance);
      break;
    case 1:
    case 2:
      tar_type = tar_type_raw;
      cur_type = cur_type_raw;
      break;
    default:
      error("Logic Error: unexpected type comparison mode %d\n", mode);
  }
  if(
    cur_type == INTSXP && mode < 2 && 
    (tar_type == INTSXP || tar_type == REALSXP)
  ) {
    return "";
  }
  const char * what;
  if(mode == 0 && tar_type == INTSXP) {
    what = "integer-like";
  } else if (mode < 2 && tar_type == REALSXP) {
    what = "numeric";
  } else {
    what = type2char(tar_type);
  }
  return ALIKEC_sprintf(
    "expected \"%s\", but got \"%s\"", what, type2char(cur_type), "", ""
  );
}
SEXP ALIKEC_type_alike(SEXP target, SEXP current, SEXP mode, SEXP tolerance) {
  SEXPTYPE mod_type, tol_type;
  const char * res;
  
  mod_type = ALIKEC_typeof_internal(mode, sqrt(DOUBLE_EPS));
  tol_type = ALIKEC_typeof_internal(tolerance, sqrt(DOUBLE_EPS));
  
  if(mod_type != INTSXP || XLENGTH(mode) != 1) 
    error("Argument `mode` must be a one length integer like vector");
  if((tol_type != INTSXP && tol_type != REALSXP) || XLENGTH(tolerance) != 1) 
    error("Argument `tolerance` must be a one length numeric vector");
  
  res = ALIKEC_type_alike_internal(target, current, asInteger(mode), asReal(tolerance));

  if(strlen(res)) {
    return(mkString(res));
  } else {
    return ScalarLogical(1);
  }
}
SEXP ALIKEC_type_alike_fast(SEXP target, SEXP current) {
  const char * res;
  res = ALIKEC_type_alike_internal(target, current, 0, sqrt(DOUBLE_EPS));
  if(strlen(res)) {
    return(mkString(res));
  } else {
    return ScalarLogical(1);
  }
}

/* - typeof ----------------------------------------------------------------- */

SEXPTYPE ALIKEC_typeof_internal(SEXP object, double tolerance) {
  int obj_len = XLENGTH(object), i, items=0, finite;
  double * obj_real, diff_abs=0, val=0, flr;
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

  if(TYPEOF(tolerance) != REALSXP || XLENGTH(tolerance) != 1L)
    error("Argument tolerance should be a one length numeric vector");

  return mkString(type2char(ALIKEC_typeof_internal(object, REAL(tolerance)[0])));
}
SEXP ALIKEC_typeof_fast(SEXP object) {
  return mkString(type2char(ALIKEC_typeof_internal(object, sqrt(DOUBLE_EPS))));
}

/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                 ATTRIBUTES                                   |
|                                                                              |
\* -------------------------------------------------------------------------- */

/*
Implements comparing character vectors element for element, allowing zero lenght
strings in `target` to match any length string in `current`
*/

const char * ALIKEC_compare_special_char_attrs(SEXP target, SEXP current) {

  SEXPTYPE cur_type, tar_type;
  R_xlen_t cur_len, tar_len, i;

  tar_type = TYPEOF(target);
  cur_type = TYPEOF(current);

  if(tar_type != cur_type && target != R_NilValue) {
    return ALIKEC_sprintf(
      "type mismatch between `target` and `current` (%s vs %s)",
      type2char(tar_type), type2char(cur_type), "", ""
    );
  } else if (target == R_NilValue || !(tar_len = XLENGTH(target))) {
    return "";
  } else if ((cur_len = XLENGTH(current)) != tar_len) {
    return ALIKEC_sprintf(
      "length mismatch between `target` and `current` (%s vs %s)",
      ALIKEC_xlen_to_char(tar_len), ALIKEC_xlen_to_char(cur_len), "", ""
    );        
  } else if (tar_type == INTSXP && !R_compute_identical(target, current, 16)) {
    return "`target` and `current` are integer and not identical";
  } else if (tar_type == STRSXP) {
    if((cur_len = XLENGTH(current)) != (tar_len = XLENGTH(target))) {
      return ALIKEC_sprintf(
        "length mismatch between `target` and `current` (%s vs %s)",
        ALIKEC_xlen_to_char(tar_len), ALIKEC_xlen_to_char(cur_len), "", ""
      );        
    }
    for(i = 0; i < tar_len; i++) {
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

  SEXP tar_attr, cur_attr, tar_attr_el, cur_attr_el, tar_attr_el_val, cur_attr_el_val;
  SEXPTYPE tar_attr_el_val_type;
  R_xlen_t cur_attr_len, tar_attr_len, tar_attr_el_val_len;
  int attr_i, tar_dim_val, attr_match;
  
  // Note we don't protect these because target and curent should come in 
  // protected so every SEXP under them should also be protected
  
  tar_attr = ATTRIB(target);
  cur_attr = ATTRIB(current);

  if(
    (tar_attr != R_NilValue || attr_mode == 2) && 
    !(tar_attr == R_NilValue && cur_attr == R_NilValue)
  ) {
    if(attr_mode == 2 && tar_attr == R_NilValue && cur_attr != R_NilValue)
      return "current must have the exact same attributes target has, with none extra or missing";
    if(tar_attr != R_NilValue && cur_attr == R_NilValue) {
      return "target has attributes but current does not";
    }
    // There must be attributes on both target and current
    if((TYPEOF(tar_attr) != LISTSXP) || (TYPEOF(cur_attr) != LISTSXP)) {
      warning("ignoring non-pairlist attributes");
    } else {
      tar_attr_len = xlength(tar_attr);
      cur_attr_len = xlength(cur_attr);
      
      if(attr_mode == 2 && tar_attr_len < cur_attr_len) {
        return ALIKEC_sprintf(
          "target and current must have same number of attributes, currently %s vs %s%s%s",
          ALIKEC_xlen_to_char(tar_attr_len), ALIKEC_xlen_to_char(cur_attr_len), "", ""
        );
      } else if (tar_attr_len > cur_attr_len) {
        return "current must have all the attributes that target has";
      } else {
        // Loop through all attr combinations; maybe could be made faster by
        // reducing the second loop each time a match is found

        for(tar_attr_el = tar_attr; tar_attr_el != R_NilValue; tar_attr_el = CDR(tar_attr_el)) {
          const char * tx = CHAR(PRINTNAME(TAG(tar_attr_el)));
          
          attr_match = 0;  // Track whether an attribute was matched or not
          for(cur_attr_el = cur_attr; cur_attr_el != R_NilValue; cur_attr_el = CDR(cur_attr_el)) {
            if(strcmp(tx, CHAR(PRINTNAME(TAG(cur_attr_el)))) == 0) {

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

              Actually, for now we're just treating this as any other attribute
              and we'll see if it causes problems.
              */

              /* not entirely sure what the "default" flag is for `identical`,
              seems to be 16 by the convention that all the flags that are TRUE
              get set to zero, but very confusing why `ignore.environment` basically
              is the opposite of all the others.  Otherwise it would have made
              sense to have the default flag be 0  NEED TO TEST CLOSURE
              COMPARISON!!!*/
              
              // - class -------------------------------------------------------

              /* see R documentation for explanations of how the special 
              attributes class, dim, and dimnames are compared */

              if(strcmp(tx, "class") == 0 && attr_mode == 0) {
                int tar_class_len, cur_class_len, len_delta, tar_class_i, cur_class_i;
                const char * cur_class;
                const char * tar_class;

                if(TYPEOF(tar_attr_el_val) != STRSXP || TYPEOF(cur_attr_el_val) != STRSXP) {
                  return "`class` attribute not character vector for both `target` and `current`; if you are using custom `class` attributes please set `attr_mode` to 1L or 2L";
                } else if(
                  (tar_class_len = XLENGTH(tar_attr_el_val)) > 
                  (cur_class_len = XLENGTH(cur_attr_el_val))
                ) {
                  return "`current` does not have all the classes `target` has";
                }
                len_delta = cur_class_len - tar_class_len;
                for(
                  cur_class_i = len_delta, tar_class_i = 0; 
                  cur_class_i < cur_class_len;
                  cur_class_i++, tar_class_i++
                ) {
                  if(
                    strcmp(
                      cur_class = CHAR(STRING_ELT(cur_attr_el_val, cur_class_i)), 
                      tar_class = CHAR(STRING_ELT(tar_attr_el_val, tar_class_i))
                    ) != 0
                  ) {
                    return ALIKEC_sprintf(
                      "`class` mismatch at class #%s: expected \"%s\" but got \"%s\"%s",
                      ALIKEC_xlen_to_char((R_xlen_t)(cur_class_i + 1)),
                      tar_class, cur_class, ""
                    );
                  }
                }
                if(!R_compute_identical(ATTRIB(tar_attr_el_val), ATTRIB(cur_attr_el_val), 16)) {
                  return "attribute `class` has mismatching attributes (check `attributes(class(obj))`)";
                }
              // - names/row.names ---------------------------------------------
              } else if ((strcmp(tx, "names") == 0 || strcmp(tx, "row.names") == 0) && attr_mode == 0) {
                const char * name_comp = ALIKEC_compare_special_char_attrs(
                  tar_attr_el_val, cur_attr_el_val
                );
                if(strlen(name_comp))
                  return ALIKEC_sprintf("`%s` mismatch: %s", tx, name_comp, "", "");
              // - dim ---------------------------------------------------------
              } else if (strcmp(tx, "dim") == 0 && attr_mode == 0) {
                if(
                  (tar_attr_el_val_type = TYPEOF(tar_attr_el_val)) != TYPEOF(cur_attr_el_val) ||
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
                      ALIKEC_xlen_to_char((R_xlen_t)(attr_i + 1)), ALIKEC_xlen_to_char((R_xlen_t)tar_dim_val), 
                      ALIKEC_xlen_to_char((R_xlen_t)(INTEGER(cur_attr_el_val)[attr_i])), ""
                    );
                } }
                if(!R_compute_identical(ATTRIB(tar_attr_el_val), ATTRIB(cur_attr_el_val), 16)) {
                  return "attribute `dim` has mismatching attributes (check `attributes(dim(obj))`)";
                }

              // - dimnames ----------------------------------------------------

              } else if (strcmp(tx, "dimnames") == 0 && attr_mode == 0) {
                SEXP tar_attr_el_val_dimname_obj, cur_attr_el_val_dimname_obj,
                  tar_attr_el_val_dimnames_names, cur_attr_el_val_dimnames_names,
                  tar_dimnames_attr, cur_dimnames_attr, tdn_attr, cdn_attr;
                int tar_dimnames_has_names, cur_dimnames_has_names;

                // The following likely doesn't need to be done for every dimnames
                // so there probably is some optimization to be had here, should
                // look into it if it seems slow; for example, `xlength` will
                // cycle through all values, and so will the checking all attributes

                tar_attr_el_val_dimnames_names = getAttrib(tar_attr_el_val, R_NamesSymbol);
                cur_attr_el_val_dimnames_names = getAttrib(cur_attr_el_val, R_NamesSymbol);
                tar_dimnames_has_names = (tar_attr_el_val_dimnames_names != R_NilValue);
                cur_dimnames_has_names = (cur_attr_el_val_dimnames_names != R_NilValue);
                tar_dimnames_attr = ATTRIB(tar_attr_el_val);
                cur_dimnames_attr = ATTRIB(cur_attr_el_val);

                if(
                  xlength(tar_dimnames_attr) - (R_xlen_t) tar_dimnames_has_names !=
                  xlength(cur_dimnames_attr) - (R_xlen_t) cur_dimnames_has_names
                ) {
                  return ALIKEC_sprintf(
                    "`dimnames` has a different number of attributes in target and current (%s vs %s) (note count excludes `dimnames` `names` attribute)",
                    ALIKEC_xlen_to_char((R_xlen_t)(xlength(tar_dimnames_attr) - tar_dimnames_has_names)), 
                    ALIKEC_xlen_to_char((R_xlen_t)(xlength(cur_dimnames_attr) - cur_dimnames_has_names)), "", ""
                  );                  
                }
                // Check that all `dimnames` attributes other than `names` are
                // identical; in practice this loop should never have to do anything
                // but this is here for consistency with other special attr treatment

                for(tdn_attr = tar_dimnames_attr; tdn_attr != R_NilValue; tdn_attr = CDR(tdn_attr)) {
                  const char * tdn_tag = CHAR(PRINTNAME(TAG(tdn_attr)));
                  if(strcmp(tdn_tag, "names") == 0) continue;
                  for(cdn_attr = cur_dimnames_attr; cdn_attr != R_NilValue; cdn_attr = CDR(cdn_attr)) {
                    if(strcmp(tdn_tag, CHAR(PRINTNAME(TAG(cdn_attr)))) == 0){
                      if(!R_compute_identical(CAR(tdn_attr), CAR(cdn_attr), 16)){
                        return ALIKEC_sprintf(
                          "`dimnames` attribute `%s` is not identical in target and current (check `attr(dimnames(obj), \"%s\")`)",
                          tdn_tag, tdn_tag, "", ""
                        );
                      } else continue;
                  } }
                  return ALIKEC_sprintf(
                    "`dimnames` attribute `%s` is missing from current but present in target (check `attr(dimnames(obj), \"%s\")`)",
                    tdn_tag, tdn_tag, "", ""
                  );
                }
                // Compare actual dimnames attr

                tar_attr_el_val_len = xlength(tar_attr_el_val);
                if(
                  (tar_attr_el_val_type = TYPEOF(tar_attr_el_val)) != TYPEOF(tar_attr_el_val) ||
                  tar_attr_el_val_type != VECSXP || 
                  tar_attr_el_val_len != xlength(cur_attr_el_val)
                ) {
                  return "`dimnames` size mismatch or non-list dimnames";
                } 
                // Now look at dimnames names
                
                const char * dimnames_name_comp = ALIKEC_compare_special_char_attrs(
                  tar_attr_el_val_dimnames_names, cur_attr_el_val_dimnames_names
                );
                if(strlen(dimnames_name_comp))
                  return ALIKEC_sprintf("`dimnames` _names_ mismatch: %s", dimnames_name_comp, "", "", "");

                // Now look at dimnames themselves

                for(attr_i = 0; attr_i < tar_attr_el_val_len; attr_i++) {
                  if (       // check dimnames match 
                    (
                      tar_attr_el_val_dimname_obj = 
                        VECTOR_ELT(tar_attr_el_val, attr_i)
                    ) != R_NilValue
                  ) {
                    cur_attr_el_val_dimname_obj = VECTOR_ELT(cur_attr_el_val, attr_i);
                    if(
                      !R_compute_identical(
                        tar_attr_el_val_dimname_obj,
                        cur_attr_el_val_dimname_obj, 16
                      )
                    ) {
                      return ALIKEC_sprintf(
                        "`dimnames` mismatch at dimension %s%s%s%s", 
                        ALIKEC_xlen_to_char((R_xlen_t)(attr_i + 1)), "", "", ""
                      );
                } } }
              } else {
                // - Other Attributes ------------------------------------------

                R_xlen_t tae_val_len, cae_val_len;
                SEXPTYPE tae_type, cae_type;

                if((tae_type = TYPEOF(tar_attr_el_val)) != (cae_type = TYPEOF(cur_attr_el_val))) {
                  return ALIKEC_sprintf(
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
                  // purposes be "identical" in the typical R sense (i.e. not 
                  // pointing to exact same memory location, but otherwise the 
                  // same, we consider the fact that they are of the same type
                  // a match for alike purposes).  This is a bit of a cop out,
                  // but the situations where these attributes alone would cause
                  // a mismatch seem pretty rare
                  return "";
                } else if (
                  ((tae_val_len = xlength(tar_attr_el_val)) != 0 || attr_mode != 0) &&
                  !R_compute_identical(tar_attr_el_val, cur_attr_el_val, 16)
                ) {
                  return ALIKEC_sprintf(
                    "attribute value mismatch for attribute `%s`%s%s%s", tx, "", "", ""
                  );                  
                } else if (
                  attr_mode == 0 && tae_val_len != (R_xlen_t) 0 &&
                  tae_val_len != (cae_val_len = xlength(cur_attr_el_val))
                ) {
                  return ALIKEC_sprintf(
                    "attribute `%s` is not of the same length in target and current (%s vs %s)%s",
                    tx, ALIKEC_xlen_to_char(tae_val_len), ALIKEC_xlen_to_char(cae_val_len), ""
                  );
                }
              } 
          } }
          if(!attr_match) {
            return ALIKEC_sprintf("attribute %s missing from current", tx, "", "", "");
  } } } } }

  return "";
}

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

/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                   ALIKE                                      |
|                                                                              |
\* -------------------------------------------------------------------------- */

SEXP ALIKEC_alike_internal(
  SEXP target, SEXP current, int int_mode, double int_tolerance, int attr_mode
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
 
  int ind_lvl = 0;          /* how deep in the stack we are */
  int ind_lvl_max = -1;     /* deepest we've gone in current stack */
  int ind_stk_sz = 32;      /* current size of stack */
  int ind_stk[ind_stk_sz];  /* track the stack */
  int tar_len, cur_len;
  SEXP sxp_stk_tar[ind_stk_sz]; /* track the parent objects */
  SEXP sxp_stk_cur[ind_stk_sz]; /* track the parent objects */
  int emr_brk = 0;
  int i;
  SEXPTYPE tar_type;

  /* Define error message; need to figure out how to move this out of here */

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

    int s4_cur, s4_tar;
    s4_tar = ((IS_S4_OBJECT)(target) != 0);
    s4_cur = ((IS_S4_OBJECT)(current) != 0);
    if(!err && (s4_cur || s4_tar)) {
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
        klass_attrib = getAttrib(klass, install("package"));
        if(xlength(klass_attrib) != 1L || TYPEOF(klass_attrib) != STRSXP)
          error("Logic Error: unexpected S4 class \"class\" attribute does not have `package` attribute in expected structure");

        t = s = PROTECT(allocList(3));
        SET_TYPEOF(s, LANGSXP);
        SETCAR(t, install("inherits")); t = CDR(t);
        SETCAR(t, current); t = CDR(t);
        SETCAR(t, klass);
        if(!asLogical(eval(s, R_BaseEnv))) {
          err = 1;
          err_base = "current does not contain class \"%s\" (package: %s)";
          err_tok1 = CHAR(asChar(klass));
          err_tok2 = CHAR(asChar(klass_attrib));
          err_tok3 = err_tok4 = "";        
        }
        UNPROTECT(1);
      }
    } else { // don't run length or attribute checks on S4

    // - Type ------------------------------------------------------------------

      if(
        strlen(err_type = ALIKEC_type_alike_internal(target, current, int_mode, int_tolerance))
      ) { 
        err = 1;
        err_base = "Type mismatch, %s";
        err_tok1 = err_type;
        err_tok2 = err_tok3 = err_tok4 = "";
      } 

    // - Length ----------------------------------------------------------------

      if(
        !err && (tar_len = xlength(target)) > 0 &&  /* zero lengths match any length */
        tar_len != (cur_len = xlength(current))
      ) {
        err = 1;
        err_base =  "Length mismatch, expected %s but got %s%s%s";
        err_tok1 = ALIKEC_xlen_to_char(tar_len);
        err_tok2 = ALIKEC_xlen_to_char(cur_len);
        err_tok3 = err_tok4 = "";
      } 
    // - Attributes ------------------------------------------------------------

      if (
        !err &&
        strlen(err_attr = ALIKEC_compare_attributes_internal(target, current, attr_mode))
      ) {
        err = 1;
        err_base = err_attr;
        err_tok1 = err_tok2 = err_tok3 = err_tok4 = "";
    } }
    // - Known Limitations -----------------------------------------------------

    tar_type = TYPEOF(target);
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
      char err_chr_indeces[ind_len + 4 * (ind_lvl + 1) + 1];
      char err_chr_index[ind_sz_max + 4 + 1];
      err_chr_indeces[0] = err_chr_index[0] = '\0';
      for(i = 0; i < ind_lvl; i++) {
        sprintf(err_chr_index, "[[%d]]", ind_stk[i] + 1);
        strcat(err_chr_indeces, err_chr_index);
      }
      /* Create final error and store in STRSXP */

      if(ind_lvl > 0) {
        err_final = (const char *) ALIKEC_sprintf("%s at index %s%s%s", err_msg, err_chr_indeces, "", "");
      } else {
        err_final = (const char *) ALIKEC_sprintf("%s%s%s%s", (char *) err_msg, "", "", "");
      }
      SEXP res;
      res = PROTECT(allocVector(STRSXP, 1));
      SET_STRING_ELT(res, 0, mkChar(err_final));
      UNPROTECT(1);
      return res;  

      //return mkString(err_final);
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