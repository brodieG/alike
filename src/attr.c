#include "alike.h"

SEXP ALIKEC_res_msg_as_sxp(struct ALIKEC_res_msg msg) {
  SEXP out = PROTECT(allocVector(VECSXP, 3));
  SEXP out_names = PROTECT(allocVector(STRSXP, 3));
  const char * names[3] = {"message", "indices", "wrap"};
  int i;

  for(i = 0; i < 3; i++) SET_STRING_ELT(out_names, i, mkChar(names[i]));

  SET_VECTOR_ELT(out, 0, mkString(msg.message));
  SET_VECTOR_ELT(out, 1, mkString(msg.indices));
  SET_VECTOR_ELT(out, 2, mkString(msg.wrap));
  setAttrib(out, R_NamesSymbol, out_names);
  UNPROTECT(2);

  return out;
}
SEXP ALIKEC_res_sub_as_sxp(struct ALIKEC_res_sub sub) {
  SEXP out = PROTECT(allocVector(VECSXP, 4));
  SEXP out_names = PROTECT(allocVector(STRSXP, 4));
  const char * names[4] = {"success", "message", "df", "lvl"};
  int i;

  for(i = 0; i < 4; i++) SET_STRING_ELT(out_names, i, mkChar(names[i]));

  SET_VECTOR_ELT(out, 0, ScalarInteger(sub.success));
  SET_VECTOR_ELT(out, 1, ALIKEC_res_msg_as_sxp(sub.message));
  SET_VECTOR_ELT(out, 2, ScalarInteger(sub.df));
  SET_VECTOR_ELT(out, 3, ScalarInteger(sub.lvl));
  setAttrib(out, R_NamesSymbol, out_names);
  UNPROTECT(2);

  return out;
}

/*
Runs alike on an attribute, really just like running alike, but since it is on
an attribute and we don't want a massive nested error message, provide a
different error message; this is not very efficient; in theory we could just
stop recursion since we're not returning the nested error message.

- `special` parameter indicates attributes that are known to have accessor
  functions (e.g. `names`).
- `attr_attr` indicates we are checking the attributes of an attribute
*/

struct ALIKEC_res_sub ALIKEC_alike_attr(
  SEXP target, SEXP current, const char * attr_name,
  struct ALIKEC_settings set, int special, int attr_attr
) {
  struct ALIKEC_res res = ALIKEC_alike_internal(target, current, set);
  struct ALIKEC_res_sub res_sub = ALIKEC_res_sub_def();
  if(!res.success) {
    res_sub.success = 0;
    res_sub.message.wrap = CSR_smprintf4(
      ALIKEC_MAX_CHAR, special ? "%s%s(%%s)%s" : "%sattr(%%s, \"%s\")%s",
      attr_attr ? "attributes(" : "", attr_name, attr_attr ? ")" : "", ""
    );
    res_sub.message.message = "be `alike` the corresponding element in target";
  }
  return res_sub;
}

/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Compare class attribute

Note that expectation is that unclassed objects will get sent here with their
implicit class defined by ALIKEC_mode.

Will set tar_is_df to 1 if prim is data frame
*/
struct ALIKEC_res_sub ALIKEC_compare_class(
  SEXP target, SEXP current, struct ALIKEC_settings set
) {
  if(TYPEOF(current) != STRSXP || TYPEOF(target) != STRSXP)
    return ALIKEC_alike_attr(target, current, "class", set, 1, 0);

  int tar_class_len, cur_class_len, len_delta, tar_class_i, cur_class_i;
  const char * cur_class;
  const char * tar_class;
  struct ALIKEC_res_sub res = ALIKEC_res_sub_def();

  tar_class_len = XLENGTH(target);
  cur_class_len = XLENGTH(current);
  R_xlen_t class_stop =
    tar_class_len > cur_class_len ? cur_class_len : tar_class_len;

  len_delta = cur_class_len - class_stop;

  for(
    cur_class_i = len_delta, tar_class_i = 0;
    cur_class_i < cur_class_len;
    cur_class_i++, tar_class_i++
  ) {
    cur_class = CHAR(STRING_ELT(current, cur_class_i));
    tar_class = CHAR(STRING_ELT(target, tar_class_i));
    if(!res.df && !strcmp(tar_class, "data.frame")) res.df = 1;

    if(res.success && strcmp(cur_class, tar_class)) { // class mismatch
      res.success = 0;
      if(cur_class_len > 1) {
        char * err_ind = CSR_len_as_chr((R_xlen_t)(cur_class_i + 1));

        res.message.wrap = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "class(%%s)[%s]", err_ind, "", "", ""
        );
        res.message.message =  CSR_smprintf4(
          ALIKEC_MAX_CHAR, "be \"%s\" (is \"%s\")",
          tar_class, cur_class, "", ""
        );
      } else {
        res.message.message =  CSR_smprintf4(
          ALIKEC_MAX_CHAR,  "be class \"%s\" (is \"%s\")",
          tar_class, cur_class, "", ""
        );
  } } }
  if(!res.success) return res;
  if(tar_class_len > cur_class_len) {
    res.success = 0;
    res.message.message = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "inherit from class \"%s\"",
      CHAR(STRING_ELT(target, tar_class_i)), "", "", ""
    );
    return res;
  }
  // Check the class attributes

  return ALIKEC_alike_attr(
    ATTRIB(target), ATTRIB(current), "class", set, 1, 1
  );
}
SEXP ALIKEC_compare_class_ext(SEXP target, SEXP current) {
  return ALIKEC_res_sub_as_sxp(
    ALIKEC_compare_class(target, current, ALIKEC_set_def(""))
  );
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Compares dimensions, but detects implicit classes by checking if atomic and
having dimensions and reports error as such if that is the case.

If there is an implicit class error res.lvl will be set to 1

tar_obj and cur_obj are the objects the dimensions are the attributes off.
*/
struct ALIKEC_res_sub ALIKEC_compare_dims(
  SEXP target, SEXP current, SEXP tar_obj, SEXP cur_obj,
  struct ALIKEC_settings set
) {
  // Invalid dims

  if(
    (TYPEOF(target) != INTSXP && target != R_NilValue) ||
    (TYPEOF(current) != INTSXP && current != R_NilValue)
  )
    return ALIKEC_alike_attr(target, current, "dim", set, 1, 0);

  // Dims -> implicit class

  R_xlen_t target_len = xlength(target), target_len_cap;
  target_len_cap = target_len > (R_xlen_t) 3 ? (R_xlen_t) 3 : target_len;
  R_xlen_t current_len = xlength(current), current_len_cap;
  current_len_cap = current_len > (R_xlen_t) 3 ? (R_xlen_t) 3 : current_len;

  struct ALIKEC_res_sub res = ALIKEC_res_sub_def();
  const char * class_err_string = "";
  const char * class_err_base = "be %s (is %s)";

  if(target_len_cap > 1 && isVectorAtomic(tar_obj)) {
    if(current == R_NilValue) {  // current is matrix/array
      class_err_string = CSR_smprintf4(
        ALIKEC_MAX_CHAR, class_err_base,
        target_len_cap > 2 ? "array" : "matrix",
        CHAR(asChar(ALIKEC_mode(cur_obj))), "", ""
      );
    } else if(isVectorAtomic(cur_obj) && current_len_cap != target_len_cap) {
      // target is matrix/array
      class_err_string = CSR_smprintf4(
        ALIKEC_MAX_CHAR, class_err_base,
        target_len_cap > 2 ? "array" : "matrix",
        current_len_cap == 2 ?
          "matrix" : (current_len_cap == 1 ? "vector" : "array"),
        "", ""
      );
    } else if(!isVectorAtomic(current)) {
      class_err_string = CSR_smprintf4(
        ALIKEC_MAX_CHAR, class_err_base,
        CHAR(asChar(ALIKEC_mode(tar_obj))), type2char(TYPEOF(cur_obj)), "", ""
    );}
  } else if (current_len_cap > 1 && isVectorAtomic(cur_obj)) {
    if(isVectorAtomic(tar_obj)) {
      class_err_string = CSR_smprintf4(
        ALIKEC_MAX_CHAR, class_err_base, CHAR(asChar(ALIKEC_mode(tar_obj))),
        current_len_cap == 2 ? "matrix" : "array", "", ""
      );
    } else {
      class_err_string = CSR_smprintf4(
        ALIKEC_MAX_CHAR, class_err_base, CHAR(asChar(ALIKEC_mode(tar_obj))),
        CHAR(asChar(ALIKEC_mode(cur_obj))), "", ""
    );}
  }
  if(class_err_string[0]) {
    res.success = 0;
    res.lvl = 1;
    res.message.message = class_err_string;
    return res;
  }
  // Normal dim checking

  if(current == R_NilValue) {
    res.success = 0;
    res.message.message = "have a \"dim\" attribute";
    return res;
  }
  if(target_len != current_len) {
    res.success = 0;
    res.message.message = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "have %s dimension%s (has %s)",
      CSR_len_as_chr(target_len), target_len == (R_xlen_t) 1 ? "" : "s",
      CSR_len_as_chr(current_len), ""
    );
    return res;
  }
  R_xlen_t attr_i;
  int tar_dim_val;

  for(attr_i = (R_xlen_t)0; attr_i < target_len; attr_i++) {
    tar_dim_val = INTEGER(target)[attr_i];
    const char * tar_dim_chr = CSR_len_as_chr((R_xlen_t)tar_dim_val);
    char * err_dim1, * err_dim2;

    if(tar_dim_val && tar_dim_val != INTEGER(current)[attr_i]) {
      if(target_len == 2) {  // Matrix
        err_dim1 = "";
        const char * err_dimtmp;
        switch(attr_i) {
          case (R_xlen_t) 0: err_dimtmp = "row%s"; break;
          case (R_xlen_t) 1: err_dimtmp = "column%s"; break;
          default:
            error(
              "Logic error: inconsistent matrix dimensions; contact maintainer."
            );
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
      res.success = 0;
      res.message.message = CSR_smprintf4(
        ALIKEC_MAX_CHAR, "have %s%s %s (has %s)",
        (const char *) err_dim1, tar_dim_chr, (const char *) err_dim2,
        CSR_len_as_chr((R_xlen_t)(INTEGER(current)[attr_i]))
      );
      return res;
  } }
  return ALIKEC_alike_attr(target, current, "dim", set, 1, 1);
}
SEXP ALIKEC_compare_dim_ext(
  SEXP target, SEXP current, SEXP tar_obj, SEXP cur_obj
) {
  return ALIKEC_res_sub_as_sxp(
    ALIKEC_compare_dims(
      target, current, tar_obj, cur_obj, ALIKEC_set_def("")
  ) );
}

/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Implements comparing character vectors element for element:
  - allowing zero length strings in `target` to match any length string in `current`
  - zero length `target` to match any `current`

This is used to compare names, row.names, etc.

Return value is either a zero length string if comparison successfull, or a
string containing `%s` that can then be used to sprintf in the name of the
object being compared.
*/
int ALIKEC_are_special_char_attrs_internal(SEXP target, SEXP current) {
  SEXPTYPE cur_type, tar_type;
  R_xlen_t tar_len;
  tar_type = TYPEOF(target);
  cur_type = TYPEOF(current);

  return tar_type != cur_type || tar_type != STRSXP || tar_type != INTSXP ||
    ((tar_len = XLENGTH(target)) && tar_len != XLENGTH(current));
}
/*
Used to construct messages like:

`names(object[[1]]$a)[1]` should be "cat" (is "rat")
an underlying assumption is that the only attributes that end up coming
here are the special ones that have accessor functions.

Note that the calling function is responsible for handling parens so as to
allow for stuff like: `names(dimnames(object))` and of subbing in the attribute
names.
*/
struct ALIKEC_res_sub ALIKEC_compare_special_char_attrs_internal(
  SEXP target, SEXP current, struct ALIKEC_settings set, int strict
) {
  struct ALIKEC_res res = ALIKEC_alike_internal(target, current, set);
  struct ALIKEC_res_sub res_sub = ALIKEC_res_sub_def();

  // Special character attributes must be alike; not sure the logic here is
  // completely correct, will have to verify

  if(!res.success) {
    res_sub.success = 0;
    res_sub.message = res.message;
    return res_sub;
  }
  // But also have contraints on values

  SEXPTYPE cur_type = TYPEOF(current), tar_type = TYPEOF(target);
  R_xlen_t cur_len, tar_len, i;

  // should have been handled previously
  if(tar_type != cur_type) error("Logic Error 266");
  // zero len match to anything
  else if (!(tar_len = XLENGTH(target))) return res_sub;
  // should have been handled previously
  else if ((cur_len = XLENGTH(current)) != tar_len) error("Logic error 268");
  else if (tar_type == INTSXP) {
    if(!R_compute_identical(target, current, 16)){
      res_sub.success=0;
      res_sub.message.message = "be identical to target";
    }
    return res_sub;
  } else if (tar_type == STRSXP) {
    // Only determine what name is wrong if we know there is a mismatch since we
    // have to loop thorugh each value.  Zero length targets match anything
    // unless in strict mode

    if(!R_compute_identical(target, current, 16)) {
      for(i = (R_xlen_t) 0; i < tar_len; i++) {
        const char * cur_name_val = CHAR(STRING_ELT(current, i));
        const char * tar_name_val = CHAR(STRING_ELT(target, i));
        if(         // check dimnames names match
          (strict || tar_name_val[0]) && strcmp(tar_name_val, cur_name_val) != 0
        ) {
          res_sub.success=0;
          res_sub.message.wrap = CSR_smprintf4(
            ALIKEC_MAX_CHAR, "%%s[%s]", CSR_len_as_chr((R_xlen_t)(i + 1)),
            "", "", ""
          );
          res_sub.message.message = CSR_smprintf4(
            ALIKEC_MAX_CHAR, "be \"%s\" (is \"%s\")",
            tar_name_val, cur_name_val, "", ""
          );
          break;
    } } }
    return res_sub;
  }
  error("Logic Error in compare_special_char_attrs; contact maintainer");
}
// External version for unit testing

SEXP ALIKEC_compare_special_char_attrs(SEXP target, SEXP current) {
  return ALIKEC_res_sub_as_sxp(
    ALIKEC_compare_special_char_attrs_internal(
      target, current, ALIKEC_set_def(""), 0
    )
  );
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Compare dimnames
*/
struct ALIKEC_res_sub ALIKEC_compare_dimnames(
  SEXP prim, SEXP sec, struct ALIKEC_settings set
) {
  struct ALIKEC_res_sub res = ALIKEC_res_sub_def();
  if(sec == R_NilValue) {
    res.success = 0;
    res.message.message = "have a \"dimnames\" attribute";
    return res;
  }
  SEXP prim_names = getAttrib(prim, R_NamesSymbol);
  SEXP sec_names = getAttrib(sec, R_NamesSymbol);
  R_xlen_t prim_len, sec_len;
  SEXPTYPE prim_type = TYPEOF(prim);
  if( // not a standard dimnames attribute
    prim_type != TYPEOF(sec) || prim_type != VECSXP ||
    ((prim_len = XLENGTH(prim)) && prim_len != (sec_len = XLENGTH(sec))) ||
    (
      prim_names != R_NilValue &&
      !ALIKEC_are_special_char_attrs_internal(prim_names, sec_names)
    )
  ) {
    struct ALIKEC_res res_tmp = ALIKEC_alike_internal(prim, sec, set);
    if(!res_tmp.success) {
      res.success = 0;
      res.message = res_tmp.message;
      res.message.wrap = CSR_smprintf4(
        ALIKEC_MAX_CHAR, "dimnames(%s)", res.message.wrap, "", "", ""
      );
    }
    return res;
  }

  /* The following likely doesn't need to be done for every dimnames so there
  probably is some optimization to be had here, should look into it if it
  seems slow; for example, `xlength` will cycle through all values, and so will
  the checking all attributes; also, do we really need to check whether dimnames
  has attributes other than names?*/

  SEXP prim_attr = ATTRIB(prim), sec_attr = ATTRIB(sec);

  /*
  Check that all `dimnames` attributes other than `names` that are both in
  target and current are alike; this is also a double loop that could be
  optimized; can't do the normal check because we need to leave out the
  `names` attribute from the comparison.  This could be simplified if we had
  an attribute comparison function that could skip a particular attribute.
  */

  SEXP prim_attr_cpy, sec_attr_cpy;
  for(
    prim_attr_cpy = prim_attr; prim_attr_cpy != R_NilValue;
    prim_attr_cpy = CDR(prim_attr_cpy)
  ) {
    SEXP prim_tag_symb = TAG(prim_attr_cpy);
    const char * prim_tag = CHAR(PRINTNAME(prim_tag_symb));
    int do_continue = 0;
    if(prim_tag_symb == R_NamesSymbol) continue;
    for(
      sec_attr_cpy = sec_attr; sec_attr_cpy != R_NilValue;
      sec_attr_cpy = CDR(sec_attr_cpy)
    ) {
      if(prim_tag_symb == TAG(sec_attr_cpy)) {
        struct ALIKEC_res res_tmp = ALIKEC_alike_internal(
          CAR(prim_attr_cpy), CAR(sec_attr_cpy), set
        );
        if(!res_tmp.success) {
          res.success = 0;
          res.message.message =
            "be `alike` the corresponding element in target.";
          res.message.wrap = CSR_smprintf4(
            ALIKEC_MAX_CHAR, "attr(dimnames(%s), \"%s\")",
            res.message.wrap, prim_tag, "", ""
          );
          return res;
        }
        do_continue = 1;
        break;
    } }
    if(do_continue) continue;
    // missing attribute
    res.success = 0;
    res.message.wrap = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "attr(dimnames(%s), \"%s\")",
      res.message.wrap, prim_tag, "", ""
    );
    res.message.message = "not be missing";
    return res;
  }
  // Compare actual dimnames attr

  if(!prim_len) return res;  // zero length list matches anything

  // dimnames names

  if(prim_names != R_NilValue) {
    struct ALIKEC_res_sub dimnames_name_comp =
      ALIKEC_compare_special_char_attrs_internal(prim_names, sec_names, set, 0);
    if(!dimnames_name_comp.success) {
      dimnames_name_comp.message.wrap = CSR_smprintf4(
        ALIKEC_MAX_CHAR, dimnames_name_comp.message.wrap,
        "names(dimnames(%s))", "", "", ""
      );
      return dimnames_name_comp;
  } }
  // look at dimnames themselves

  SEXP prim_obj, sec_obj;
  R_xlen_t attr_i;

  for(attr_i = (R_xlen_t) 0; attr_i < prim_len; attr_i++) {
    if((prim_obj = VECTOR_ELT(prim, attr_i)) != R_NilValue) {
      sec_obj = VECTOR_ELT(sec, attr_i);

      struct ALIKEC_res_sub dimnames_comp =
        ALIKEC_compare_special_char_attrs_internal(
          prim_obj, sec_obj, set, 0
        );
      if(!dimnames_comp.success) {
        const char * err_tok;
        if(prim_len == 2) { // matrix like
          switch(attr_i) {
            case (R_xlen_t) 0: err_tok = "rownames(%s)"; break;
            case (R_xlen_t) 1: err_tok = "colnames(%s)"; break;
            default:
             error(
               "Logic Error: dimnames dimension mismatch; contact maintainer."
             );
          }
        } else {
          err_tok = CSR_smprintf4(
            ALIKEC_MAX_CHAR, "dimnames(%%s)[[%s]]",
            CSR_len_as_chr(attr_i + (R_xlen_t) 1), "", "", ""
          );
        }
        dimnames_comp.message.wrap = CSR_smprintf4(
          ALIKEC_MAX_CHAR, dimnames_comp.message.wrap, err_tok, "", "", ""
        );
        return dimnames_comp;
  } } }
  return res;
}
SEXP ALIKEC_compare_dimnames_ext(SEXP prim, SEXP sec) {
  return(
    ALIKEC_res_sub_as_sxp(
      ALIKEC_compare_dimnames(prim, sec, ALIKEC_set_def(""))
  ) );
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Compare time series attribute; some day will have to actually get an error
display that can handle floats
*/
struct ALIKEC_res_sub ALIKEC_compare_ts(
  SEXP target, SEXP current, struct ALIKEC_settings set
) {
  SEXPTYPE tar_type = TYPEOF(target);
  struct ALIKEC_res_sub res = ALIKEC_res_sub_def();
  if(
    tar_type == REALSXP && TYPEOF(current) == tar_type &&
    XLENGTH(target) == 3 && XLENGTH(current) == 3
  ) {
    double * tar_real = REAL(target), * cur_real = REAL(current);

    for(R_xlen_t i = 0; i < 3; i++) {
      if(tar_real[i] != 0 && tar_real[i] != cur_real[i]) {
        res.success = 0;
        char * tar_num = R_alloc(21, sizeof(char));
        char * cur_num = R_alloc(21, sizeof(char));
        snprintf(tar_num, 20, "%g", tar_real[i]);
        snprintf(cur_num, 20, "%g", cur_real[i]);
        res.message.message = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "be %s (is %s)", tar_num, cur_num, "", ""
        );
        res.message.wrap = CSR_smprintf4(
          ALIKEC_MAX_CHAR, "tsp(%%s)[%s]", CSR_len_as_chr(i + 1), "", "", ""
        );
        return res;
    } }
  } else {
    return ALIKEC_alike_attr(target, current, "tsp", set, 1, 0);
  }
  return res;
}
/*
external
*/
SEXP ALIKEC_compare_ts_ext(SEXP target, SEXP current) {
  return ALIKEC_res_sub_as_sxp(
    ALIKEC_compare_ts(target, current, ALIKEC_set_def())
  );
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/

struct ALIKEC_res_sub  ALIKEC_compare_levels(
  SEXP target, SEXP current, struct ALIKEC_settings set
) {
  if(TYPEOF(target) == STRSXP && TYPEOF(current) == STRSXP) {
    struct ALIKEC_res_sub res = ALIKEC_compare_special_char_attrs_internal(
      target, current, set, 0
    );
    if(!res.success) {
      res.message.wrap = CSR_smprintf4(
        ALIKEC_MAX_CHAR, res.message.wrap, "levels(%s)", "", "", ""
      );
    }
    return res;
  }
  return ALIKEC_alike_attr(target, current, "levels", set, 1, 0);
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
normal attribute comparison; must be alike with some exceptions for
reference attributes.

Note that we feed missing attributes as R_NilValue, which is unambiguous since
`attr(x, y) <- NULL` unsets attributes so there shouldn't be an actual attribute
with a NULL value

Since reference attrs are references to other objects that we cannot directly
compare, and could for all intents and purposes be "alike" in the typical R
sense, i.e. not pointing to exact same memory location, but otherwise the same,
we consider the fact that they are of the same type a match for alike purposes.
This is a bit of a cop out, but the situations where these attributes alone
would cause a mismatch seem pretty rare
*/

struct ALIKEC_res_sub ALIKEC_compare_attributes_internal_simple(
  SEXP target, SEXP current, const char * attr_name,
  struct ALIKEC_settings set
) {
  R_xlen_t tae_val_len, cae_val_len;
  SEXPTYPE tae_type = TYPEOF(target), cae_type = TYPEOF(current);
  tae_val_len = XLENGTH(target);
  cae_val_len = XLENGTH(current);

  struct ALIKEC_res_sub res = ALIKEC_res_sub_def();

  // Start with all cases that don't produce errors

  int dont_check = !set->attr_mode && !tae_val_len;
  int both_null = tae_type == NILSXP && cae_type == NILSXP;
  int ref_obj = set->attr_mode && (
    tae_type == EXTPTRSXP || tae_type == WEAKREFSXP ||
    tae_type == BCODESXP || tae_type == ENVSXP
  );
  if(dont_check || both_null || ref_obj) return res;

  // Now checks that produce errors

  if(tae_type == NILSXP || cae_type == NILSXP) {
    res.success = 0;
    res.message.message = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "%shave attribute \"%s\"",
      tae_type == NILSXP ? "not " : "", attr_name, "", ""
    );
  } else if(tae_type != cae_type) {
    res.success = 0;
    res.message.message = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "be %s (is %s)", type2char(tae_type),
      type2char(cae_type), "", ""
    );
    res.message.wrap = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "attr(%%s, \"%s\")",
      attr_name, "", "", ""
    );
  } else if (
    (tae_val_len = xlength(target)) != (cae_val_len = xlength(current))
  ) {
    if(set->attr_mode || tae_val_len) {
      res.success = 0;
      res.message.message = CSR_smprintf4(
        ALIKEC_MAX_CHAR, "be %s (is %s)", CSR_len_as_chr(tae_val_len),
        CSR_len_as_chr(cae_val_len), "", ""
      );
      res.message.wrap = CSR_smprintf4(
        ALIKEC_MAX_CHAR, "length(attr(%%s, \"%s\"))",
        attr_name, "", "", ""
    );}
  } else {
    res = ALIKEC_alike_attr(target, current, attr_name, set, 0, 0);
  }
  return res;
}
/* Used by alike to compare attributes;

Code originally inspired by `R_compute_identical` (thanks R CORE)
*/

struct ALIKEC_res_sub ALIKEC_compare_attributes_internal(
  SEXP target, SEXP current, struct ALIKEC_settings set
) {
  /*
  Array to store major errors from, in order:
    0. class,
    1. tsp
    2. dim
    3. names
    4. rownames
    5. dimnames
    6. other
    7. missing*/

  const char * err_major[8] = {"", "", "", "", "", "", "", ""};
  const char * err_wrap[8] = {"%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s"};
  struct ALIKEC_res_sub res_attr = ALIKEC_res_sub_def(), res_sub;

  // Note we don't protect these because target and curent should come in
  // protected so every SEXP under them should also be protected

  SEXP tar_attr, cur_attr, prim_attr, sec_attr;
  int rev = 0, is_df = 0;

  tar_attr = ATTRIB(target);
  cur_attr = ATTRIB(current);

  /*
  Here we need to `rev` so that our double loop works; if we don't rev and
  `target` has no attributes then we wouldn't check anything.  Since we know
  in that case we would fail at the first `current` attribute, maybe we should
  simplify...
  */

  if(tar_attr == R_NilValue && cur_attr == R_NilValue) return res_attr;
  else if(tar_attr == R_NilValue) {
    rev = 1;
    prim_attr = cur_attr;
    sec_attr = tar_attr;
    if(set->attr_mode == 2) {
      err_major[7] = "have attributes";
    }
  } else {
    prim_attr = tar_attr;
    sec_attr = cur_attr;
    if(cur_attr == R_NilValue) {
      err_major[7] = CSR_smprintf4(
        ALIKEC_MAX_CHAR, "not have attributes (has %s attributes)",
        CSR_len_as_chr(xlength(cur_attr)), "", "", ""
  );} }
  /*
  Mark that we're in attribute checking so we can handle recursions within
  attributes properly
  */
  set->in_attr++;

  /*
  Loop through all attr combinations; maybe could be made faster by
  reducing the second loop each time a match is found, though this would require
  duplication of the attributes (likely faster for items with lots of attributes,
  but slower for those with few; ideally need a way to duplicate the LISTSXP but
  not the contents - I guess wouldn't be too hard to implement).

  Alternate: use hash tables, though likely not worth it unless more than 25
  attributes, which should be rare
  */
  SEXP prim_attr_el, sec_attr_el;
  size_t sec_attr_counted = 0, sec_attr_count = 0, prim_attr_count = 0;

  for(
    prim_attr_el = prim_attr; prim_attr_el != R_NilValue;
    prim_attr_el = CDR(prim_attr_el)
  ) {
    SEXP prim_tag = TAG(prim_attr_el);
    const char * tx = CHAR(PRINTNAME(prim_tag));
    prim_attr_count++;
    SEXP sec_attr_el_tmp = R_NilValue;

    for(
      sec_attr_el = sec_attr; sec_attr_el != R_NilValue;
      sec_attr_el = CDR(sec_attr_el)
    ) {
      if(!sec_attr_counted) sec_attr_count++;
      if(prim_tag == TAG(sec_attr_el)) {
        sec_attr_el_tmp = sec_attr_el;
        if(sec_attr_counted) break;  // Don't need to do full loop since we did it once already
    } }
    sec_attr_counted = 1;
    sec_attr_el = sec_attr_el_tmp;

    if(prim_attr_el == R_NilValue) { // NULL attrs shouldn't be possible
      error(
        "Logic Error: attribute %s is NULL for `%s`", tx,
        rev ? "current" : "target"
    );}

    // Undo reverse now that we've gone through double loop

    SEXP tar_attr_el, tar_attr_el_val, cur_attr_el, cur_attr_el_val, tar_tag;
    if(rev) {
      tar_attr_el = sec_attr_el;
      cur_attr_el = prim_attr_el;
      tar_tag = TAG(sec_attr_el);
    } else {
      tar_attr_el = prim_attr_el;
      cur_attr_el = sec_attr_el;
      tar_tag = prim_tag;
    }
    // No match only matters if target has attrs or in strict mode
    if(
      (
        (tar_attr_el == R_NilValue && set->attr_mode == 2) ||
        cur_attr_el == R_NilValue
      ) && !err_major[7][0]
    ) {
      err_major[7] = CSR_smprintf4(
        ALIKEC_MAX_CHAR, "%shave attribute \"%s\"",
        (cur_attr_el == R_NilValue ? "" : "not "), tx, "", ""
      );
    }
    cur_attr_el_val = cur_attr_el != R_NilValue ? CAR(cur_attr_el) : R_NilValue;
    tar_attr_el_val = tar_attr_el != R_NilValue ? CAR(tar_attr_el) : R_NilValue;

    // = Baseline Check ========================================================

    if(set->attr_mode && cur_attr_el_val != R_NilValue && !strlen(err_major[6])) {
      res_sub = ALIKEC_compare_attributes_internal_simple(
        tar_attr_el_val, cur_attr_el_val, tx, set
      );
      if(!res_sub.success) {
        err_major[6] = res_sub.message.message;
        err_wrap[6] = res_sub.message.wrap;
      }
    // = Custom Checks =========================================================

    /* see alike documentation for explanations of how the special
    attributes in this section are compared */

    } else {
      // - Class ---------------------------------------------------------------

      /* Class errors always trump all others so no need to calculate further;
      for every other error we have to keep going in case we eventually find a
      class error*/

      if(tar_tag == R_ClassSymbol) {
        SEXP cur_attr_el_val_tmp =
          PROTECT(ALIKEC_class(rev ? target : current, cur_attr_el_val));
        SEXP tar_attr_el_val_tmp =
          PROTECT(ALIKEC_class(!rev ? target : current, tar_attr_el_val));
        struct ALIKEC_res_sub class_comp = ALIKEC_compare_class(
          tar_attr_el_val_tmp, cur_attr_el_val_tmp, set
        );
        is_df = class_comp.df;
        UNPROTECT(2);
        if(!class_comp.success) {
          err_major[0] = class_comp.message.message;
          err_wrap[0] = class_comp.message.wrap;
          break;
        }
      // - Names ---------------------------------------------------------------

      } else if (tar_tag == R_NamesSymbol || tar_tag == R_RowNamesSymbol) {
        struct ALIKEC_res_sub name_comp =
          ALIKEC_compare_special_char_attrs_internal(
            tar_attr_el_val, cur_attr_el_val, set, 0
          );
        if(!name_comp.success) {
          char * tx_name = CSR_smprintf4(
            ALIKEC_MAX_CHAR, "%s%%s%s",
            tar_tag == R_NamesSymbol ? "names(" : "rownames(",
            ")", "", ""
          );
          int err_ind = tar_tag == R_NamesSymbol ? 3 : 4;
          err_major[err_ind] = name_comp.message.message;
          err_wrap[err_ind] = CSR_smprintf4(
            ALIKEC_MAX_CHAR, tx_name, name_comp.message.wrap, "", "", ""
          );
        }
        continue;
      // - Dims ----------------------------------------------------------------

      } else if (tar_tag == R_DimSymbol && set->attr_mode == 0) {
        int err_ind = 2;

        struct ALIKEC_res_sub dim_comp = ALIKEC_compare_dims(
          tar_attr_el_val, cur_attr_el_val, target, current, set
        );
        // implicit class error upgrades to major error

        if(dim_comp.lvl) err_ind = 0;

        err_major[err_ind] = dim_comp.message.message;
        err_wrap[err_ind] = dim_comp.message.wrap;

      // - dimnames ------------------------------------------------------------

      } else if (tar_tag == R_DimNamesSymbol) {
        struct ALIKEC_res_sub dimname_comp = ALIKEC_compare_dimnames(
          tar_attr_el_val, cur_attr_el_val, set
        );
        err_major[5] = dimname_comp.message.message;
        err_wrap[5] = dimname_comp.message.wrap;

      // - levels --------------------------------------------------------------

      } else if (tar_tag == R_LevelsSymbol) {
        struct ALIKEC_res_sub levels_comp = ALIKEC_compare_levels(
          tar_attr_el_val, cur_attr_el_val, set
        );
        err_major[6] = levels_comp.message.message;
        err_wrap[6] = levels_comp.message.wrap;

      // - tsp -----------------------------------------------------------------

      } else if (tar_tag == R_TspSymbol) {

        struct ALIKEC_res_sub ts_comp = ALIKEC_compare_ts(
          tar_attr_el_val, cur_attr_el_val, set
        );
        err_major[1] = ts_comp.message.message;
        err_wrap[1] = ts_comp.message.wrap;

      // - normal attrs --------------------------------------------------------

      } else {
        struct ALIKEC_res_sub attr_comp =
          ALIKEC_compare_attributes_internal_simple(
            tar_attr_el_val, cur_attr_el_val, tx, set
          );
        err_major[6] = attr_comp.message.message;
        err_wrap[6] = attr_comp.message.wrap;
      }
  } }
  // If in strict mode, must have the same number of attributes

  if(set->attr_mode == 2 && prim_attr_count != sec_attr_count) {
    err_major[7] = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "have %s attribute%s (has %s)",
      CSR_len_as_chr(prim_attr_count), prim_attr_count != 1 ? "s" : "",
      CSR_len_as_chr(sec_attr_count), ""
  );}
  // Now determine which error to throw, if any

  if(!set->in_attr)
    error("Logic Error: attribute depth counter corrupted; contact maintainer");
  set->in_attr--;

  res_attr.df = is_df;
  int i;
  for(i = 0; i < 8; i++) {
    if(err_major[i][0] && (!rev || (rev && set->attr_mode == 2))) {
      res_attr.success = 0;
      res_attr.message.message = err_major[i];
      res_attr.message.wrap = err_wrap[i];
      res_attr.lvl = i;
      return res_attr;
  } }
  // Passed

  return res_attr;
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
external interface for compare attributes
*/
SEXP ALIKEC_compare_attributes(SEXP target, SEXP current, SEXP attr_mode) {
  SEXPTYPE attr_mode_type = TYPEOF(attr_mode);

  if(
    (attr_mode_type != INTSXP && attr_mode_type != REALSXP) ||
    XLENGTH(attr_mode) != 1
  )
    error("Argument `mode` must be a one length integer like vector");

  struct ALIKEC_settings set = ALIKEC_set_def("");
  set->attr_mode = asInteger(attr_mode);

  struct ALIKEC_res_sub comp_res =
    ALIKEC_compare_attributes_internal(target, current, set);
  return ALIKEC_res_sub_as_sxp(comp_res);
}
