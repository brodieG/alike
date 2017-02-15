#include "alike.h"

/*
compare types, accounting for "integer like" numerics; empty string means
success, otherwise outputs an a character string explaining why the types are
not alike
*/
struct ALIKEC_res_strings ALIKEC_type_alike_internal(
  SEXP target, SEXP current, int mode, R_xlen_t max_len
) {
  SEXPTYPE tar_type, cur_type, tar_type_raw, cur_type_raw;
  int int_like = 0;
  tar_type_raw = TYPEOF(target);
  cur_type_raw = TYPEOF(current);

  struct ALIKEC_res_strings res =
    (struct ALIKEC_res_strings) {.target="", .actual=""};

  if(tar_type_raw == cur_type_raw) return res;

  if(
    mode == 0 && (
      (
        tar_type_raw == INTSXP && (
          max_len < 0 ||
          (XLENGTH(target) <= max_len && XLENGTH(current) <= max_len)
      ) ) || (
        tar_type_raw == CLOSXP || tar_type_raw == SPECIALSXP ||
        tar_type_raw == BUILTINSXP
    ) )
  ) {
    tar_type = ALIKEC_typeof_internal(target);
    cur_type = ALIKEC_typeof_internal(current);
    int_like = 1;
  } else {
    tar_type = tar_type_raw;
    cur_type = cur_type_raw;
  }
  if(tar_type == cur_type) return res;
  if(
    cur_type == INTSXP && mode < 2 &&
    (tar_type == INTSXP || tar_type == REALSXP)
  ) {
    return res;
  }
  const char * what;
  if(mode == 0 && int_like) {
    what = "integer-like";
  } else if (mode < 2 && tar_type == REALSXP) {
    what = "numeric";
  } else if (mode == 0 && tar_type == CLOSXP) {
    what = "function";
  } else {
    what = type2char(tar_type);
  }
  return (struct ALIKEC_res_strings) {
    .target=CSR_smprintf4(ALIKEC_MAX_CHAR, "be type \"%s\"", what, "", "", ""),
    .actual=CSR_smprintf4(
      ALIKEC_MAX_CHAR, "is \"%s\"", type2char(cur_type), "", "", ""
    )
  };
}
SEXP ALIKEC_type_alike(SEXP target, SEXP current, SEXP mode, SEXP max_len) {
  SEXPTYPE mod_type, max_len_type;
  struct ALIKEC_res_strings res;

  mod_type = TYPEOF(mode);
  max_len_type = TYPEOF(max_len);

  if((mod_type != INTSXP && mod_type != REALSXP) || XLENGTH(mode) != 1)
    error("Argument `mode` must be a one length integer like vector");
  if((max_len_type != INTSXP && max_len_type != REALSXP) || XLENGTH(max_len) != 1)
    error("Argument `mode` must be a one length integer like vector");

  res = ALIKEC_type_alike_internal(
    target, current, asInteger(mode), asInteger(max_len)
  );
  if(res.target[0]) {
    return(ALIKEC_res_strings_to_SEXP(res));
  } else {
    return ScalarLogical(1);
  }
}
SEXP ALIKEC_type_alike_fast(SEXP target, SEXP current) {
  struct ALIKEC_res_strings res;
  res = ALIKEC_type_alike_internal(target, current, 0, 100);
  if(res.target[0]) {
    return(ALIKEC_res_strings_to_SEXP(res));
  } else {
    return ScalarLogical(1);
  }
}

/* - typeof ----------------------------------------------------------------- */

SEXPTYPE ALIKEC_typeof_internal(SEXP object) {
  double * obj_real;
  SEXPTYPE obj_type = TYPEOF(object);

  switch(obj_type) {
    case REALSXP:
      {
        R_xlen_t obj_len = XLENGTH(object), i;
        obj_real = REAL(object);
        /*
        could optimize this more by using the magic number tricks or bit
        fiddling, but at end of day this still wouldn't be fast enough to
        realistically use on a very large vector, so it doesn't really matter
        */
        for(i = 0; i < obj_len; i++)
          if(!isnan(obj_real[i]) && obj_real[i] != (int)obj_real[i])
            return REALSXP;
        return INTSXP;
      }
      break;
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
      return CLOSXP;
  }
  return(obj_type);
}
/*
External interface for typeof, here mostly so we don't have to deal with the
SEXP return in the internal use case
*/

SEXP ALIKEC_typeof(SEXP object) {
  return mkString(type2char(ALIKEC_typeof_internal(object)));
}
