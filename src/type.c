#include "alike.h"

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
  return CSR_smprintf4(
    ALIKEC_MAX_CHAR,
    "type \"%s\" (is \"%s\")", what, type2char(cur_type), "", ""
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
