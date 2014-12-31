#include <R.h>
#include <Rinternals.h>

SEXP ALIKEC_alike (SEXP target, SEXP current, SEXP int_mode, SEXP int_tol, SEXP attr_mode);
SEXP ALIKEC_alike_fast (SEXP target, SEXP current);
SEXP ALIKEC_typeof(SEXP object, SEXP tolerance);
SEXP ALIKEC_typeof_fast(SEXP object);
SEXP ALIKEC_type_alike(SEXP target, SEXP current, SEXP mode, SEXP tolerance);
SEXP ALIKEC_type_alike_fast(SEXP target, SEXP current);
SEXPTYPE ALIKEC_typeof_internal(SEXP object, double tolerance);
const char *  ALIKEC_type_alike_internal(SEXP target, SEXP current, int mode, double tolerance);
SEXP ALIKEC_compare_attributes(SEXP target, SEXP current, SEXP attr_mode);
SEXP ALIKEC_test(SEXP obj1);
SEXP ALIKEC_compare_special_char_attrs(SEXP target, SEXP current);

// - Init and pre-install Symbols ----------------------------------------------

SEXP ALIKEC_SYM_inherits;
SEXP ALIKEC_SYM_package;
