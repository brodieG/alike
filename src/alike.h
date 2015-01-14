#include <R.h>
#include <Rinternals.h>
#include <ctype.h>

// - Main Funs -----------------------------------------------------------------

SEXP ALIKEC_alike (SEXP target, SEXP current, SEXP int_mode, SEXP int_tol, SEXP attr_mode);
SEXP ALIKEC_alike_fast (SEXP target, SEXP current);

SEXP ALIKEC_typeof(SEXP object, SEXP tolerance);
SEXP ALIKEC_typeof_fast(SEXP object);
SEXP ALIKEC_type_alike(SEXP target, SEXP current, SEXP mode, SEXP tolerance);
SEXP ALIKEC_type_alike_fast(SEXP target, SEXP current);

// - Internal Funs -------------------------------------------------------------

SEXPTYPE ALIKEC_typeof_internal(SEXP object, double tolerance);
const char *  ALIKEC_type_alike_internal(SEXP target, SEXP current, int mode, double tolerance);

SEXP ALIKEC_compare_attributes(SEXP target, SEXP current, SEXP attr_mode);
SEXP ALIKEC_compare_special_char_attrs(SEXP target, SEXP current);
const char * ALIKEC_compare_attributes_internal(SEXP target, SEXP current, int attr_mode, int * is_df, int * err_lvl);
SEXP ALIKEC_compare_class_ext(SEXP prim, SEXP sec, SEXP rev);
SEXP ALIKEC_compare_dimnames_ext(SEXP prim, SEXP sec);
SEXP ALIKEC_compare_dim_ext(SEXP prim, SEXP sec, SEXP target, SEXP current, SEXP rev);

// - Utility Funs --------------------------------------------------------------

SEXP ALIKEC_mode(SEXP obj);
SEXP ALIKEC_test(SEXP obj1);

// - Imported Funs -------------------------------------------------------------

const char * (*CSR_smprintf4)(
  size_t, const char *, const char *, const char *, const char *, const char *
);
const char * (*CSR_len_as_chr)(R_xlen_t);
size_t (*CSR_len_chr_len)(R_xlen_t);

// - Init and pre-install Symbols ----------------------------------------------

SEXP ALIKEC_SYM_inherits;
SEXP ALIKEC_SYM_package;

// - Constants -----------------------------------------------------------------

#ifndef ALIKEC_MAX_CHAR
# define ALIKEC_MAX_CHAR 10000
#endif
