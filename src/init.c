#include "alike.h"
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
  {"alike", (DL_FUNC) &ALIKEC_alike, 5},
  {"alike_fast", (DL_FUNC) &ALIKEC_alike_fast, 2},
  {"typeof", (DL_FUNC) &ALIKEC_typeof, 2},
  {"typeof_fast", (DL_FUNC) &ALIKEC_typeof_fast, 1},
  {"type_alike", (DL_FUNC) &ALIKEC_type_alike, 4},
  {"type_alike_fast", (DL_FUNC) &ALIKEC_type_alike_fast, 2},
  {"compare_attributes", (DL_FUNC) &ALIKEC_compare_attributes, 3},
  {"test", (DL_FUNC) &ALIKEC_test, 1},
  {"compare_names", (DL_FUNC) &ALIKEC_compare_special_char_attrs, 2},
  {"compare_dimnames", (DL_FUNC) &ALIKEC_compare_dimnames_ext, 2},
  {"compare_class", (DL_FUNC) &ALIKEC_compare_class_ext, 3},
  {"compare_dims", (DL_FUNC) &ALIKEC_compare_dim_ext, 5},
  {NULL, NULL, 0}
};

void R_init_alike(DllInfo *info)
{
 /* Register the .C and .Call routines.
    No .Fortran() or .External() routines,
    so pass those arrays as NULL.
  */
  ALIKEC_SYM_package = install("package");
  ALIKEC_SYM_inherits = install("inherits");
  CSR_smprintf4 = (
    char *(*)(
      size_t, const char *, const char *, const char *,  const char *,  const char *
    )
  ) R_GetCCallable("cstringr", "CSR_smprintf4");
  CSR_len_chr_len = (size_t(*)(R_xlen_t)) R_GetCCallable("cstringr", "CSR_len_chr_len");
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_RegisterCCallable("alike", "ALIKEC_alike_fast", (DL_FUNC) ALIKEC_alike_fast);
  CSR_len_as_chr = (char * (*)(R_xlen_t)) R_GetCCallable("cstringr", "CSR_len_as_chr");
}

