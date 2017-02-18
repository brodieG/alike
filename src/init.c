#include "alike.h"
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
  {"alike_ext", (DL_FUNC) &ALIKEC_alike_ext, 4},
  {"alike_fast1", (DL_FUNC) &ALIKEC_alike_fast1, 4},
  {"alike_fast2", (DL_FUNC) &ALIKEC_alike_fast2, 2},
  {"typeof", (DL_FUNC) &ALIKEC_typeof, 1},
  {"type_alike", (DL_FUNC) &ALIKEC_type_alike, 4},
  {"syntactic_names", (DL_FUNC) &ALIKEC_syntactic_names_exp, 1},
  {"compare_attributes", (DL_FUNC) &ALIKEC_compare_attributes, 3},
  {"test", (DL_FUNC) &ALIKEC_test, 1},
  {"test2", (DL_FUNC) &ALIKEC_test2, 2},
  {"is_valid_name_ext", (DL_FUNC) &ALIKEC_is_valid_name_ext, 1},
  {"is_dfish", (DL_FUNC) &ALIKEC_is_dfish_ext, 1},
  {"compare_names", (DL_FUNC) &ALIKEC_compare_special_char_attrs, 2},
  {"compare_dimnames", (DL_FUNC) &ALIKEC_compare_dimnames_ext, 2},
  {"compare_class", (DL_FUNC) &ALIKEC_compare_class_ext, 2},
  {"compare_dims", (DL_FUNC) &ALIKEC_compare_dim_ext, 5},
  {"compare_ts", (DL_FUNC) &ALIKEC_compare_ts_ext, 2},
  {"lang_alike", (DL_FUNC) &ALIKEC_lang_alike_ext, 3},
  {"lang_alike_chr", (DL_FUNC) &ALIKEC_lang_alike_chr_ext, 3},
  {"fun_alike", (DL_FUNC) &ALIKEC_fun_alike_ext, 2},
  {"deparse", (DL_FUNC) &ALIKEC_deparse_ext, 2},
  {"deparse_oneline", (DL_FUNC) &ALIKEC_deparse_oneline_ext, 3},
  {"pad", (DL_FUNC) &ALIKEC_pad_ext, 3},
  {"pad_or_quote", (DL_FUNC) &ALIKEC_pad_or_quote_ext, 3},
  {"match_call", (DL_FUNC) &ALIKEC_match_call, 3},
  {"abstract_ts", (DL_FUNC) &ALIKEC_abstract_ts, 2},
  {"env_track", (DL_FUNC) &ALIKEC_env_track_test, 2},
  {"msg_sort", (DL_FUNC) &ALIKEC_sort_msg, 1},
  {"msg_merge", (DL_FUNC) &ALIKEC_merge_msg, 1},
  {NULL, NULL, 0}
};

void R_init_alike(DllInfo *info)
{
  ALIKEC_SYM_package = install("package");
  ALIKEC_SYM_inherits = install("inherits");
  ALIKEC_SYM_paren_open = install("(");
  ALIKEC_SYM_tilde = install("~");
  ALIKEC_SYM_args = install("args");
  ALIKEC_SYM_deparse = install("deparse");
  ALIKEC_SYM_nlines = install("nlines");
  ALIKEC_SYM_widthcutoff = install("width.cutoff");
  ALIKEC_SYM_getOption = install("getOption");
  ALIKEC_SYM_matchcall = install("match.call");
  ALIKEC_SYM_current = install("current");
  ALIKEC_SYM_attributes = install("attributes");
  ALIKEC_SYM_attr = install("attr");
  ALIKEC_SYM_colnames = install("colnames");
  ALIKEC_SYM_length = install("length");
  ALIKEC_SYM_syntacticnames = install("syntacticnames");

  CSR_smprintf4 = (
    char *(*)(
      size_t, const char *, const char *, const char *,  const char *,
      const char *
    )
  ) R_GetCCallable("cstringr", "CSR_smprintf4");
  CSR_smprintf6 = (
    char *(*)(
      size_t, const char *, const char *, const char *,  const char *,
      const char *,  const char *,  const char *
    )
  ) R_GetCCallable("cstringr", "CSR_smprintf6");
  CSR_len_chr_len = (size_t(*)(R_xlen_t)) R_GetCCallable("cstringr", "CSR_len_chr_len");
  CSR_strmlen = (size_t(*)(const char *, size_t)) R_GetCCallable("cstringr", "CSR_strmlen");
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_RegisterCCallable(
    "alike", "ALIKEC_pad_or_quote", (DL_FUNC) ALIKEC_pad_or_quote
  );
  R_RegisterCCallable("alike", "ALIKEC_pad", (DL_FUNC) ALIKEC_pad);
  R_RegisterCCallable("alike", "ALIKEC_alike_ext", (DL_FUNC) ALIKEC_alike_ext);
  R_RegisterCCallable("alike", "ALIKEC_deparse", (DL_FUNC) ALIKEC_deparse);
  CSR_len_as_chr = (char * (*)(R_xlen_t)) R_GetCCallable("cstringr", "CSR_len_as_chr");
  CSR_strmcpy = (char * (*)(const char * str, size_t maxlen)) R_GetCCallable("cstringr", "CSR_strmcpy");
}

// void R_unload_alike(DllInfo *dll)
// {
//     UNPROTECT(2);
// }
