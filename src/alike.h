#include "pfhash.h"

#ifndef _ALIKEC_H
#define _ALIKEC_H

  // - Data Structures ---------------------------------------------------------

  struct ALIKEC_res {
    int success;
    char * message;
    int df;
  };
  struct ALIKEC_res_attr {
    int success;
    const char * message;
    int df;
    int lvl;
  };
  struct ALIKEC_settings_env {
    int stack_size;
    int stack_ind;
    int stack_mult;
    int stack_size_init;
    SEXP * env_stack;
    int debug;
  };
  struct ALIKEC_settings {
    int type_mode;
    double int_tolerance;
    int attr_mode;
    const char * prepend;
    int suppress_warnings;
    SEXP match_env;
    struct ALIKEC_settings_env * env_set;
    int no_rec;
    size_t in_attr;
  };

  // - Constants ---------------------------------------------------------------

  #define ALIKEC_MAX_CHAR 10000
  #define ALIKEC_MAX_ENVS 65536

  // - Main Funs ---------------------------------------------------------------

  SEXP ALIKEC_alike (
    SEXP target, SEXP current, SEXP type_mode, SEXP int_tol, SEXP attr_mode,
    SEXP suppress_warnings, SEXP match_env
  );
  SEXP ALIKEC_alike_fast (SEXP target, SEXP current);
  const char * ALIKEC_alike_internal(
    SEXP target, SEXP current, struct ALIKEC_settings * set
  );
  SEXP ALIKEC_typeof(SEXP object, SEXP tolerance);
  SEXP ALIKEC_typeof_fast(SEXP object);
  SEXP ALIKEC_type_alike(SEXP target, SEXP current, SEXP mode, SEXP tolerance);
  SEXP ALIKEC_type_alike_fast(SEXP target, SEXP current);

  // - Internal Funs -------------------------------------------------------------

  SEXPTYPE ALIKEC_typeof_internal(SEXP object, double tolerance);
  const char *  ALIKEC_type_alike_internal(
    SEXP target, SEXP current, int mode, double tolerance
  );
  SEXP ALIKEC_compare_attributes(SEXP target, SEXP current, SEXP attr_mode);
  SEXP ALIKEC_compare_special_char_attrs(SEXP target, SEXP current);
  struct ALIKEC_res_attr ALIKEC_compare_attributes_internal(
    SEXP target, SEXP current, struct ALIKEC_settings * set
  );
  SEXP ALIKEC_compare_class_ext(SEXP prim, SEXP sec);
  SEXP ALIKEC_compare_dimnames_ext(SEXP prim, SEXP sec);
  SEXP ALIKEC_compare_dim_ext(SEXP prim, SEXP sec, SEXP target, SEXP current);
  const char * ALIKEC_lang_alike_internal(
    SEXP target, SEXP current, SEXP match_env
  );
  SEXP ALIKEC_lang_alike_ext(SEXP target, SEXP current, SEXP match_env);
  const char * ALIKEC_lang_alike_rec(
    SEXP target, SEXP cur_par, pfHashTable * tar_hash, pfHashTable * cur_hash,
    pfHashTable * rev_hash, size_t * tar_varnum, size_t * cur_varnum,
    int formula, SEXP match_call, SEXP match_env
  );
  const char * ALIKEC_fun_alike_internal(SEXP target, SEXP current);
  SEXP ALIKEC_fun_alike_ext(SEXP target, SEXP current);
  SEXP ALIKEC_compare_ts_ext(SEXP target, SEXP current);

  // - Utility Funs --------------------------------------------------------------

  struct ALIKEC_settings * ALIKEC_set_def();
  SEXP ALIKEC_mode(SEXP obj);
  SEXP ALIKEC_test();
  SEXP ALIKEC_deparse_ext(SEXP obj, SEXP lines);
  const char * ALIKEC_deparse(SEXP obj, R_xlen_t lines);
  SEXP ALIKEC_match_call(SEXP call, SEXP match_call, SEXP env);
  SEXP ALIKEC_findFun(SEXP symbol, SEXP rho);
  SEXP ALIKEC_string_or_true(const char * var);
  SEXP ALIKEC_class(SEXP obj, SEXP class);
  SEXP ALIKEC_abstract_ts(SEXP x, SEXP what);
  int ALIKEC_env_track(SEXP env, struct ALIKEC_settings_env * set);
  SEXP ALIKEC_env_track_test(SEXP env, SEXP stack_size_init);
  struct ALIKEC_settings_env * ALIKEC_env_set_create(int stack_size_init);

  // - Imported Funs -------------------------------------------------------------

  char * (*CSR_smprintf4)(
    size_t, const char *, const char *, const char *, const char *, const char *
  );
  char * (*CSR_len_as_chr)(R_xlen_t);
  size_t (*CSR_strmlen)(const char *, size_t);
  size_t (*CSR_len_chr_len)(R_xlen_t);
  char * (*CSR_strmcpy)(const char * str, size_t maxlen);

  // - Init and pre-install Symbols ----------------------------------------------

  SEXP ALIKEC_SYM_inherits;
  SEXP ALIKEC_SYM_package;
  SEXP ALIKEC_SYM_tilde;
  SEXP ALIKEC_SYM_paren_open;
  SEXP ALIKEC_SYM_args;
  SEXP ALIKEC_SYM_deparse;
  SEXP ALIKEC_SYM_nlines;
  SEXP ALIKEC_SYM_getOption;
  SEXP ALIKEC_SYM_matchcall;
  SEXP ALIKEC_CALL_matchcall;
  SEXP ALIKEC_CALL_matchcall_sub;

#endif
