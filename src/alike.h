#include "pfhash.h"

#ifndef _ALIKEC_H
#define _ALIKEC_H

  // - Data Structures ---------------------------------------------------------

  /*
  index structures used to track the location at which an error occurs
  */
  union ALIKEC_index_raw {
    R_xlen_t num;
    const char * chr;
  };
  struct ALIKEC_index {
    union ALIKEC_index_raw ind;
    int type;               // 0 is numeric, 1 is character
  };
  struct ALIKEC_res {
    int success;
    char * message;
    int df;
    // track indices of error, this will be allocated with as many items as
    // there are recursion levels.
    struct ALIKEC_index * indices;
    size_t rec_lvl;                 // max recursion depth
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
    int type_mode, attr_mode, fuzzy_int_max_len, suppress_warnings;
    const char * prepend;
    SEXP env;                               // what environment to look for functions to match call in
    struct ALIKEC_settings_env * env_set;   // Used to track whether we've encountered an environment before
    int no_rec;                             // block futher recursion into environments
    size_t in_attr;                         // whether we're recursing through attributes
    size_t rec_lvl;                         // level of recursion
    size_t rec_lvl_last;                    // level of recursion last time ALIKEC_alike_internal was called
  };

  // - Constants ---------------------------------------------------------------

  #define ALIKEC_MAX_CHAR 10000
  #define ALIKEC_MAX_ENVS 65536

  // - Main Funs ---------------------------------------------------------------

  SEXP ALIKEC_alike (
    SEXP target, SEXP current, SEXP type_mode, SEXP attr_mode, SEXP env,
    SEXP fuzzy_int_max_len, SEXP suppress_warnings
  );
  SEXP ALIKEC_alike_ext(SEXP target, SEXP current, SEXP env);
  SEXP ALIKEC_alike_fast1 (SEXP target, SEXP current, SEXP settings);
  SEXP ALIKEC_alike_fast2 (SEXP target, SEXP current);
  const char * ALIKEC_alike_internal(
    SEXP target, SEXP current, struct ALIKEC_settings * set
  );
  SEXP ALIKEC_typeof(SEXP object);
  SEXP ALIKEC_type_alike(SEXP target, SEXP current, SEXP mode, SEXP max_len);
  SEXP ALIKEC_type_alike_fast(SEXP target, SEXP current);

  // - Internal Funs -------------------------------------------------------------

  SEXPTYPE ALIKEC_typeof_internal(SEXP object);
  const char *  ALIKEC_type_alike_internal(
    SEXP target, SEXP current, int mode, R_xlen_t max_len
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
  SEXP ALIKEC_test(SEXP mode, SEXP a, SEXP b);
  SEXP ALIKEC_test2(
    SEXP target, SEXP current
  );
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
