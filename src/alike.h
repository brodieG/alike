#include "pfhash.h"
#include <wchar.h>

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
  struct ALIKEC_res_fin {
    const char * message;
    const char * call;
  };
  struct ALIKEC_res_msg {
    int success;
    const char * message;
    const char * indices;
    const char * wrap;
  };
  // Keep track of environments in recursion to make sure we don't get into a
  // infinite recursion loop

  struct ALIKEC_env_track {
    int stack_size;
    int stack_ind;
    int stack_mult;
    int stack_size_init;
    int no_rec;       // prevent further recursion into environments
    SEXP * env_stack;
    int debug;
  };
  // track indices of error, this will be allocated with as many items as
  // there are recursion levels.

  struct ALIKEC_rec_track {
    size_t lvl;        // recursion depth
    size_t lvl_max;    // max recursion depth so far
    struct ALIKEC_index * indices;
    struct ALIKEC_env_track * envs;
    int gp;            // general purpose flag
  };
  struct ALIKEC_res {
    int success;
    struct ALIKEC_res_msg message;
    int df;
    struct ALIKEC_rec_track rec;
  };
  // Structure used for functions called by 'alike_obj', main difference with
  // the return value of 'alike_obj' is 'indices', since that is a more complex
  // object that requires initialization

  struct ALIKEC_res_sub {
    int success;
    struct ALIKEC_res_msg message;
    int df;      // whether df or not, not use by all functions
    int lvl;     // Type of error used for prioritizing
  };
  struct ALIKEC_settings {
    int type_mode, attr_mode, lang_mode, fuzzy_int_max_len, suppress_warnings;
    const char * prepend;     // no longer in use
    SEXP env;                 // what envto look for functions to match call in
    int in_attr;
    int width;                // Tell alike what screen width to assume
  };

  // - Constants --------------------------------------------------------------

  #define ALIKEC_MAX_CHAR 10000
  #define ALIKEC_MAX_ENVS 65536

  // - Main Funs --------------------------------------------------------------

  SEXP ALIKEC_alike (
    SEXP target, SEXP current, SEXP curr_sub, SEXP type_mode, SEXP attr_mode,
    SEXP env, SEXP fuzzy_int_max_len, SEXP suppress_warnings, SEXP lang_mode,
    SEXP width
  );
  SEXP ALIKEC_alike_ext(SEXP target, SEXP current, SEXP env, SEXP cur_sub);
  SEXP ALIKEC_alike_fast1(
    SEXP target, SEXP current, SEXP curr_sub, SEXP settings
  );
  SEXP ALIKEC_alike_fast2(SEXP target, SEXP current);
  struct ALIKEC_res ALIKEC_alike_internal(
    SEXP target, SEXP current, struct ALIKEC_settings set
  );
  SEXP ALIKEC_typeof(SEXP object);
  SEXP ALIKEC_type_alike(SEXP target, SEXP current, SEXP mode, SEXP max_len);
  SEXP ALIKEC_type_alike_fast(SEXP target, SEXP current);

  // - Internal Funs ----------------------------------------------------------

  SEXPTYPE ALIKEC_typeof_internal(SEXP object);
  const char * ALIKEC_type_alike_internal(
    SEXP target, SEXP current, int mode, R_xlen_t max_len
  );
  SEXP ALIKEC_compare_attributes(SEXP target, SEXP current, SEXP attr_mode);
  SEXP ALIKEC_compare_special_char_attrs(SEXP target, SEXP current);
  struct ALIKEC_res_sub ALIKEC_compare_attributes_internal(
    SEXP target, SEXP current, struct ALIKEC_settings set
  );
  SEXP ALIKEC_compare_class_ext(SEXP prim, SEXP sec);
  SEXP ALIKEC_compare_dimnames_ext(SEXP prim, SEXP sec);
  SEXP ALIKEC_compare_dim_ext(SEXP prim, SEXP sec, SEXP target, SEXP current);
  const char * ALIKEC_lang_alike_internal(
    SEXP target, SEXP current, struct ALIKEC_settings set
  );
  SEXP ALIKEC_lang_alike_ext(SEXP target, SEXP current, SEXP match_env);
  const char * ALIKEC_lang_alike_rec(
    SEXP target, SEXP cur_par, pfHashTable * tar_hash, pfHashTable * cur_hash,
    pfHashTable * rev_hash, size_t * tar_varnum, size_t * cur_varnum,
    int formula, SEXP match_call, SEXP match_env, struct ALIKEC_settings set
  );
  const char * ALIKEC_fun_alike_internal(SEXP target, SEXP current);
  SEXP ALIKEC_fun_alike_ext(SEXP target, SEXP current);
  SEXP ALIKEC_compare_ts_ext(SEXP target, SEXP current);

  // - Utility Funs -----------------------------------------------------------

  struct ALIKEC_rec_track ALIKEC_rec_def();
  struct ALIKEC_settings ALIKEC_set_def();
  struct ALIKEC_rec_track ALIKEC_rec_ind_chr(
    struct ALIKEC_rec_track res, const char * ind
  );
  struct ALIKEC_rec_track ALIKEC_rec_ind_num(
    struct ALIKEC_rec_track res, R_xlen_t ind
  );
  struct ALIKEC_res_sub ALIKEC_res_sub_def();
  SEXP ALIKEC_mode(SEXP obj);
  SEXP ALIKEC_test(SEXP obj);
  SEXP ALIKEC_test2(
    SEXP target, SEXP current
  );
  SEXP ALIKEC_getopt(const char * opt);
  SEXP ALIKEC_deparse_ext(SEXP obj, SEXP width_cutoff);
  SEXP ALIKEC_deparse_oneline_ext(
    SEXP obj, SEXP max_chars, SEXP keep_at_end
  );
  SEXP ALIKEC_deparse(SEXP obj, int width_cutoff);
  const char * ALIKEC_pad(SEXP obj, R_xlen_t lines, int pad);
  SEXP ALIKEC_pad_ext(SEXP obj, SEXP lines, SEXP pad);
  const char * ALIKEC_deparse_chr(SEXP obj, int width_cutoff);
  SEXP ALIKEC_match_call(SEXP call, SEXP match_call, SEXP env);
  SEXP ALIKEC_findFun(SEXP symbol, SEXP rho);
  SEXP ALIKEC_string_or_true(struct ALIKEC_res_fin);
  SEXP ALIKEC_class(SEXP obj, SEXP class);
  SEXP ALIKEC_abstract_ts(SEXP x, SEXP what);
  int ALIKEC_env_track(SEXP env, struct ALIKEC_env_track * envs);
  SEXP ALIKEC_env_track_test(SEXP env, SEXP stack_size_init);
  struct ALIKEC_env_track * ALIKEC_env_set_create(int stack_size_init);
  int ALIKEC_is_valid_name(const char *name);
  SEXP ALIKEC_is_valid_name_ext(SEXP name);
  int ALIKEC_is_dfish(SEXP obj);
  SEXP ALIKEC_is_dfish_ext(SEXP obj);
  struct ALIKEC_rec_track ALIKEC_rec_inc(struct ALIKEC_rec_track);
  struct ALIKEC_rec_track ALIKEC_rec_dec(struct ALIKEC_rec_track);

  // - Imported Funs ----------------------------------------------------------

  char * (*CSR_smprintf4)(
    size_t, const char *, const char *, const char *, const char *, const char *
  );
  char * (*CSR_smprintf6)(
    size_t, const char *, const char *, const char *, const char *, const char *,
    const char *, const char *
  );
  char * (*CSR_len_as_chr)(R_xlen_t);
  size_t (*CSR_strmlen)(const char *, size_t);
  size_t (*CSR_len_chr_len)(R_xlen_t);
  char * (*CSR_strmcpy)(const char * str, size_t maxlen);

  // - Init and pre-install Symbols -------------------------------------------

  SEXP ALIKEC_SYM_inherits;
  SEXP ALIKEC_SYM_package;
  SEXP ALIKEC_SYM_tilde;
  SEXP ALIKEC_SYM_paren_open;
  SEXP ALIKEC_SYM_args;
  SEXP ALIKEC_SYM_deparse;
  SEXP ALIKEC_SYM_nlines;
  SEXP ALIKEC_SYM_getOption;
  SEXP ALIKEC_SYM_matchcall;
  SEXP ALIKEC_SYM_widthcutoff;
  SEXP ALIKEC_CALL_matchcall;
  SEXP ALIKEC_CALL_matchcall_sub;
  SEXP ALIKEC_SYM_current;

#endif
