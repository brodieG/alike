#include "alike.h"

/*
Functions are considered alike if they have compatible function signatures:
- every argument present in target must be present in current
- if target has `...` as an argument, then current may have additional arguments
- arguments that have default values in target must have default values in
  current

Basically, the idea is that any valid call to `current` should also be a valid
call to `target`.

A few different matching situations:
- before dots in target, must match exactly
- at / after dots can have all sorts of extra arguments, but must eventually have
  dots
- after dots, can have all sorts of extra arguments, but eventually must match
  argument

fun(a, b, ..., c, d)
fun(a, b, e, f, ..., g, c, e)

*/

const char * ALIKEC_fun_alike_internal(SEXP target, SEXP current) {
  if(!isFunction(target) || !isFunction(current))
    error("Arguments must be functions.");

  SEXP tar_form, cur_form, args;
  SEXPTYPE tar_type = TYPEOF(target), cur_type = TYPEOF(current);

  // Translate specials and builtins to formals, if possible

  if(
    tar_type == SPECIALSXP || tar_type == BUILTINSXP ||
    cur_type == SPECIALSXP || cur_type == BUILTINSXP
  ) {
    args = PROTECT(list2(ALIKEC_SYM_args, R_NilValue));
    SET_TYPEOF(args, LANGSXP);
  } else PROTECT(R_NilValue);

  if(tar_type == SPECIALSXP || tar_type == BUILTINSXP) {
    SETCADR(args, target);
    target = PROTECT(eval(args, R_BaseEnv));
  } else PROTECT(R_NilValue);

  if(cur_type == SPECIALSXP || cur_type == BUILTINSXP) {
    SETCADR(args, current);
    current = PROTECT(eval(args, R_BaseEnv));
  } else PROTECT(R_NilValue);
  // Cycle through all formals

  int dots = 0, dots_last = 0, dots_reset = 0, tag_match = 1, dots_cur = 0;
  R_xlen_t tar_args = 0;
  SEXP last_match = R_NilValue, tar_tag, cur_tag;
  const char * res = "";
  for(
    tar_form = FORMALS(target), cur_form = FORMALS(current);
    tar_form != R_NilValue && cur_form != R_NilValue;
    tar_form = CDR(tar_form), cur_form = CDR(cur_form), tar_args++
  ) {
    tar_tag = TAG(tar_form);
    cur_tag = TAG(cur_form);
    if(dots && dots_last) dots_reset = 1;
    if(!dots && tar_tag == R_DotsSymbol) dots = dots_last = 1;
    if(!dots_cur && cur_tag == R_DotsSymbol) dots_cur = 1;
    if(tar_tag == cur_tag) {
      if(CAR(tar_form) != R_MissingArg && CAR(cur_form) == R_MissingArg) {
        res = (const char *) CSR_smprintf4(
          ALIKEC_MAX_CHAR, "have a default value for argument `%s`",
          CHAR(PRINTNAME(tar_tag)), "", "", ""
        );
        break;
      }
      last_match = tar_tag;
    } else {
      tag_match = 0;           // no match until proven otherwise
      if(dots && dots_last) {  // True if dots or if last arg was dots
        for(
          SEXP cur_next = cur_form; cur_next != R_NilValue;
          cur_next = CDR(cur_next)
        ) {
          SEXP cur_tag_next = TAG(cur_next);
          if(!dots_cur && cur_tag_next == R_DotsSymbol) dots_cur = 1;
          if(cur_tag_next == tar_tag) {
            last_match = tar_tag;
            tag_match = 1;
            cur_form = cur_next;
            break;
      } } }
      if(!tag_match) break;
    }
    if(dots_reset) dots_last = 0;  // Need to know loop right after tar_form is dots
  }
  // We have a mismatch; produce error message

  int cur_mismatch = cur_form != R_NilValue && last_match != R_DotsSymbol;
  if(res[0] == '\0' && (tar_form != R_NilValue || !tag_match || cur_mismatch)) {
    if(dots && !dots_cur) {
      res = "have a `...` argument";
    } else if (!tar_args && tar_form == R_NilValue) {
      res = "not have any arguments";
    } else {
      const char * arg_type = "as first argument";
      const char * arg_name;
      const char * arg_mod = "";
      if(last_match != R_NilValue) {
        arg_type = (const char *) CSR_smprintf4(
          ALIKEC_MAX_CHAR, "after argument `%s`",
          CHAR(PRINTNAME(last_match)), "", "", ""
      );}
      if(tar_form != R_NilValue || !tag_match){
        arg_name = CHAR(PRINTNAME(TAG(tar_form)));
      } else if(cur_mismatch) {
        arg_mod = "not ";
        arg_name = CHAR(PRINTNAME(TAG(cur_form)));
      } else 
        error(
            "Logic Error: unexpected closure arg outcome; contact maintainer"
            );
      res = (const char *) CSR_smprintf4(
        ALIKEC_MAX_CHAR, "%shave argument `%s` %s", arg_mod, arg_name, arg_type,
        ""
  );} }
  // Success

  UNPROTECT(3);
  return res;
}
SEXP ALIKEC_fun_alike_ext(SEXP target, SEXP current) {
  const char * res = ALIKEC_fun_alike_internal(target, current);
  if(strlen(res)) return mkString(res);
  return(ScalarLogical(1));
}
