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

const char * ALIKEC_func_alike_internal(SEXP target, SEXP current) {
  if(TYPEOF(target) != CLOSXP || TYPEOF(current) != CLOSXP)
    error("Arguments must be closures.");

  int dots = 0, dots_last = 0, dots_reset = 0;
  SEXP last_match = R_NilValue;
  for(
    SEXP tar_form = FORMALS(target), cur_form = FORMALS(current);
    tar_form != R_NilValue && cur_form != R_NilValue;
    tar_form = CDR(tar_form), cur_form = CDR(cur_form)
  ) {
    SEXP tar_tag = TAG(tar_form), cur_tag = TAG(cur_form);
    if(dots && dots_last) dots_reset = 1;
    if(!dots && tar_tag == R_DotsSymbol) dots = dots_last = 1;
    if(tar_tag == cur_tag) {
      if(CAR(tar_form) != R_UnboundValue && CAR(cur_form) == R_UnboundValue) {
        return (const char *) CSR_smprintf4(
          ALIKEC_MAX_CHAR, "have a default value for argument `%s`\n",
          CHAR(PRINTNAME(tar_tag)), "", "", ""
        );
      }
    } else {
      if(dots && dots_last) {  // True if dots or if last arg was dots
        int tag_match = 0;
        for(
          SEXP cur_next = cur_form; cur_next != R_NilValue;
          cur_next = CDR(cur_next)
        ) {
          SEXP cur_tag_next = TAG(cur_next);
          if(cur_tag_next == tar_tag) {
            tag_match = 1;
            last_match = tar_tag;
            break;
          }
        }
        if(!tag_match) {
          const char * arg_type = "as first argument";
          if(last_match != R_NilValue) {
            arg_type = (const char *) CSR_smprintf4(
              ALIKEC_MAX_CHAR, "after argument `%s`",
              CHAR(PRINTNAME(last_match)), "", "", ""
            );
          }
          return CSR_smprintf4(
            ALIKEC_MAX_CHAR, "have argument `%s` %s", CHAR(PRINTNAME(tar_tag)),
            arg_type, "", ""
          );
    } } }
    if(dots_reset) dots_last = 0;  // Need to know loop right after tar_form is dots
  }
  return "";
}
SEXP ALIKEC_func_alike_ext(SEXP target, SEXP current) {
  const char * res = ALIKEC_func_alike_internal(target, current);
  if(strlen(res)) return mkString(res);
  return(ScalarLogical(1));
}
