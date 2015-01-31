#include "alike.h"

// - Implement a Hash Table ----------------------------------------------------




// - Anonymize Formula ---------------------------------------------------------

/*
Creates a copy of the call mapping objects to a deterministic set of names
based on the order in which they appear in the call

Here we use an environment to try to take advantage of the hash search to
identify whether a symbol already showed up or not. TBD how much faster this is
than just doing a full lookup on lists.

*/

char * ALIKEC_symb_abstract(SEXP symb, pfHashTable hash, size_t varnum) {
  char * symb_chr = CHAR(PRINTNAME(symb));
  char * symb_abs = pfHashFind(hash, symb_chr, TRUE);
  if(symb_abs == NULL) {
    symb_abs = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "a%s", CSR_len_as_chr(varnum), "", "", ""
    );
    pfHashSet(hash, symb_chr, symb_abs);
  }
  return symb_abs;
}

const char * ALIKEC_call_abs_rec(
  SEXP target, SEXP current, SEXP tar_hash_env, SEXP cur_hash_env,
  size_t * varnum, int abstract_consts
) {
  if(CAR(target) != CAR(current)) {  // Actual fun call must match exactly
    return "language mismatch";
  }
  SEXP tar_sub, cur_sub;
  for(
    tar_sub = CDR(target), cur_sub = CDR(current); tar_sub != R_NilValue;
    tar_sub = CDR(tar_sub), cur_sub = CDR(cur_sub)
  ) {
    SEXP tar_sub_car = CAR(tar_sub), cur_sub_car = CAR(cur_sub);
    SEXPTYPE tsc_type = TYPEOF(tar_sub_car), csc_type = TYPEOF(cur_sub_car);
    if((tsc_type == SYMSXP || csc_type == SYMSXP) && tsc_type != csc_type) {
      return "language mismach";
    }
    if(tsc_type == SYMSXP) {
      SEXP tar_abs = ALIKEC_symb_abstract(tar_sub_car, tar_hash_env, *varnum);
      SEXP cur_abs = ALIKEC_symb_abstract(cur_sub_car, cur_hash_env, *varnum);

      if(tar_abs != cur_abs) return "language mismatch";
      if(tar_abs == R_UnboundValue) (*varnum)++;
    } else if (tsc_type == LANGSXP && csc_type != LANGSXP) {
      return "language mismatch";
    } else if (tsc_type == LANGSXP) {
      return ALIKEC_call_abs_rec(
        tar_sub_car, cur_sub_car, tar_hash_env, cur_hash_env, varnum,
        abstract_consts
      );
    } else if (!abstract_consts && !R_compute_identical(target, current, 16)) {
      // Maybe this shouldn't be "identical", but too much of a pain in the butt

      return "language mismatch";
    }


  }
}

SEXP ALIKEC_call_abstract(SEXP target, SEXP current, int abstract_consts) {
  if(TYPEOF(target) != LANGSXP || TYPEOF(current) != LANGSXP)
    error("Arguments must be LANGSXP");
  pfHashTable * tar_hash = pfHashCreate(NULL);
  pfHashTable * cur_hash = pfHashCreate(NULL);
  size_t tmp = 0;
  size_t * varnum = &tmp;


  pfHashDestroy(tar_hash);
  pfHashDestroy(cur_hash);
  return R_NilValue;


}
/*
Determine whether objects should be compared as calls or as formulas; the main
difference in treatment is that calls are match-called if possible, and also
that for calls constants need not be the same
*/

SEXP ALIKEC_lang_alike_internal(SEXP target, SEXP current) {
  return R_NilValue;
}
