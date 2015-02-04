#include "alike.h"
#include "pfhash.h"

// Moves pointer on language object to skip any `(` calls since those are
// already accounted for in parsing and as such don't add anything


SEXP ALIKEC_skip_paren(SEXP lang) {
  if(TYPEOF(lang) != LANGSXP) return(lang);
  while(
    CAR(lang) == ALIKEC_SYM_paren_open && CDR(CDR(lang)) == R_NilValue
  ) lang = CADR(lang);
  return lang;
}

// - Anonymize Formula ---------------------------------------------------------

/*
Creates a copy of the call mapping objects to a deterministic set of names
based on the order in which they appear in the call

Here we use an environment to try to take advantage of the hash search to
identify whether a symbol already showed up or not. TBD how much faster this is
than just doing a full lookup on lists.
*/

/* Look up symbol in hash table, if already present, return the anonymized
version of the symbol.  If not, add to the hash table

symb the symbol to lookup
hash the hash table
varnum used to generate the anonymized variable name
*/

char * ALIKEC_symb_abstract(SEXP symb, pfHashTable * hash, size_t * varnum) {
  const char * symb_chr = CHAR(PRINTNAME(symb));
  char * symb_abs = pfHashFind(hash, (char *) symb_chr);  // really shouldn't have to do this, but can't be bothered re-defining the hash library
  if(symb_abs == NULL) {
    symb_abs = CSR_smprintf4(
      ALIKEC_MAX_CHAR, "a%s", CSR_len_as_chr(*varnum), "", "", ""
    );
    pfHashSet(hash, (char *) symb_chr, symb_abs);
    (*varnum)++;
  }
  return symb_abs;
}

int ALIKEC_lang_alike_rec(
  SEXP target, SEXP current, pfHashTable * tar_hash, pfHashTable * cur_hash,
  size_t * tar_varnum, size_t * cur_varnum, int formula
) {
  if(CAR(target) != CAR(current)) return 0;  // Actual fun call must match exactly
  SEXP tar_sub, cur_sub;
  for(
    tar_sub = CDR(target), cur_sub = CDR(current); tar_sub != R_NilValue;
    tar_sub = CDR(tar_sub), cur_sub = CDR(cur_sub)
  ) {
    tar_sub = ALIKEC_skip_paren(tar_sub);
    cur_sub = ALIKEC_skip_paren(cur_sub);

    SEXP tar_sub_car = CAR(tar_sub), cur_sub_car = CAR(cur_sub);
    SEXPTYPE tsc_type = TYPEOF(tar_sub_car), csc_type = TYPEOF(cur_sub_car);
    if((tsc_type == SYMSXP || csc_type == SYMSXP) && tsc_type != csc_type)
      return 0;
    if(tsc_type == SYMSXP) {
      char * tar_abs = ALIKEC_symb_abstract(tar_sub_car, tar_hash, tar_varnum);
      char * cur_abs = ALIKEC_symb_abstract(cur_sub_car, cur_hash, cur_varnum);
      if(strcmp(tar_abs, cur_abs)) return 0;
    } else if (tsc_type == LANGSXP && csc_type != LANGSXP) {
      return 0;
    } else if (tsc_type == LANGSXP) {
      if(
        ! ALIKEC_lang_alike_rec(
          tar_sub_car, cur_sub_car, tar_hash, cur_hash, tar_varnum,
          cur_varnum, formula
        )
      ) {
        return 0;
      }
    } else if (formula && !R_compute_identical(tar_sub_car, cur_sub_car, 16)) {
      // Maybe this shouldn't be "identical", but too much of a pain in the butt
      // to do an all.equals type comparison

      return 0;
    }
  }
  return 1;
}
/*
Determine whether objects should be compared as calls or as formulas; the main
difference in treatment is that calls are match-called if possible, and also
that for calls constants need not be the same
*/

const char * ALIKEC_lang_alike_internal(SEXP target, SEXP current) {
  if(TYPEOF(target) != LANGSXP || TYPEOF(current) != LANGSXP)
    error("Arguments must be LANGSXP");
  pfHashTable * tar_hash = pfHashCreate(NULL);
  pfHashTable * cur_hash = pfHashCreate(NULL);
  size_t tartmp = 0, curtmp=0;
  size_t * tar_varnum = &tartmp;
  size_t * cur_varnum = &curtmp;
  int formula = 0;

  // Determine if it is a formular or not

  SEXP class = getAttrib(target, R_ClassSymbol);
  if(
    class != R_NilValue && TYPEOF(class) == STRSXP &&
    !strcmp("formula", CHAR(asChar(STRING_ELT(class, XLENGTH(class) - 1)))) &&
    CAR(target) == ALIKEC_SYM_tilde
  ) {
    formula = 1;
  }
  int res = ALIKEC_lang_alike_rec(
    target, current, tar_hash, cur_hash, tar_varnum, cur_varnum, formula
  );
  const char * err_msg = "";
  if(!res) {
    if(formula)
      err_msg = "have matching formulas (see docs for what constitutes matching formulas)";
    else
      err_msg = "have matching calls (see docs for what constitute matching calls)";
  }
  return err_msg;
}

SEXP ALIKEC_lang_alike_ext(SEXP target, SEXP current) {
  const char * res = ALIKEC_lang_alike_internal(target, current);
  if(strlen(res)) return mkString(res);
  return ScalarLogical(1);
}
