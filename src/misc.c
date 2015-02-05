#include "alike.h"
#include "pfhash.h"

// - Helper Functions ----------------------------------------------------------

/* equivalent to `mode` in R, note this is a bit approximate and just trying to
hit the obvious corner cases between `typeof` and `mode`*/

SEXP ALIKEC_mode(SEXP obj) {
  const char * class;
  switch(TYPEOF(obj)) {
    case NILSXP: class = "NULL"; break;
    case SYMSXP: class = "name"; break;
    case FUNSXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case CLOSXP: class = "function"; break;
    case LANGSXP: class = "call"; break;
    case REALSXP: class = "numeric"; break;
    default: class = type2char(TYPEOF(obj));
  }
  return(mkString(class));
}
// - Testing Function ----------------------------------------------------------

SEXP ALIKEC_test(SEXP obj1, SEXP obj2, SEXP obj3) {

  // if(TYPEOF(obj1) != LISTSXP || TYPEOF(obj2) != LISTSXP)
  //   error("incorrect input type");
  // SEXP res;
  // R_xlen_t i, j;

  PrintValue(TAG(R_NilValue));
  // if(asInteger(obj3)) for(R_xlen_t i = 0; obj1 != R_NilValue && i < 25; i++, obj1 = CDR(obj1)) 1;
  // else xlength(obj1) < 25;
  // if(asInteger(obj3)) {
  //   pfHashTable * hash = pfHashCreate(NULL);
  //   for(i = 0; obj1 != R_NilValue; obj1 = CDR(obj1), i++) {
  //     const char * symb_chr = CHAR(PRINTNAME(TAG(obj1)));
  //     char * symb_abs = pfHashFind(hash, (char *) symb_chr);
  //     if(symb_abs == NULL) {
  //       symb_abs = (char *) CHAR(asChar(CAR(obj1)));
  //       pfHashSet(hash, (char *) symb_chr, symb_abs);
  //   } }
  //   res = PROTECT(allocVector(STRSXP, i));

  //   for(j = 0; obj2 != R_NilValue && j < i; obj2 = CDR(obj2), j++) {
  //     const char * symb_chr = CHAR(PRINTNAME(TAG(obj2)));
  //     char * map_val = pfHashFind(hash, (char *) symb_chr);
  //     SET_STRING_ELT(res, j, mkChar(map_val == NULL ? "" : map_val));
  //   }
  // } else {
  //   res = PROTECT(allocVector(STRSXP, xlength(obj1)));
  //   SEXP obj2_sub;
  //   for(i = 0; obj1 != R_NilValue; obj1 = CDR(obj1), i++) {
  //     const char * symb_chr = CHAR(PRINTNAME(TAG(obj1)));
  //     for(obj2_sub = obj2; obj2_sub != R_NilValue; obj2_sub = CDR(obj2_sub)) {
  //       const char * symb_chr_2 = CHAR(PRINTNAME(TAG(obj2_sub)));
  //       if(!strcmp(symb_chr, symb_chr_2)) {
  //         SET_STRING_ELT(res, i, asChar(CAR(obj1)));
  //         break;
  //       }
  //     }
  //   }
  // }
  // UNPROTECT(1);
  // return res;
  return R_NilValue;
}

