#include "alike.h"

// - Helper Functions ----------------------------------------------------------

/*
Returns a character pointer to the string representation of the integer; allocates
with R_alloc so in theory don't need to worry about freeing memory
*/

const char * ALIKEC_xlen_to_char(R_xlen_t a) {
  if(a < 0)
    error("Logic Error: unexpected negative length value.");
  int int_len = (int) ceil(log10(a + 1.00001));  // + 1.00001 to account for 0
  char * res;
  res = R_alloc(int_len + 1, sizeof(char));
  sprintf(res, "%d", (int) a);    // used to be %td, but doesn't work on windows?
  return (const char *) res;
}
/* Returns a character pointer containing the results of using `a` as the parent
string and all the others a substrings with `sprintf`

note:
- will over-allocate by up to 8 characters to account for possibility there may
  not be an "%s" in `a`
*/

const char * ALIKEC_sprintf(const char * a, const char * b, const char * c, const char * d, const char * e) {
  int full_len = strlen(a) + strlen(b) + strlen(c) + strlen(d) + strlen(e) + 1;
  char * res;
  res = R_alloc(full_len, sizeof(char));
  sprintf(res, a, b, c, d, e);
  return (const char *) res;
}

/* Estimate how many characters an integer can be represented with */

int ALIKEC_int_charlen (R_xlen_t a) {
  if(a < 0)
    error("Logic Error: unexpected negative length value.");
  return (int) ceil(log10(a + 1.1));
}
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

SEXP ALIKEC_test(SEXP obj1) {
  Rprintf("boom %s\n", type2char(TYPEOF(allocVector(VECSXP, 1))));
  return R_NilValue;
}

