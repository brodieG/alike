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
  sprintf(res, "%td", a);    // Correct type of R_xlen_t?
  return (const char *) res;
}
/* Returns a character pointer containing the results of using `a` as the parent
string and all the others a substrings with `sprintf`

note:
- will over-allocate by up to 8 characters to account for possibility there may
  not be an "%s" in `a`
*/

const char * ALIKEC_sprintf(char * a, const char * b, const char * c, const char * d, const char * e) {
  int full_len = strlen(a) + strlen(b) + strlen(c) + strlen(d) + strlen(e) + 1;
  char * res;
  res = R_alloc(full_len, sizeof(char));
  sprintf(res, a, b, c, d, e);
  return res;
}

/* Estimate how many characters an integer can be represented with */

int ALIKEC_int_charlen (R_xlen_t a) {
  if(a < 0)
    error("Logic Error: unexpected negative length value.");
  return (int) ceil(log10(a + 1.1));
}


// - Testing Function ----------------------------------------------------------

SEXP ALIKEC_test(SEXP obj1) {

  //return(ScalarInteger(obj1 == ALIKEC_SYM_package));
  return(ScalarInteger(!strcmp(CHAR(asChar(obj1)), "package")));
  // klass = getAttrib(obj1, R_ClassSymbol);

  // t = s = PROTECT(allocList(3));
  // SET_TYPEOF(s, LANGSXP);
  // SETCAR(t, install("inherits")); t = CDR(t);
  // SETCAR(t,  obj2); t = CDR(t);
  // SETCAR(t, klass);
  // UNPROTECT(1);
  // return eval(s, rho);
  // CHAR(asChar(klass))
  // // UNPROTECT(1);

  // Rprintf("class: %s\n", CHAR(asChar(klass)));
  // return ScalarLogical(inherits(obj2, CHAR(asChar(klass))));
}
