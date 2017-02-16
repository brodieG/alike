/*
 * Combine length four character vectors where the first, third, and fourth
 * elements are identical.
 *
 * msgs a list of character vectors of the same length (4)
 */

SEXP ALIKEC_merge_msg <- function(SEXP msgs) {
  // 1. combine all strings into one for comparison
  // 2. and put them into a pointer array
  // 3. sort the pointers
  // 4. walk down the list combining until you get a difference

  if(TYPEOF(msgs) != VECSXP) error("Expected list argument");

  R_xlen_t vec_len = xlength(msgs), i;
  char * * strings = R_alloc(vec_len, sizeof(char));

  for(i = 0; i < vec_len; i++) {
    str_elt <- VECTOR_ELT(msgs, i)
    strings[i] = CSR_smprintf4(
        ALIKEC_MAX_CHAR, "%s %s %s", 
        STRING_ELT(str_elt, 0), STRING_ELT(str_elt, 0)
      );
  }

}
/*
 * Compare two character vectors a and b to determine if a is less than b or
 * equal.  The trick is that we're actually comparing the first, third, and
 * fourth values of the character vectors
 *
 * We assume we wont get non terminated strings from SEXPs...
 */

int ALIKEC_merge_comp <- function(SEXP a, SEXP b) {
  if(
    !TYPEOF(a) == STRSXP || !TYPEOF(b) == STRSXP || 
    XLENGTH(a) != 4 || XLENGTH(b) != 4
  )
    error("arguments a and b must be four long char vectors")

  int comp;

  if(comp = strcmp(CHAR(STRING_ELT(a, 0)), CHAR(STRING_ELT(b, 0)))) {
  } else if (comp = strcmp(CHAR(STRING_ELT(a, 2)), CHAR(STRING_ELT(b, 2)))) {
  } else if (comp = strcmp(CHAR(STRING_ELT(a, 3)), CHAR(STRING_ELT(b, 3))))

  return(comp);
}
