#include "alike.h"

/*
 * Structure to capture the data to sort as well as the index so that we can
 * recover the sort index after the fact
 */

struct ALIKEC_sort_dat {
  const char * string;
  R_xlen_t index;
};

/*
 * Compare two character vectors a and b to determine if a is less than b or
 * equal.  The trick is that we're actually comparing the first, third, and
 * fourth values of the character vectors
 *
 * We assume we wont get non terminated strings from SEXPs...
 */

int ALIKEC_merge_comp(const void *p, const void *q) {
  struct ALIKEC_sort_dat a = *(struct ALIKEC_sort_dat *) p;
  struct ALIKEC_sort_dat b = *(struct ALIKEC_sort_dat *) q;

  return(strcmp(a.string, b.string));
}
/*
 * Sort a list of 4 length character vectors by the 1st, 3rd, and 4th elements
 */

SEXP ALIKEC_sort_msg(SEXP msgs) {
  if(TYPEOF(msgs) != VECSXP) {
    Rprintf("%s\n", type2char(TYPEOF(msgs)));
    error("Expected list argument");
  }

  R_xlen_t vec_len = xlength(msgs), i;

  struct ALIKEC_sort_dat * sort_dat =
    (struct ALIKEC_sort_dat *) R_alloc(vec_len, sizeof(struct ALIKEC_sort_dat));

  for(i = 0; i < vec_len; i++) {
    SEXP str_elt = VECTOR_ELT(msgs, i);
    if(TYPEOF(str_elt) != STRSXP || XLENGTH(str_elt) != 4)
      error(
        "Internal Error: unexpected string format to merge; contact maintainer"
      );
    sort_dat[i] = (struct ALIKEC_sort_dat) {
      CSR_smprintf4(
        ALIKEC_MAX_CHAR, "%s %s %s",
        CHAR(STRING_ELT(str_elt, 0)), CHAR(STRING_ELT(str_elt, 2)),
        CHAR(STRING_ELT(str_elt, 3)), ""
      ),
      i
    };
  }
  qsort(sort_dat, vec_len, sizeof(struct ALIKEC_sort_dat), ALIKEC_merge_comp);

  SEXP msg_sort = PROTECT(allocVector(VECSXP, vec_len));

  for(i = 0; i < vec_len; i++) {
    SET_VECTOR_ELT(msg_sort, i, VECTOR_ELT(msgs, sort_dat[i].index));
  }
  UNPROTECT(1);
  return(msg_sort);
}

/*
 * Combine length four character vectors where the first, third, and fourth
 * elements are identical.
 *
 * msgs a list of character vectors of the same length (4)
 */

SEXP ALIKEC_merge_msg(SEXP msgs) {
  // 1. combine all strings into one for comparison
  // 2. and put them into a pointer array
  // 3. sort the pointers
  // 4. walk down the list combining until you get a difference
  return msgs;

}
