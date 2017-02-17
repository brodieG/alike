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
    error("Expected list argument, got %s", type2char(TYPEOF(msgs)));
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

  R_xlen_t len = XLENGTH(msgs);
  SEXP res;


  if(len > 1) {
    // 1. Sort the strings (really only need to do this if longer than 3, but oh
    // well

    SEXP msg_sort = PROTECT(ALIKEC_sort_msg(msgs));
    R_xlen_t groups = 1;

    // Determine how many groups of similar things there are in our list

    for(R_xlen_t i=1; i < len; i++) {
      SEXP v_elt = VECTOR_ELT(msg_sort, i);
      SEXP v_elt_prev = VECTOR_ELT(msg_sort, i - 1);
      if(
        strcomp(CHAR(STRING_ELT(v_elt, 0)), CHAR(STRING_ELT(v_elt_prev, 0))) ||
        strcomp(CHAR(STRING_ELT(v_elt, 2)), CHAR(STRING_ELT(v_elt_prev, 2))) ||
        strcomp(CHAR(STRING_ELT(v_elt, 3)), CHAR(STRING_ELT(v_elt_prev, 3)))
      ) {
        ++groups;
      }
    }
    // If we need to condense the list, then allocate it, otherwise just return
    // the original list

    if(groups < len) {
      res = PROTECT(allocVector(VECSXP, groups));
      R_xlen_t k = 0;      // count the index in our result vector
      R_xlen_t j = 0;      // count how many elements in group
      // this will be the concatented second value in our vectors

      const char * target = CHAR(asChar(VECTOR_ELT(msg_sort), 0));

      for(R_xlen_t i=1; i < len; i++) {

        SEXP v_elt = VECTOR_ELT(msg_sort, i);
        SEXP v_elt_prv = VECTOR_ELT(msg_sort, i - 1);

        if(
          strcomp(CHAR(STRING_ELT(v_elt, 0)), CHAR(STRING_ELT(v_elt_prv, 0))) ||
          strcomp(CHAR(STRING_ELT(v_elt, 2)), CHAR(STRING_ELT(v_elt_prv, 2))) ||
          strcomp(CHAR(STRING_ELT(v_elt, 3)), CHAR(STRING_ELT(v_elt_prv, 3)))
        ) {
          SET_VECTOR_ELT(res, k, duplicate(v_elt));
          if(j) {  // there are merged values
            SEXP v_elt_dup = VECTOR_ELT(res, k);
            target = CSR_smprintf4(
              ALIKEC_MAX_CHAR, "%s, or %s",
              target, CHAR(STRING_ELT(v_elt, 1)), "", ""
            )
            SET_STRING_ELT(v_elt_dup, 2, target);
          }
          j = 0;
          ++k;
        } else {
          //  we have two elements that are the same, need to merge them

          target = CSR_smprintf4(
            ALIKEC_MAX_CHAR, "%s, %s", target, CHAR(STRING_ELT(v_elt, 1)), "",
            ""
          );
        }
        ++j;
      }
    } else {
      res = PROTECT(msgs); // stack balance
    }
      //
    // While strings are the same as the prior one, concatenate the contents of
    // the second element in the vector string

    UNPROTECT(1);
  } else res = msgs;






  return msgs;

}
