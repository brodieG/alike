#' A C version of Index Structure
#' 
#' @import inline
#' @export
#' @param x a list
#' @param terminal.nodes.only a logical vector 

index_structure_c <- cfunction(c(x = "numeric"), "
  SEXP result = PROTECT(allocVector(REALSXP, 1));
  /*REAL(result)[0] = 3.1;*/
  /*Rprintf(\"Hello %d\", result);*/
  /*REAL(result)[0] = asReal(x[0]) + asReal(x[1]);*/
  UNPROTECT(1);

  return result;
")

#' @export

my_add1 <- cfunction(c(a = "integer", b = "integer"), "
  /*SEXP result = PROTECT(allocVector(REALSXP, 1));*/
  /*REAL(result)[0] = asReal(a) + asReal(b);*/
  /*UNPROTECT(1);*/
  int *x;
  x = (int *) R_alloc(1, sizeof(int));
  return R_NilValue;
")

my_add2 <- cfunction(c(a = "integer", b = "integer"), "
  /*SEXP result = PROTECT(allocVector(REALSXP, 1));*/
  /*REAL(result)[0] = asReal(a) + asReal(b);*/
  /*UNPROTECT(1);*/
  int *x;
  x = (int *) R_alloc(100, sizeof(int));
  return R_NilValue;
")

my_add3 <- cfunction(c(a = "integer", b = "integer"), "
  /*SEXP result = PROTECT(allocVector(REALSXP, 1));*/
  /*REAL(result)[0] = asReal(a) + asReal(b);*/
  /*UNPROTECT(1);*/
  int *x;
  x = (int *) R_alloc(10000, sizeof(int));
  return R_NilValue;
")

my_add4 <- cfunction(c(a = "integer", b = "integer"), "
  /*SEXP result = PROTECT(allocVector(REALSXP, 1));*/
  /*REAL(result)[0] = asReal(a) + asReal(b);*/
  /*UNPROTECT(1);*/
  return R_NilValue;
")

lst <- list(list(1,2), list(3, list(1, list(1, list(1, 2, 3)))))
lst.2 <- list(list(1,2), list(3, list(1, list(1))))
library(inline)

boom <- cfunction(c(target="list", current="list"), "
  int ind_val = 0;          /* current index value */
  int ind_lvl = 0;          /* how deep in the stack we are */
  int ind_stk_sz = 32;      /* current size of stack */
  int ind_stk[ind_stk_sz];  /* track the stack */
  SEXP sxp_stk_tar[ind_stk_sz]; /* track the parent objects */
  SEXP sxp_stk_cur[ind_stk_sz]; /* track the parent objects */
  int emr_brk = 0;
  int i, k=0;

  SEXP tar_nxt = target;
  SEXP cur_nxt = current;

  while(TRUE) {   
    
    Rprintf(\"At Level %d, index %d, length %d, type %d, length.2 %d, type2 %d\\n\", ind_lvl, ind_val, length(tar_nxt), TYPEOF(tar_nxt), length(cur_nxt), TYPEOF(cur_nxt));

    if(emr_brk++ > 50) 
      error(\"Infininte Loop?\");

    if(TYPEOF(tar_nxt) == 19) {
      if(TYPEOF(cur_nxt) != 19 || length(cur_nxt) != length(tar_nxt)) {
        /* SEXP chr_err;
        chr_err = PROTECT(allocVector(STRSXP, 1)); */
        int ind_len = 0;
        int ind_sz = 0; 
        int ind_sz_max = 0; 
        
        for(i = 1; i <= ind_lvl; i++) { /* first level is meaningless */
          if((ind_sz = (int) ceil(log10(ind_stk[i] + 1.1))) > ind_sz_max)
            ind_sz_max = ind_sz;  /* we will use to allocate a char vec of appropriate size */
          /* Rprintf(\"  ind_sz: %d, ind_len: %d, stk: %d, log: %3.1f, ceil: %3.1f \\n\", ind_sz, ind_len, ind_stk[i] + 1, log10(ind_stk[i] + 1), ceil(log10(ind_stk[i] + 1))); */
          ind_len = ind_len + ind_sz;
        }
        /* Rprintf(\"ind_sz_max: %d, ind_len: %d\\n\", ind_sz_max, ind_len); */
        char chr_err_all[ind_len + 4 * (ind_lvl + 1) + 1];
        char chr_err[ind_sz_max + 4 + 1];
        chr_err_all[0] = chr_err[0] = '\\0';
        for(i = 1; i <= ind_lvl; i++) {
          /* Rprintf(\"Doing: [[%d]]\", ind_stk[i] + 1); */
          sprintf(chr_err, \"[[%d]]\", ind_stk[i] + 1);
          strcat(chr_err_all, chr_err);
        }
        Rprintf(\"%s\\n\", chr_err_all);
        return R_NilValue;

        error(\"Structure Mismatch\");
      }
      if(length(tar_nxt) > ind_val) {
        if(ind_lvl >= ind_stk_sz) {
          error(\"Exceeded Stack Size\");  /* placeholder, should re-allocate stack */
        } else if (ind_lvl < 0) {
          error(\"Negative Stack Index\"); /* genuine error */
        }
        ind_stk[ind_lvl] = ind_val;
        sxp_stk_tar[ind_lvl] = tar_nxt;
        sxp_stk_cur[ind_lvl] = cur_nxt;
        tar_nxt = VECTOR_ELT(tar_nxt, ind_val);
        cur_nxt = VECTOR_ELT(cur_nxt, ind_val);
        ind_lvl++;
        ind_val = 0;        
      } else {
        if(--ind_lvl < 0)
          break;
        ind_val = ind_stk[ind_lvl] + 1;
        tar_nxt = sxp_stk_tar[ind_lvl];
        cur_nxt = sxp_stk_tar[ind_lvl];
      }
    } else {
      if(--ind_lvl < 0)
        break;
      ind_val = ind_stk[ind_lvl] + 1;
      tar_nxt = sxp_stk_tar[ind_lvl];
      cur_nxt = sxp_stk_tar[ind_lvl];
    }
  }
  /* Rprintf(\"%f\", type2char(TYPEOF(a))); */
  
  SEXP res;
  res = PROTECT(allocVector(INTSXP, ind_stk_sz));
  /* Rprintf(\"Stack: \");*/
  for(i = 0; i < ind_stk_sz; i++) {
    INTEGER(res)[i] = (int) ind_stk[i];
    /* Rprintf(\"%d:%d:%d \", ind_stk[i], i, INTEGER(res)[i]); */
  }
  /*Rprintf(\"\\n\");*/
  UNPROTECT(1);
  return res;
")

boom(lst, lst.2)