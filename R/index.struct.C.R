# #' A C version of Index Structure
# #' 
# #' @import inline
# #' @export
# #' @param x a list
# #' @param terminal.nodes.only a logical vector 

# index_structure_c <- cfunction(c(x = "numeric"), "
#   SEXP result = PROTECT(allocVector(REALSXP, 1));
#   /*REAL(result)[0] = 3.1;*/
#   /*Rprintf(\"Hello %d\", result);*/
#   /*REAL(result)[0] = asReal(x[0]) + asReal(x[1]);*/
#   UNPROTECT(1);

#   return result;
# ")

# #' @export

# my_add1 <- cfunction(c(a = "integer", b = "integer"), "
#   /*SEXP result = PROTECT(allocVector(REALSXP, 1));*/
#   /*REAL(result)[0] = asReal(a) + asReal(b);*/
#   /*UNPROTECT(1);*/
#   int *x;
#   x = (int *) R_alloc(1, sizeof(int));
#   return R_NilValue;
# ")

# my_add2 <- cfunction(c(a = "integer", b = "integer"), "
#   /*SEXP result = PROTECT(allocVector(REALSXP, 1));*/
#   /*REAL(result)[0] = asReal(a) + asReal(b);*/
#   /*UNPROTECT(1);*/
#   int *x;
#   x = (int *) R_alloc(100, sizeof(int));
#   return R_NilValue;
# ")

# my_add3 <- cfunction(c(a = "integer", b = "integer"), "
#   /*SEXP result = PROTECT(allocVector(REALSXP, 1));*/
#   /*REAL(result)[0] = asReal(a) + asReal(b);*/
#   /*UNPROTECT(1);*/
#   int *x;
#   x = (int *) R_alloc(10000, sizeof(int));
#   return R_NilValue;
# ")

# my_add4 <- cfunction(c(a = "integer", b = "integer"), "
#   /*SEXP result = PROTECT(allocVector(REALSXP, 1));*/
#   /*REAL(result)[0] = asReal(a) + asReal(b);*/
#   /*UNPROTECT(1);*/
#   return R_NilValue;
# ")

# # lst <-   list(list( 1,  2), list( 3, list( 4, list( 5, list(6, 6.1, 6.2)))))
# # > microbenchmark(boom(lst, lst))
# # 
# # Inline version
# # 
# # Unit: microseconds
# #            expr   min     lq median     uq    max neval
# #  boom(lst, lst) 1.495 1.5635 1.6145 1.7245 18.175   100
# # 
# # Dumb dyn.load
# # > microbenchmark(.Call("C_alike", lst, lst))
# # Unit: microseconds
# #                        expr    min     lq median     uq    max neval
# #  .Call("C_alike", lst, lst) 18.366 18.789 19.114 19.317 82.323   100

# # Registered dyn.load as per 2001 R News article
# # microbenchmark(.Call("C_alike", lst, lst))
# # Unit: microseconds
# #                        expr  min    lq median    uq    max neval
# #  .Call("C_alike", lst, lst) 1.82 1.829  1.866 1.929 31.034   100
# 
# # microbenchmark(.Call("alike", lst, lst))
# Unit: microseconds
#                      expr   min    lq median    uq    max neval
#  .Call("alike", lst, lst) 1.079 1.087 1.1425 1.199 15.247   100

# lst <-   list(list( 1,  2), list( 3, list( 4, list( 5, list(6, 6.1, 6.2)))))
# lst.2 <- list(list(11, 21), list(31, list(41, list(51, list(61          )))))
# library(inline)


# boom <- cfunction(c(target="list", current="list"), "
#   int ind_val = 0;          /* current index value */
#   int ind_lvl = 0;          /* how deep in the stack we are */
#   int ind_lvl_max = -1;     /* deepest we've gone in current stack */
#   int ind_stk_sz = 32;      /* current size of stack */
#   int ind_stk[ind_stk_sz];  /* track the stack */
#   SEXP sxp_stk_tar[ind_stk_sz]; /* track the parent objects */
#   SEXP sxp_stk_cur[ind_stk_sz]; /* track the parent objects */
#   int emr_brk = 0;
#   int i, k=0;
#   SEXPTYPE cur_type, tar_type;

#   sxp_stk_tar[0] = target;
#   sxp_stk_cur[0] = current;

#   while(TRUE) {   
    
#     if(ind_lvl_max < ind_lvl) { /* need to initialize current stack position */
#       ind_stk[ind_lvl] = 0;
#       ind_lvl_max = ind_lvl;
#     }
#     if(ind_lvl >= ind_stk_sz) {
#       error(\"Exceeded Stack Size\");  /* placeholder, should re-allocate stack */
#     } 
#     if(ind_lvl < 0)
#       error(\"Logic Error, level drop not detected\");
#     if(emr_brk++ > 50) 
#       error(\"Infininte Loop?\");
#     if (ind_lvl < 0) {
#       error(\"Negative Stack Index\"); /* genuine error */
#     }

#     /**************************************************************************

#     Rprintf(\"At Level %d, index %d, length %d, type %d, length.2 %d, type2 %d\\n\", ind_lvl, ind_stk[ind_lvl], length(target), TYPEOF(target), length(current), TYPEOF(current));
    
#     for(i=0; i <= ind_lvl_max; i++) {
#       Rprintf(\"%d \", ind_stk[i]);
#     }
#     if(TYPEOF(target) == 14)
#       Rprintf(\"Value: %f\", REAL(target)[0]);
#     Rprintf(\"\\n\");

#     /**************************************************************************/
    
#     if((tar_type = TYPEOF(target)) != (cur_type = TYPEOF(current))) {
#       error(\"Type mismatch, got %s but expected %s\", type2char(cur_type), type2char(tar_type));
#     } else if(tar_type == 19 && length(target) != length(current)) {
#       error(\"Length mismatch, got %d but expected %d\", length(current), length(target));
#     }

#     if(TYPEOF(target) == 19) {
#       if(ind_stk[ind_lvl] + 1 > length(target)) { /* no sub-items to check */
#         if(ind_lvl <= 0)
#           break;
#         target = sxp_stk_tar[ind_lvl - 1];
#         current = sxp_stk_cur[ind_lvl - 1];
#         ind_stk[ind_lvl - 1]++;     /* Since we completed list, parent pointer should be moved forward to review next */
#         ind_stk[ind_lvl] = 0;       /* Need to reset pointer before we leave this level as next time we get here it will be with a fresh list */
#         ind_lvl--;
#         ind_lvl_max--;
#       } else {
#         sxp_stk_tar[ind_lvl] = target;
#         sxp_stk_cur[ind_lvl] = current;
#         /* Rprintf(\"At level %d, advancing to %d\\n\", ind_lvl, ind_stk[ind_lvl]); */
#         target = VECTOR_ELT(target, ind_stk[ind_lvl]);
#         current = VECTOR_ELT(current, ind_stk[ind_lvl]);
#         ind_lvl++;
#       }
#     } else {
#       if(ind_lvl <= 0)
#         break;
#       target = sxp_stk_tar[ind_lvl - 1];
#       current = sxp_stk_cur[ind_lvl - 1];
#       ind_stk[ind_lvl - 1]++;
#       ind_lvl--;
#     }
#   }
#   /* Rprintf(\"%f\", type2char(TYPEOF(a))); */
  
#   SEXP res;
#   res = PROTECT(allocVector(INTSXP, ind_stk_sz));
#   /* Rprintf(\"Stack: \");*/
#   for(i = 0; i < ind_stk_sz; i++) {
#     INTEGER(res)[i] = (int) ind_stk[i];
#     /* Rprintf(\"%d:%d:%d \", ind_stk[i], i, INTEGER(res)[i]); */
#   }
#   /*Rprintf(\"\\n\");*/
#   UNPROTECT(1);
#   return res;
# ")


# boom(lst, lst.2)

#       # if(TYPEOF(cur_nxt) != 19 || length(cur_nxt) != length(tar_nxt)) {
#       #   /* SEXP chr_err;
#       #   chr_err = PROTECT(allocVector(STRSXP, 1)); */
#       #   if(TYPEOF(cur_nxt) != 19)
#       #     Rprintf(\"List mismatch failure\\n\");
#       #   if(length(cur_nxt) != length(tar_nxt))
#       #     Rprintf(\"Length mismatch failure\\n\");
#       #   int ind_len = 0;
#       #   int ind_sz = 0; 
#       #   int ind_sz_max = 0; 
        
#       #   for(i = 0; i <= ind_lvl; i++) { /* first level is meaningless; not sure */
#       #     if((ind_sz = (int) ceil(log10(ind_stk[i] + 1.1))) > ind_sz_max)
#       #       ind_sz_max = ind_sz;  /* we will use to allocate a char vec of appropriate size */
#       #     /* Rprintf(\"  ind_sz: %d, ind_len: %d, stk: %d, log: %3.1f, ceil: %3.1f \\n\", ind_sz, ind_len, ind_stk[i] + 1, log10(ind_stk[i] + 1), ceil(log10(ind_stk[i] + 1))); */
#       #     ind_len = ind_len + ind_sz;
#       #   }
#       #   /* Rprintf(\"ind_sz_max: %d, ind_len: %d\\n\", ind_sz_max, ind_len); */
#       #   char chr_err_all[ind_len + 4 * (ind_lvl + 1) + 1];
#       #   char chr_err[ind_sz_max + 4 + 1];
#       #   chr_err_all[0] = chr_err[0] = '\\0';
#       #   for(i = 0; i <= ind_lvl; i++) {
#       #     /* Rprintf(\"Doing: [[%d]]\", ind_stk[i] + 1); */
#       #     sprintf(chr_err, \"[[%d]]\", ind_stk[i] + 1);
#       #     strcat(chr_err_all, chr_err);
#       #   }
#       #   Rprintf(\"%s\\n\", chr_err_all);
#       #   return R_NilValue;

#       #   error(\"Structure Mismatch\");
#       # }