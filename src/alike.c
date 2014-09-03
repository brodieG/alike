#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP C_alike (SEXP target, SEXP current);

static const
R_CallMethodDef callMethods[] = {
  {"alike", (DL_FUNC) &C_alike, 2},
  NULL 
};

void R_init_alike(DllInfo *info)
{
 /* Register the .C and .Call routines.
    No .Fortran() or .External() routines,
    so pass those arrays as NULL.
  */
  R_registerRoutines(info,
  NULL, callMethods,
  NULL, NULL);
}

int C_int_charlen (int a) {
  return (int) ceil(log10(a + 1.1));
}

SEXP C_alike (SEXP target, SEXP current) {

  /* General algorithm here is to:
      - Check whether objects are equal
      1. If they are, and one is list, dive into first element
      - Keep diving until hit something that isn't a list
      2. Walk to next item in item before, and go back to 1.
      - If no more items in list, then go to .2
    This should guarantee that we visit every element
  */
 
  int ind_val = 0;          /* current index value */
  int ind_lvl = 0;          /* how deep in the stack we are */
  int ind_lvl_max = -1;     /* deepest we've gone in current stack */
  int ind_stk_sz = 32;      /* current size of stack */
  int ind_stk[ind_stk_sz];  /* track the stack */
  SEXP sxp_stk_tar[ind_stk_sz]; /* track the parent objects */
  SEXP sxp_stk_cur[ind_stk_sz]; /* track the parent objects */
  int emr_brk = 0;
  int i, k=0;
  SEXPTYPE cur_type, tar_type;
  SEXP sxp_err;

  /* Define error message; need to figure out how to move this out of here */

  const char * err_msgs[2];
  err_msgs[0] = "Type mismatch, expected %s but got %s";
  err_msgs[1] = "Length mismatch, expected %s but got %s";
  int err_type = -1;
  int err_len = 0;
  char * err_tar;
  char * err_cur;
  
  /* Initialize Object Tracking Stack */

  sxp_stk_tar[0] = target;
  sxp_stk_cur[0] = current;

  while(TRUE) {   
    
    if(ind_lvl_max < ind_lvl) { /* need to initialize current stack position */
      ind_stk[ind_lvl] = 0;
      ind_lvl_max = ind_lvl;
    }
    if(ind_lvl >= ind_stk_sz) {
      error("Exceeded Stack Size");  /* placeholder, should re-allocate stack */
    } 
    if(ind_lvl < 0)
      error("Logic Error, level drop not detected");
    if(emr_brk++ > 50) 
      error("Infininte Loop?");
    if (ind_lvl < 0) {
      error("Negative Stack Index"); /* genuine error */
    }
    /* 
    Generate error messages to return as character vector; error message
    structure will be to have some base line message and then two components
    showing the target expectation and the actual value.  Most of what is going
    on here is getting everything in the right format for the final error
    message.  Things to know:

    err_msgs is a constant array with all the error messages, indexed by 
    `err_type`
    */
    
    if((tar_type = TYPEOF(target)) != (cur_type = TYPEOF(current))) {      
      err_type = 0;
      err_tar = R_alloc(strlen(type2char(tar_type)) + 1, sizeof(char));
      err_cur = R_alloc(strlen(type2char(cur_type)) + 1, sizeof(char));
      strcpy(err_tar, type2char(tar_type));
      strcpy(err_cur, type2char(cur_type));
    } else if(tar_type == 19 && length(target) != length(current)) {
      err_type = 1;
      err_tar = R_alloc(C_int_charlen(length(target)) + 1, sizeof(char));
      err_cur = R_alloc(C_int_charlen(length(current)) + 1, sizeof(char));
      sprintf(err_tar, "%d", length(target));
      sprintf(err_cur, "%d", length(current));      
    }
    if(err_type > -1) {
      err_len = strlen(err_msgs[err_type]) + strlen(err_tar) + strlen(err_cur) - 4; /* -4 because we have 2 %s in the message */
      char err_base[err_len + 1];
      sprintf(err_base, err_msgs[err_type], err_tar, err_cur);

      int ind_len = 0;
      int ind_sz = 0; 
      int ind_sz_max = 0; 

      /*
      Compute the part of the error that gives the index where the discrepancy
      occurred.  Note that last level is meaningless as it has not been dived 
      into yet so we purposefully ignore it with `i < ind_lvl`
      */

      for(i = 0; i < ind_lvl; i++) { 
        if((ind_sz = C_int_charlen(ind_stk[i])) > ind_sz_max)
          ind_sz_max = ind_sz;  /* we will use to allocate a char vec of appropriate size */
        ind_len = ind_len + ind_sz;
      }
      char err_chr_indeces[ind_len + 4 * (ind_lvl + 1) + 1];
      char err_chr_index[ind_sz_max + 4 + 1];
      err_chr_indeces[0] = err_chr_index[0] = '\0';
      for(i = 0; i < ind_lvl; i++) {
        sprintf(err_chr_index, "[[%d]]", ind_stk[i] + 1);
        strcat(err_chr_indeces, err_chr_index);
      }
      /* Create final error and store in STRSXP */

      sxp_err = PROTECT(allocVector(STRSXP, 1));
      char err_final[err_len + strlen(err_chr_indeces) + 10]; 
      if(ind_lvl > 0) {
        sprintf(err_final, "%s at index %s", err_base, err_chr_indeces);
      } else {
        sprintf(err_final, "%s", err_base);
      }
      SET_STRING_ELT(sxp_err, 0, mkChar(err_final));
      UNPROTECT(1);
      return sxp_err;
    }
    /* If object list, then dive in */

    if(TYPEOF(target) == 19) {
      if(ind_stk[ind_lvl] + 1 > length(target)) { /* no sub-items to check */
        if(ind_lvl <= 0)
          break;
        target = sxp_stk_tar[ind_lvl - 1];
        current = sxp_stk_cur[ind_lvl - 1];
        ind_stk[ind_lvl - 1]++;     /* Since we completed list, parent pointer should be moved forward to review next */
        ind_stk[ind_lvl] = 0;       /* Need to reset pointer before we leave this level as next time we get here it will be with a fresh list */
        ind_lvl--;
        ind_lvl_max--;
      } else {
        sxp_stk_tar[ind_lvl] = target;
        sxp_stk_cur[ind_lvl] = current;
        /* Rprintf("At level %d, advancing to %d\n", ind_lvl, ind_stk[ind_lvl]); */
        target = VECTOR_ELT(target, ind_stk[ind_lvl]);
        current = VECTOR_ELT(current, ind_stk[ind_lvl]);
        ind_lvl++;
      }
    } else {
      if(ind_lvl <= 0)
        break;
      target = sxp_stk_tar[ind_lvl - 1];
      current = sxp_stk_cur[ind_lvl - 1];
      ind_stk[ind_lvl - 1]++;
      ind_lvl--;
    }
  }
  SEXP res;
  res = PROTECT(allocVector(LGLSXP, 1));
  LOGICAL(res)[0] = 1;
  UNPROTECT(1);
  return res;  
}


    /**************************************************************************

    Rprintf(\"At Level %d, index %d, length %d, type %d, length.2 %d, type2 %d\n\", ind_lvl, ind_stk[ind_lvl], length(target), TYPEOF(target), length(current), TYPEOF(current));
    
    for(i=0; i <= ind_lvl_max; i++) {
      Rprintf(\"%d \", ind_stk[i]);
    }
    if(TYPEOF(target) == 14)
      Rprintf(\"Value: %f\", REAL(target)[0]);
    Rprintf(\"\n\");

    /**************************************************************************/
