#include "alike.h"
/*
Functions used to manage tracking recursion into list like objects

To track recursion you must:
- initalize the rec track object first,
- increment each time you recurse,
- decrement each time you recurse,
- mark lvl_max when you hit an error
*/
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Allocate the storage for the indices; should be done at first error, then
as we unwind recursion we record the values of the indices for each level
prior to the error

By design the rec.lvl should be 0 if there is no recursion.
*/
struct ALIKEC_rec_track ALIKEC_rec_ind_init(struct ALIKEC_rec_track rec) {
  if(rec.lvl) {
    rec.indices = (struct ALIKEC_index *)
      R_alloc(rec.lvl, sizeof(struct ALIKEC_index));
  }
  return rec;
}
/*
After error has been found, populate the last value in our index tracking
structure, AND decrement the index level.

See ALIKEC_rec_ind_init for details on index structure
*/
struct ALIKEC_rec_track ALIKEC_rec_ind_set(
  struct ALIKEC_rec_track rec, struct ALIKEC_index ind
) {
  // Initialize indices if not initialized

  if(!rec.indices) {
    rec = ALIKEC_rec_ind_init(rec);
    rec.lvl_max = rec.lvl;
  }
  // Find correct spot in previously allocated indices spaces, clearly relies on
  // lvl being exactly correct...

  struct ALIKEC_index * cur_ind = rec.indices + rec.lvl - 1;
  *cur_ind = ind;
  return rec;
}
/*
Record character or numeric index values
*/
struct ALIKEC_rec_track ALIKEC_rec_ind_chr(
  struct ALIKEC_rec_track res, const char * ind
) {
  union ALIKEC_index_raw ind_u = {.chr = ind};
  return ALIKEC_rec_ind_set(res, (struct ALIKEC_index) {ind_u, 1});
}
struct ALIKEC_rec_track ALIKEC_rec_ind_num(
  struct ALIKEC_rec_track res, R_xlen_t ind
) {
  union ALIKEC_index_raw ind_u = {.num = ind};
  return ALIKEC_rec_ind_set(res, (struct ALIKEC_index) {ind_u, 0});
}
struct ALIKEC_rec_track ALIKEC_rec_def() {
  return (struct ALIKEC_rec_track) {
    .lvl = 0,
    .lvl_max = 0,
    .indices = 0,  // NULL pointer
    .envs = 0,     // NULL pointer
    .gp = 0
  };
}
/*
increment recursion

decrementing happens via ALIKEC_rec_ind_set
*/
struct ALIKEC_rec_track ALIKEC_rec_inc(struct ALIKEC_rec_track rec) {
  size_t lvl_old = rec.lvl;
  rec.lvl++;
  if(rec.lvl < lvl_old)
    error(
      "Logic Error: %s; contact maintainer.",
      "max recursion depth exceeded, this really shouldn't happen"
    );
  return rec;
}
struct ALIKEC_rec_track ALIKEC_rec_dec(struct ALIKEC_rec_track rec) {
  if(!rec.lvl)
    error(
      "Logic Error: %s; contact maintainer.",
      "tried to decrement rec counter below zero"
    );
  rec.lvl--;
  return rec;
}
/*
Translate recorded index data into a character string

This function is DEPRECATED in favor of the one that returns the index as
language
*/
const char * ALIKEC_rec_ind_as_chr(struct ALIKEC_rec_track rec) {
  char * err_chr_index, * err_chr_indices = "";
  const char * err_chr_index_val;
  size_t err_size = 0, ind_size_max = 0, ind_size;

  if(rec.lvl_max) {  // Recursion occurred
    // Scan through all indices to calculate size of required vector

    for(size_t i = 0; i < rec.lvl_max; i++) {
      switch(rec.indices[i].type) {
        case 0:
          ind_size = CSR_len_chr_len(rec.indices[i].ind.num);
          break;
        case 1:
          ind_size =
            CSR_strmlen(rec.indices[i].ind.chr, ALIKEC_MAX_CHAR) + 2;
          break;
        default: {
          error(
            "Logic Error: unexpected index type %d; contact maintainer.",
            rec.indices[i].type
          );
        }
      }
      if(ind_size > ind_size_max) ind_size_max = ind_size;
      err_size += ind_size;
    }
    // Allways alloacate as if index will be in [[index]] form as that is
    // worst case
    err_chr_indices = (char *) R_alloc(
      err_size + 4 * rec.lvl_max + 1, sizeof(char)
    );
    err_chr_index = (char *) R_alloc(ind_size_max + 4 + 1, sizeof(char));
    err_chr_indices[0] = '\0';

    for(size_t i = 0; i < rec.lvl_max; i++) {
      const char * index_tpl = "[[%s]]";
      switch(rec.indices[i].type) {
        case 0:
          {
            err_chr_index_val =
              (const char *) CSR_len_as_chr(rec.indices[i].ind.num);
          }
          break;
        case 1:
          {
            err_chr_index_val = rec.indices[i].ind.chr;
            if(!ALIKEC_is_valid_name(err_chr_index_val)){
              index_tpl = "$`%s`";
            } else {
              index_tpl = "$%s";
            }
          }
          break;
        default:
          error(
            "Logic Error: unexpected index type (2) %d", rec.indices[i].type
          );
      }
      // Used to leave off last index for possible different treatment if
      // dealing with a DF, but giving that up for now

      sprintf(err_chr_index, index_tpl, err_chr_index_val);
      if(i < rec.lvl_max) {  // all these chrs should be terminated...
        strcat(err_chr_indices, err_chr_index);
      }
    }
  }
  return err_chr_indices;
}
/*
Closely related to ALIKEC_rec_ind_as_chr except that it return a list (vector)
with the language call with all the indices subset, and the pointer to the
location in the language call that needs to be substituted.
*/
SEXP ALIKEC_rec_ind_as_lang(struct ALIKEC_rec_track rec) {
  SEXP res = PROTECT(allocVector(VECSXP, 2));
  setAttrib(res, ALIKEC_SYM_syntacticnames, ScalarLogical(1));
  SEXP lang = PROTECT(list1(R_NilValue));
  SEXP lang_cpy = lang;

  if(rec.lvl_max) {  // Recursion occurred
    // Make call to `[[` or `$`.  CAR is the `[[` or `$`, CADDR is the index
    // value, and CADR is the spot that will be filled in with what is being
    // subsetted: CADR$CADDR or CADR[[CADDR]]

    for(size_t i = rec.lvl_max; i > 0; i--) {
      size_t j = i - 1;
      SEXP index_call = PROTECT(lang3(R_NilValue, R_NilValue, R_NilValue));
      switch(rec.indices[j].type) {
        case 0:
          SETCAR(index_call, R_Bracket2Symbol);
          SETCADDR(index_call, ScalarReal(rec.indices[j].ind.num));
          break;
        case 1:
          SETCAR(index_call, R_DollarSymbol);
          SETCADDR(index_call, install(rec.indices[j].ind.chr));
          if(!ALIKEC_is_valid_name(rec.indices[j].ind.chr))
            setAttrib(res, ALIKEC_SYM_syntacticnames, ScalarLogical(0));
          break;
        default: {
          error(
            "Logic Error: unexpected index type %d; contact maintainer.",
            rec.indices[j].type
      );} }
      SETCAR(lang, index_call);
      UNPROTECT(1);
      lang = CDR(index_call);
    }
    SET_VECTOR_ELT(res, 0, CAR(lang_cpy));
    SET_VECTOR_ELT(res, 1, lang);
  }
  UNPROTECT(2);
  return res;
}
