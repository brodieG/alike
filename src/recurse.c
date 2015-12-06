#include "alike.h"
/*
Functions used to manage tracking recursion into list like objects
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
  Rprintf("Initializing rec ind to %d\n", rec.lvl);
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

  if(!rec.indices) rec = ALIKEC_rec_ind_init(rec);

  // Find correct spot in previously allocated indices spaces, clearly relies on
  // lvl being exactly correct...

  Rprintf("Setting index lvl: %d with indices %d\n", rec.lvl, rec.indices);
  struct ALIKEC_index * cur_ind = rec.indices + rec.lvl;
  *cur_ind = ind;
  rec.lvl--;
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
    .init = 0,     // will be set to 1 on first recursion
    .lvl = 0,
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
  if(!rec.init) {
    if(rec.lvl)
      error(
        "Logic Error: lvl should be zero in rec tracker; contact maintainer."
      );
    rec.init = 1;
  } else {
    size_t lvl_old = rec.lvl;
    rec.lvl++;
    if(rec.lvl < lvl_old)
      error(
        "Logic Error: %s; contact maintainer.",
        "max recursion depth exceeded, this really shouldn't happen"
      );
  }
  return rec;
}

