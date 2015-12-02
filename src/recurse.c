/*
Functions used to manage tracking recursion into list like objects
*/


/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
After error has been found, populate the last value in our index tracking
structure, AND decrement the index level.

See ALIKEC_rec_ind_init for details on index structure
*/
ALIKEC_rec_track ALIKEC_rec_ind_set(
  struct ALIKEC_rec_track rec, struct ALIKEC_index ind
) {
  // Find correct spot in previously allocated indices spaces, clearly relies on
  // lvl being exactly correct...

  if(rec.lvl <= rec.lvl_start)
    error("Logic Error: unwound too many recursion levels; contact maintainer");

  size_t offset = rec.lvl - rec.lvl_start;
  struct ALIKEC_index * cur_ind = rec.indices + offset;
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
/*
Allocate the storage for the indices; should be done at first error, then
as we unwind recursion we record the values of the indices for each level
prior to the error

rec.lvl is always greater than rec.lvl_start if there is recursion; if there
is no recursion then the two values are the same.
*/
struct ALIKEC_rec_track ALIKEC_rec_ind_init(struct ALIKEC_rec_track rec) {
  if(rec.lvl < rec.lvl_start)
    error(
      "Logic Error: negative recursion level detected %d %d",
      rec.lvl, rec.lvl_start
    );
  size_t rec_off = rec.lvl - rec.lvl_start;
  if(rec_off) {
    rec.indices = (struct ALIKEC_index *)
      R_alloc(rec_off, sizeof(struct ALIKEC_index));
  }
  return rec;
}
