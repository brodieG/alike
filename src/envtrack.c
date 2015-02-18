#include "alike.h"

/*
We need environment stack tracking that will not persist across .Call calls
*/

/*
Allocated and re-allocate our env stack tracking object

Return 0 for failure, 1 for normal success, 2 for success requiring
re-allocation, 3 for success requiring re-allocation and copying
*/

int ALIKEC_env_stack_alloc(struct ALIKEC_settings_env * set) {
  int success = 1;
  // Rprintf(
  //   "Allocation\n\tsize: %d\n\tind: %d\n\tmult: %d\n\tinit: %d\n",
  //   set->stack_size, set->stack_ind, set->stack_mult, set->stack_size_init
  // );
  if(set->stack_size <= set->stack_ind) {
    int stack_size_old = set->stack_size;
    set->stack_size = set->stack_size_init * 1 << set->stack_mult;
    if(set->stack_size > ALIKEC_MAX_ENVS) return 0;

    SEXP * env_stack_tmp = (SEXP *) R_alloc(set->stack_size, sizeof(SEXP));

    success = 2;
    if(set->env_stack == 0) {
      set->env_stack = env_stack_tmp;
    }
    else if(set->stack_mult) { // Prev allocation happened, need to copy pointers
      for(int i = 0; i < stack_size_old; i++)
        env_stack_tmp[i] = set->env_stack[i];
      set->env_stack = env_stack_tmp; // ideally would free env_stack before repointing...
      success = 3;
    }
    set->stack_mult++;
  }
  return success;
}
/*
Initialize our stack tracking object
*/
struct ALIKEC_settings_env * ALIKEC_env_set_create(int stack_size_init) {
  if(stack_size_init < 0)
    error("`alike` env stack size init should be greater than zero");
  struct ALIKEC_settings_env * set =
    (struct ALIKEC_settings_env *)
      R_alloc(1, sizeof(struct ALIKEC_settings_env));
  set->stack_size = set->stack_ind = set->stack_mult = 0;
  set->env_stack = 0;
  set->stack_size_init = stack_size_init;
  int res = ALIKEC_env_stack_alloc(set);
  if(!res) error("Unable to allocate `alike` environment stack");
  return set;
}

/*
Track what environments we've checked already

Not super efficient allocation here; we should really free previous allocation
instead of just leaving it hanging until .Call ends.

Also, this should really be a linked list so we don't have to re-copy all the
pointers every time we expand the list.

Really taking solace in the point that this part of the code should be rarely
activated.

Returns
  * > 1 if the environment has not been seen before (and adds it to stack),
    really it is the result of the allocation attempt
  * 0 if the environment is new
  * -1 if we are out of space in the env stack
*/

int ALIKEC_env_track(SEXP env, struct ALIKEC_settings_env * set) {
  int alloc_res;
  if(!(alloc_res = ALIKEC_env_stack_alloc(set))) return -1;
  int env_found = 0;
  for(int i = 0; i < set->stack_ind; i++) {
    if(env == set->env_stack[i]) {
      env_found = 1;
      break;
  } }
  if(env_found) return 0;
  set->env_stack[set->stack_ind] = env;
  set->stack_ind++;
  return alloc_res;
}
/*
External interface purely for testing whether our environment hashing
is working
*/

SEXP ALIKEC_env_track_test(SEXP env_list, SEXP stack_size_init) {
  int stack_init_int = asInteger(stack_size_init);
  if(stack_init_int == NA_INTEGER || stack_init_int < 0)
    error("Logic Error: stack_size_init must be positive");
  if(TYPEOF(env_list) != VECSXP)
    error("Logic Error: expected a list for argument `env_list`");
  struct ALIKEC_settings_env * set = ALIKEC_env_set_create(stack_init_int);

  R_xlen_t len = XLENGTH(env_list);
  SEXP res = PROTECT(allocVector(INTSXP, len));
  int * res_int = INTEGER(res);
  R_xlen_t i;

  for(i = 0; i < len; i++) {
    SEXP env = VECTOR_ELT(env_list, i);
    if(TYPEOF(env) != ENVSXP)
      error("All contents of `env_list` should be environments; error at item %d\n", i + 1);
    res_int[i] = ALIKEC_env_track(env, set);
  }
  UNPROTECT(1);
  return res;
}
