#' Internal Funs
#'
#' R interface for an internal C functions used by \code{alike}.  Provided
#' primarily for unit testing purposes
#'
#' @aliases name_compare class_compare dimname_compare dim_compare ts_compare
#'   lang_alike fun_alike dep_alike match_call_alike env_track
#' @keywords internal
#' @param int_mode

attr_compare <- function(target, current, attr.mode=0L)
  .Call(ALIKEC_compare_attributes, target, current, attr.mode)

#' @export

name_compare <- function(target, current)
  .Call(ALIKEC_compare_names, target, current)

class_compare <- function(target, current, rev=0)  # `rev` is unused; here for legacy
  .Call(ALIKEC_compare_class, target, current)

dimname_compare <- function(target, current)
  .Call(ALIKEC_compare_dimnames, target, current)

dim_compare <- function(
  target, current, tar_obj=integer(), cur_obj=integer(), rev=0L
)
  .Call(ALIKEC_compare_dims, target, current, tar_obj, cur_obj, rev);

ts_compare <- function(target, current)
  .Call(ALIKEC_compare_ts, target, current)

#' @export

lang_alike <- function(target, current, match.call.env=parent.frame())
  .Call(ALIKEC_lang_alike, target, current, match.call.env)

#' @export

fun_alike <- function(target, current)
  .Call(ALIKEC_fun_alike, target, current)

#' @export

dep_alike <- function(obj, lines)
  .Call(ALIKEC_deparse, obj, lines)

#' @export

match_call_alike <- function(call, env)
  .Call(ALIKEC_match_call, call, quote(match.call(NULL, quote(NULL))), env)

#' @export

env_track <- function(env, size_init = 32)
  .Call(ALIKEC_env_track, env, size_init)

#' Used for testing C code
#'
#' @export
#' @keywords internal

alike_test <- function(target, current, settings=parent.frame())
  .Call(ALIKEC_test, target, current, settings)

#' @export
#' @keywords internal

alike_test2 <- function(target, current)
  .Call(ALIKEC_test, target, current, sys.frame(sys.nframe()))

#' @export
#' @keywords internal

alike_test3 <- function(target, current)
  .Call(ALIKEC_test2, target, current)


#' Pre-calculated Precision Level
#'
#' Used to limit overhead of calls the require use of
#' \code{.Machine$double.eps ^ 0.5}

MachDblEpsSqrt <- .Machine$double.eps ^ 0.5
