#' \code{`alike`} methods for recursive indeces
#' 
#' Recursive index lists are considered \code{`alike`} if
#' every terminal index in \code{`obj`} appears in \code{`obj.reference`},
#' where appearance includes appearing as a subset.  For example, 
#' \code{`c(1, 2, 1)`} would be considered as appearing in 
#' \code{`c(1, 2, 1, 3)`}.  Additionally, every terminal index
#' in \code{`obj.reference`} must be matched by a terminal index
#' \code{`obj`}.
#' 
#' @aliases alike.recursive_terminal_index_list alike.recursive_full_index_list
#' @seealso alike index_structure
#' @name alike.recursive_index_list
#' @param obj.reference R object to use as reference in comparison
#' @param obj object to compare
#' @param strict set to TRUE so that only indices that are equal pass; this is 
#'   mostly so you can get the custom error reporting when they don't.  It's a 
#'   bit of a mis-use of alikeness.
#' @param ... extra arguments for methods
#' @return TRUE if the objects are alike, a one length character vector describing why they are not otherwise

NULL

#' @export

alike.recursive_terminal_index_list <- function(obj.reference, obj, strict=FALSE, ...) {
  NextMethod()
}
#' @export

alike.recursive_full_index_list <- function(obj.reference, obj, strict=FALSE, ...) {
  if(!is.logical(strict) || length(strict) != 1L) stop("Argument `strict` must be a one length logical.")
  if(!(strict)) {
    if(!is.recursive_full_index_list(obj.reference) || !is.recursive_index_list(obj)) {
      stop(
        "Arguments `obj.reference` must be \"recursive_full_index_list\" ",
        "and `obj` must be \"recursive_index_list\"."
    ) }
    obj.reference <- terminal_indices(obj.reference)
  }
  NextMethod()    
}
#' @export

alike.recursive_index_list <- function(obj.reference, obj, ...) {
  tryCatch(
    alike_recursive_index_list_common(obj.reference, obj, strict=FALSE),
    error=function(e) stop(conditionMessage(e))
  )
}
#' @export

all.equal.recursive_index_list <- function(target, current, ...) {
  tryCatch(
    alike_recursive_index_list_common(target, current, strict=TRUE),
    error=function(e) stop(conditionMessage(e))
  )
}
# Common code to support alike and all.equal

alike_recursive_index_list_common <- function(obj.reference, obj, strict=FALSE) {
  if(!is.logical(strict) || length(strict) != 1L) 
    stop("Argument `strict` must be a one length logical.")
  if(strict) {
    if(
      !(is.recursive_full_index_list(obj.reference) && is.recursive_full_index_list(obj)) &&
      !(is.recursive_terminal_index_list(obj.reference) && is.recursive_terminal_index_list(obj.reference))
    ) {
      stop(
        "Arguments `obj.reference` and `obj` must both be \"recursive_full_index_list\"",
        " or \"recursive_terminal_index_list\"."
      )
    }
  } else {
    if(
      !is.recursive_terminal_index_list(obj.reference) || 
      !is.recursive_index_list(obj)
    ) {
      stop(
        "Arguments `obj.reference` must be \"recursive_terminal_index_list\"",
        " and `obj` must be \"recursive_index_list\"."
    ) }
    if(!is.recursive_terminal_index_list(obj)) obj <- terminal_indices(obj)
  }
  match.vec.ref <- logical(length(obj.reference))
  
  for(i in seq_along(obj.reference)) {
    match.vec <- logical(length(obj))
    ref.len <- length(obj.reference[[i]])    
    for(j in seq_along(obj)) {
      if(
        identical(
          c(obj.reference[[i]]), 
          obj[[j]][1L:(if(strict) length(obj[[j]]) else ref.len)])
        ) { # if strict==TRUE, objects must be identical
        match.vec.ref[[i]] <- TRUE
        match.vec[[j]] <- TRUE
      } 
    }
    obj <- obj[!match.vec]
  }
  if(length(obj) || length(obj.reference[!match.vec.ref])) {
    if(length(obj.reference[!match.vec.ref])) {
      msg <- paste0(
        "has incorrect recursive structure (index ", 
        paste0("[[", obj.reference[!match.vec.ref][[1L]], "]]", collapse=""), 
        " doesn't exist in Argument `obj`)."
      )
      return(
        structure(
          msg, 
          err.type="structure", index=obj.reference[!match.vec.ref][[1L]], 
          missing.in="obj"
      ) )  
    } else {
      msg <- paste0(
        "has incorrect recursive structure (index ", 
          paste0("[[", obj[[1L]], "]]", collapse=""), 
          " doesn't exist in Argument `obj.reference`)."
      )
      return(
        structure(
          msg, err.type="structure", index=obj[[1L]], 
          missing.in="obj.reference"
      ) )  
    }
  }
  TRUE
}