# function removes parens (e.g. turns `(2 + 2)` into 2 + 2), which
# actually should have no impact on evaluation at all, even with 
# more complex parenthetical statements, since the nested call structure
# already captures any parentheses

rem_parens <- function(x) {  
  if(!is.call(x)) return(x)
  if(length(x) < 1L) {
    stop("Don't know how to handle zero length calls.")
  } else if (length(x) == 1L) {
    return(x)
  } 
  if(identical(x[[1L]], quote(`(`))) {
    if(length(x) != 2L) stop("Don't know how to handle `(` calls with more than one argument.")
    return(if(is.call(x[[2L]])) Recall(x[[2L]]) else x[[2L]])
  } 
  for(i in 2L:length(x)) {
    if(is.call(x[[i]])) x[[i]] <- Recall(x[[i]])
  }
  return(x)
}
# Generate a copy of the objects with every non call object replaced by NULL

reduce_to_funs <- function(x) {
  if(!is.call(x)) stop("Logic Error")
  if(length(x) == 0L) stop("Don't know how to handle zero length calls.")
  if(length(x) == 1L) return(x)

  for(i in length(x):2L) if(is.call(x[[i]])) x[[i]] <- Recall(x[[i]]) else x <- nullify(x, i)
  return(x)
}
# Generate a copy of the objects with every replaced symbol replaced by a
# standardized name that is derived from the location of the first appearance
# of the symbol.  Calls and constants are left unchanged.  This is to
# allow comparison of formulas with different variable names.

reduce_to_formula_template <- function(x) {
  if(!is.call(x) || !inherits(x, "formula")) stop("Logic Error")
  var.names <- all.vars(x, unique=TRUE)
  name.root <- paste0(var.names, collapse="")
  var.replace <- paste0(name.root, "_", seq_along(var.names))     # these names are guaranteed not to exist in var.names
  var.replace <- sub(paste0("^", name.root), "var", var.replace)  # need names to be the same even if name root isn't

  (
    function(x) {
      if(is.call(x)) {
        if(length(x) == 0L) stop("don't know how to handle zero length calls")
        if(length(x) == 1L) return(x)
        for(i in 2L:length(x)) {
          x[[i]] <- Recall(x[[i]])
        }
        return(x)
      }
      if(is.symbol(x)) {
        return(as.name(var.replace[match(as.character(x), var.names)]))
      }
      x
    }
  ) (x)
}
# Generate a copy of the objects with all calls match.call()ed

rearrange_funs <- function(x, frame) {
  if(!is.call(x)) stop("Logic Error")
  if(length(x) == 0L) stop("Don't know how to handle zero length calls.")
  if(length(x) == 1L) return(x)

  if(!inherits(call.arranged <- try(match.call(eval(x[[1L]], frame), x), silent=TRUE), "try-error")) x <- call.arranged
  
  for(i in length(x):2L) if(is.call(x[[i]])) x[[i]] <- Recall(x[[i]])
  return(x)
}
# Helper function for several alike.* methods

run_comps <- function(
  obj.reference, obj, int.strict, class.mode, 
  special.attrs=TRUE, all.attrs=FALSE, comp.fun, ...
) {
  checks <- Filter(
    is.character, 
    list(
      struct_alike(obj.reference, obj, int.strict=int.strict),
      class_alike(obj.reference, obj, class.mode=class.mode, comp.fun=comp.fun, ...),
      attributes_alike(
        obj.reference, obj, special.attrs=special.attrs, all.attrs=all.attrs, 
        comp.fun=comp.fun, int.strict=int.strict, ...
    ) ) )
  if(length(checks)) return(checks[[1]]) else return(TRUE)
}

#' Compare Two Objects
#' 
#' Similar to \code{`\link{all.equal}`}, except that:
#' \itemize{
#'   \item It is not an S3 generic, all comparisons are handled within this
#'     function
#'   \item numeric objects are compared and if no element is more than
#'     \code{`tolerance`} apart the objects are considered equal
#'   \item other objects are compared with \code{`\link{identical}`}
#'   \item returns \code{FALSE} on failure, not a string explaining the error
#'   \item does not check attributes if both \code{`target`} and \code{`current`}
#'     are numeric; implicitly does so if they are not as it uses 
#'     \code{`identical`} in that case.
#' }
#' The main purpose of this function is to allow reasonable comparisons of
#' numerics with a little less overhead than \code{`\link{all.equal}`}.
#' 
#' @export
#' @param target the reference object
#' @param current the object to compare to the reference object
#' @param tolerance numeric 1 length the allowable differences between the 
#'   squares of the  differences between the two numeric objects (note the 
#'   default is equivalent to the .Machine$double.eps ^ 0.5 \code{`all.equal`} 
#'   uses)
#' @param ... arguments to pass on to \code{`\link{identical}`}
#' @return logical 1 length

all_equal <- function(target, current, tolerance = .Machine$double.eps, ...) {
  if(length(target) != length(current)) return(FALSE)
  if(is.numeric(target) && is.numeric(current)) {
    return(all((target / current - 1) ^ 2 < tolerance, na.rm=TRUE))
  }
  identical(target, current, ...)
}
