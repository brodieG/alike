#' C version of \code{`alike`}
#' 
#' @section \code{`attr_mode`}:
#' 
#' Paramter \code{`attr_mode`} controls how attributes are compared:
#' \itemize{
#'   \item 0: special attributes are compared specially, and only attributes 
#'     present in target are compared.  Additionally, zero length attributes in
#'     \code{`target`} must be of the same type as the corresponding attribute in 
#'     \code{`current`}, but the \code{`current`} attribute may be any length
#'   \item 1: all attributes present in \code{`target`} must be present in 
#'     \code{`current`} and be identical.
#'   \item 2: all attributes present in \code{`target`} must be present in 
#'     \code{`current`} and there may be no additional attributes in \code{`current`}
#' }
#'
#' Attributes on attributes are checked indirectly only when \code{`target`}
#' has an attribute with length that isn't special, at which point the attributes
#' must be identical (and by extension, must have identical attributes 
#' themselves).  Attributes that are being treated "specially" or zero length
#' attributes don't have their attributes checked.
#' 
#' Special attributes at this point include `dim`, `dimnames`, and `class`.
#' 
#' @export
#' @useDynLib alike, .registration=TRUE, .fixes="ALIKEC_"

alike2 <- function(
  target, current, int_mode=0L, int_tol=.Machine$double.eps ^ 0.5, attr_mode=0L
) 
  .Call(ALIKEC_alike2, target, current, int_mode, int_tol, attr_mode)

#' @export

.alike2 <- function(target, current) .Call(ALIKEC_alike2_fast, target, current)

#' C version of \code{`type_of`}
#' 
#' The dot version \code{`.typeof2`} doesn't allow you to specify tolerance 
#' (uses the same default value) so that it can evaluate faster.
#' 
#' @seealso type_of
#' @aliases .typeof2
#' @param object the object to check the type of
#' @param tolerance similar to the \code{`\link{all.equal}`} \code{`tolerance`} argument
#' @export

typeof2 <- function(object, tolerance=.Machine$double.eps ^ 0.5)
  .Call(ALIKEC_typeof2, object, tolerance)


#' @export

.typeof2 <- function(object) .Call(ALIKEC_typeof2_fast, object)

#' C version of \code{`type_alike`}
#' 
#' The dot version \code{`.type_alike`} doesn't allow you to specify tolerance 
#' (uses the same default value) or mode so that it can evaluate faster.
#' 
#' @seealso type_alike
#' @aliases .type_alike2
#' @param tolerance similar to the \code{`\link{all.equal}`} \code{`tolerance`} argument
#' @export

type_alike2 <- function(target, current, mode=0L, tolerance=.Machine$double.eps ^ 0.5)
  .Call(ALIKEC_type_alike2, target, current, mode, tolerance)

#' @export

.type_alike2 <- function(target, current)
  .Call(ALIKEC_type_alike2_fast, target, current)

#' Compare Attributes
#' 
#' R interface for an internal C function used by \code{`alike`}.  Provided 
#' primarily for unit testing purposes
#' 
#' @seealso type_alike
#' @export
#' @param int_mode 

attr_compare <- function(target, current, attr_mode=0L)
  .Call(ALIKEC_compare_attributes, target, current, attr_mode)

#' @export

alike_test <- function(x) .Call(ALIKEC_test, x)
