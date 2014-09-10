#' C version of \code{`alike`}
#' 
#' @export
#' @useDynLib alike, .registration=TRUE, .fixes="ALIKEC_"

alike2 <- function(
  target, current, int_mode=0L, int_tol=.Machine$double.eps ^ 0.5, 
  class_mode=0L, attr_mode=0L
) {
  .Call(
    ALIKEC_alike, target, current, int_mode, int_tol, class_mode, attr_mode, 
    PACKAGE="alike"
  )
}

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

typeof2 <- function(object, tolerance=.Machine$double.eps ^ 0.5) {
  .Call(ALIKEC_typeof2, object, tolerance)
}

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
