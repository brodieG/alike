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

#' C version of \code{`typeof`}
#' 
#' @export

typeof2 <- function(object, int_mode=0L, int_tol=.Machine$double.eps ^ 0.5) {
  .Call(ALIKEC_typeof, object, int_mode, int_tol, PACKAGE="alike")
}