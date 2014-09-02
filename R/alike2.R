#' C version of \code{`alike`}
#' 
#' @export
#' @useDynLib alike, .registration=TRUE, .fixes="C_"

alike2 <- function(target, current) {
  .Call(C_alike, target, current, PACKAGE="alike")
}