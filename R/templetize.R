#' Reduces Objects to Their Basic Structure
#'
#' This is implemented in R so will slow down validations a little bit if you
#' use it within the \code{validate*()} expression itself.
#'
#' @export
#' @param con the data frame to truncate (name is due to S3 generic being for connection)
#' @param ... for compatibility with S3 generic
#' @return 0 row data frame
#' @examples
#' iris.tpl <- truncate(iris)
#' alike(iris.tpl, iris[1:10, ])
#' alike(iris.tpl, transform(iris, Species=as.characterSpecies))

abstract <- function(x, ...) UseMethod("abstract")

abstract.data.frame <- function(x, ...) con[0, ]

#' @export

abstract.default <- function(x, ...) {
  if(length(attr(x, "class")) || !is.atomic(x) || !identical(class(x), mode(x))) return(x)
  length(x) <- 0L
  x
}

#' @export

abstract.list <- function(x, ...) {
  for(i in length(x)) {
    x[[i]] <- abstract(con[[i]])
  }
  con
}
