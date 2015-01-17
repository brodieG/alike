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

truncate.data.frame <- function(con, ...) con[0, ]

#' @export

truncate.default <- function(con, ...) {
  if(length(attr(con, "class")) || !is.atomic(con) || !identical(class(con), mode(con))) return(con)
  length(con) <- 0L
  con
}

#' @export

truncate.list <- function(con, ...) {
  for(i in length(con)) {
    con[[i]] <- truncate(con[[i]])
  }
  con
}
