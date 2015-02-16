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
  x
}

#' Abstracts Time Series Parameters
#'
#' \code{\link{alike}} will treat time series parameter components with zero in
#' them as wildcards.  This function allows you to create these wild card time
#' series attribute since R does not allow direct creation/modification of
#' \code{ts} attributes with zero values.
#'
#' Make sure you do not try to use the templates you create with this for
#' anything other than as \code{\link{alike}} templates since the result is
#' likely undefined given R expects non zero values for the \code{ts}
#' attribute and attempts to prevent such attributes.
#'
#' @param what which portion of the \code{ts} attribute to abstract, by default
#'   all three are abstracted, but you can select, any one, two, or all
#' @return a \code{ts} object with the \code{ts} parameter modified
#' @export

abstract.ts <- function(x, what=c("start", "end", "frequency"), ...) {
  what.valid <- c("start", "end", "frequency")
  if(
    !is.character(what) || any(is.na(what)) || !all(what %in% what.valid)
  )
    stop(
      "Argument `what` must be character with all values in ",
      deparse(what.valid)
    )
  tsp <- attr(x, "tsp")
  if(!is.numeric(tsp) || length(tsp) != 3L)
    stop("Argument `x` must have a \"tsp\" attribute that is numeric(3L)")
  zero.out <- match(unique(what), what.valid) - 1L
  .Call(ALIKEC_abstract_ts, x, zero.out)
}
