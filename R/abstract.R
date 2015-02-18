#' Turn S3 Objects Into Templates
#'
#' Create templates for use by \code{\link{alike}}. Currently somewhat
#' experimental; behavior may change in future.
#'
#' \code{abstract} is intended to create templates for use by
#' \code{\link{alike}}.  The result of abstraction is often a partially
#' specified object.  This type of object may not be suited for use in typical
#' R computations and may cause errors (or worse) if you try to use them as
#' normal R objects.
#'
#' \code{\link{alike}} is an S3 generic.  Note that the default method will
#' dispatch on implicit classes, so if you attempt to \code{abstract} an object
#' without an explicit \code{abstract} method, it will get abstracted based on
#' its implicit class.  If you define your own \code{abstract} method and do not
#' wish further abstraction based on implicit classes do not use
#' \code{\link{NextMethod}}.
#'
#' S4 and RC objects are returned unchanged.
#'
#' @export
#' @seealso \code{\link{abstract.ts}}
#' @param x the object to abstract
#' @param ... arguments for methods that require further arguments
#' @return abstracted object
#' @examples
#' iris.tpl <- abstract(iris)
#' alike(iris.tpl, iris[1:10, ])
#' alike(iris.tpl, transform(iris, Species=as.character(Species))
#'
#' abstract(1:10)
#' abstract(matrix(1:9, nrow=3))
#' abstract(list(1:9, runif(10)))

abstract <- function(x, ...) UseMethod("abstract")

#' @rdname abstract
#' @export

abstract.data.frame <- function(x, ...) x[0, ]

#' @rdname abstract
#' @export

abstract.default <- function(x, ...) {
  if(isS4(x)) return(x);
  if(!is.null(class.exp <- attr(x, "class"))) {
    attr(x, "class") <- NULL
    x <- abstract(x, ...)  # handle implicit classes
    class(x) <- class.exp
  }
  if(!is.atomic(x)) return(x)
  length(x) <- 0L
  x
}
#' @rdname abstract
#' @export

abstract.array <- function(x, ...) {
  if(!is.atomic(x)) NextMethod()
  ndims <- length(dim(x))
  length(x) <- 0L
  dim(x) <- rep(0L, ndims)
  if(!is.null(dimnames(x)))
    dimnames(x) <- replicate(NULL, length(dimnames(x)), simplify=FALSE)
  x
}
#' @rdname abstract
#' @export

abstract.matrix <-function(x, ...) abstract.array(x, ...)

#' @rdname abstract
#' @export

abstract.list <- function(x, ...) {
  for(i in seq_along(x)) {
    x[[i]] <- abstract(x[[i]])
  }
  x
}
#' @rdname abstract
#' @export

abstract.lm <- function(x, ...) {
  names(attr(x$terms, "dataClasses")) <- NULL
  names(x$model) <- NULL
  names(attr(attr(x$model, "terms"), "dataClasses")) <- NULL
  NextMethod()
}

#' Abstract Time Series
#'
#' \code{\link{alike}} will treat time series parameter components with zero in
#' them as wildcards.  This function allows you to create these wild card time
#' series attributes since R does not allow direct creation/modification of
#' \code{ts} attributes with zero values.
#'
#' Make sure you do not try to use the templates you create with this for
#' anything other than as \code{\link{alike}} templates since the result is
#' likely undefined given R expects non zero values for the \code{ts}
#' attribute and attempts to prevent such attributes.
#'
#' @seealso \code{\link{abstract}}
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
  x <- .Call(ALIKEC_abstract_ts, x, zero.out)
  NextMethod()
}
