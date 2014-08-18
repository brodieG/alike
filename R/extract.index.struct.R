# NEED TO FIX THE ERROR THROWING IN CHECKARGS TO TAKE ADVANTAGE OF EDITING CONDITIONS!!!

#' @rdname index_structure
#' @export

extract_with_index <- function(x, index, ...) {
  UseMethod("extract_with_index")
}
#' @method extract_with_index default
#' @S3method extract_with_index default

extract_with_index.default <- function(x, index, ...) {
  if(!is.numeric(index) || length(index) < 1L || any(index < 0)) stop("Argument `index` must be positive numeric and contain at least one value")
  if(!is.recursive(x)) stop("Argument `x` must be a recursive object")
  if(!is.list(x)) if(inherits(try(x <- as.list(x), silent=TRUE), "try-error")) stop("Argument `x` must be coercible to list with `as.list`")
  x.call <- quote(x)
  for(i in seq_along(index)) x.call <- call("[[", x.call, index[[i]])
  eval(x.call)
}
#' @rdname index_structure
#' @export

extract_with_indices <- function(x, index.list, ...) {
  UseMethod("extract_with_indices")
}
#' @method extract_with_indices default
#' @S3method extract_with_indices default

extract_with_indices.default <- function(x, index.list, ...) {
  if(!is.list(index.list)) stop("Argument `index.list` must be a list")
  if(!is.recursive(x)) stop("Argument `x` must be a recursive object")
  out.lst <- vector("list", length(index.list))
  par.call <- sys.call()
  tryCatch(
    for(i in seq_along(index.list)) {
      out.lst[[i]] <- extract_with_index(x, index.list[[i]])
    },
    error=function(e) {  # Throw special error iff the error is related to a problematic subsetting vector inside `index.list`
      if(identical(conditionMessage(e), "subscript out of bounds") && identical(conditionCall(e), quote(x[[index[[1L]]]]))) {
        e[["call"]] <- par.call
        e[["message"]] <- paste0(
          "subscript out of bounds when attempting to subset `", 
          deparse(par.call[[2]]),"` with recursive index ", 
          deparse(par.call[[3]]), "[[", i, "]]")
      }
      stop(e)
  } )
  out.lst
}
