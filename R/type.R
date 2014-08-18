#' @export

type_alike <- function(obj.reference, obj, int.strict=0L, ...) {
  UseMethod("type_alike")
}
#' @export

type_alike.default <- function(obj.reference, obj, int.strict=0L, ...) {
  if(
    !is.numeric(int.strict) || length(int.strict) != 1L || ! any(int.strict == 0:2)
  ) {
    stop("Argument `int.strict` must be a one length integer %in% 0:2.")
  }
  if(is.numeric(obj) && is.numeric(obj.reference)) {  
    ref.type <- typeof(obj.reference)
    if(!int.strict) {
      cur.type <- type_of(obj)   # this is not the base R function
    } else {
      cur.type <- typeof(obj)
    }
    if(int.strict < 2L && cur.type == "integer") return(TRUE)
    else return(cur.type == ref.type)
  }
  return(typeof(obj) == typeof(obj.reference))
}
#' @export

type_of <- function(obj, int.strict=0L) {
  if(
    !is.numeric(int.strict) || length(int.strict) != 1L || ! any(int.strict == 0:2)
  ) {
    stop("Argument `int.strict` must be a one length integer %in% 0:2.")
  }
  if(is.numeric(obj) && !int.strict) {
    obj.non.na <- obj[!is.na(obj)]
    if(all(obj.non.na == as.integer(obj.non.na))) return("integer")
  }
  return(typeof(obj))
}