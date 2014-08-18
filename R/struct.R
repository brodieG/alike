#' @rdname alike
#' @export

struct_alike <- function(
  obj.reference, obj, special.attrs=TRUE, int.strict=TRUE, ...
) {
  UseMethod("struct_alike")
}
#' @method struct_alike function
#' @S3method struct_alike function

struct_alike.function <- function(
  obj.reference, obj, special.attrs=TRUE, int.strict=0L, ...
) {
  if(identical(typeof(obj.reference), "closure") && identical(typeof(obj), "closure")) {
    obj.reference <- formals(obj.reference)
    obj <- formals(obj)
    if(!isTRUE(form.check <- NextMethod("struct_alike")))
     return(paste("formals", form.check)) else return(TRUE)
  }
  NextMethod("struct_alike")
}
#' @method struct_alike call
#' @S3method struct_alike call

struct_alike.call <- function(
  obj.reference, obj, special.attrs=TRUE, int.strict=TRUE, ...
) {
  if(is.call(obj.reference) && is.call(obj)) {
    if(length(obj.reference) == 0L) {
      stop("Don't know how to handle zero length calls")
    } else if (length(obj.reference) == 1L) {
      return(TRUE)
  } }
  NextMethod("struct_alike")
}

#' @method struct_alike default
#' @S3method struct_alike default

struct_alike.default <- function(
  obj.reference, obj, special.attrs=TRUE, int.strict=TRUE, ...
) {
  if(!isTRUE(type.check <- type_alike(obj.reference, obj, int.strict))) {
    return(
      paste0(
        "should be ", type_of(obj.reference, int.strict), " instead of ", 
        type_of(obj, int.strict)
  ) ) }
  if((ref.len <- length(obj.reference)) != 0L) {
    if(ref.len != (obj.len <- length(obj))) {
      return(paste0("should have length ", ref.len, " instead of ", obj.len))
  } }
  return(TRUE)
}