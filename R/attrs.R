#' @rdname alike
#' @export

attributes_alike <- function(
  obj.reference, obj, special.attrs=TRUE, all.attrs=FALSE,
  comp.fun=all_equal, int.strict=0L, ...
) {
  UseMethod("attributes_alike")
}
#' @method attributes_alike default
#' @S3method attributes_alike default

attributes_alike.default <- function(
  obj.reference, obj, special.attrs=TRUE, all.attrs=FALSE,
  comp.fun=all_equal, int.strict=0L, ...
) {
  if(!is.logical(special.attrs) || length(special.attrs) != 1) stop("Argument `special.attrs` must be a 1 length logical")
  if(!is.logical(all.attrs) || length(all.attrs) != 1) stop("Argument `all.attrs` must be a 1 length logical")

  ref.attrs <- attributes(obj.reference)
  ref.attrs <- ref.attrs[names(ref.attrs) != "class"]
  obj.attrs <- attributes(obj.reference)
  obj.attrs <- obj.attrs[names(obj.attrs) != "class"]
  
  if(all.attrs == TRUE){
    if(!identical(sort(names(ref.attrs)), sort(names(obj.attrs)))) {
      return("does not have the same attributes")
  } }
  if(
    !is.null(ref.attrs <- attributes(obj.reference)) && 
    length(ref.attrs.noclass <- ref.attrs[names(ref.attrs) != "class"]) > 0L
  ) {
    ref.attrs.noclass.names <- names(ref.attrs.noclass)
    for(i in seq_along(ref.attrs.noclass)){
      obj.attr <- attr(obj, ref.attrs.noclass.names[[i]])
      if(is.null(obj.attr)) {
        return(paste0("is missing required attribute `", ref.attrs.noclass.names[[i]], "`"))
      } else if(length(ref.attrs.noclass[[i]]) == 0L) {
        if(!type_alike(ref.attrs.noclass[[i]], obj.attr, int.strict=int.strict)) {
          return(
            paste0(
              "has incorrect type for attribute `", ref.attrs.noclass.names[[i]], 
              "`; should be ", type_of(ref.attrs.noclass[[i]], int.strict=int.strict), 
              " instead of ", type_of(obj.attr, int.strict=int.strict)))
        }
      } else if(special.attrs &&
        isTRUE(ref.attrs.noclass.names[[i]] == "dim") &&   # Special treatment for dim attribute
        isTRUE(type_of(ref.attrs.noclass[[i]], int.strict=int.strict) == "integer")
      ) {
        dim.non.zero <- ref.attrs.noclass[[i]] != 0L
        if(!type_alike(ref.attrs.noclass[[i]], obj.attr, int.strict=int.strict) || 
          length(obj.attr) != length(ref.attrs.noclass[[i]]) ||
          any(obj.attr[!dim.non.zero] < 0L) ||
          !isTRUE(comp.fun(ref.attrs.noclass[[i]][dim.non.zero], obj.attr[dim.non.zero], ...))
        ) { 
          dim.string.vec <- ifelse(!dim.non.zero, "*", paste0(ref.attrs.noclass[[i]], "L"))
          return(
            paste0(
              "has incorrect `", ref.attrs.noclass.names[[i]], 
              "` attribute; should be c(", paste0(dim.string.vec, collapse=", "), 
              ") where `*` can be any positive integer"
        ) ) }
      } else if(special.attrs &&
        isTRUE(comp.fun(ref.attrs.noclass.names[[i]], "dimnames", ...)) &&   # Special treatment for dimnames attribute, make sure it has dimnames properties
        is.list(ref.attrs.noclass[[i]]) && 
        type_of(attr.dim <- attr(obj, "dim"), int.strict=int.strict) == "integer" &&
        length(ref.attrs.noclass[[i]]) == length(attr.dim) &&
        all(vapply(ref.attrs.noclass[[i]], function(x) is.character(x) || is.null(x), logical(1L))) &&
        identical(
          vapply(
            ref.attrs.noclass[[i]][non.nulls <- vapply(ref.attrs.noclass[[i]], Negate(is.null), logical(1L))], 
            length, integer(1L)
          ),
          attr.dim[non.nulls]
        )
      ) {        
        if(!is.list(obj.attr) || length(obj.attr) != length(ref.attrs.noclass[[i]]) || 
          !isTRUE(comp.fun(obj.attr[non.nulls], ref.attrs.noclass[[i]][non.nulls]), ...)
        ) {
          wrong.dim <- which(
            mapply(Negate(identical), obj.attr[non.nulls], ref.attrs.noclass[[i]][non.nulls])
          )[[1]]
          return(
            paste0(
              "has incorrect  `", ref.attrs.noclass.names[[i]], 
              "` for dimension ", wrong.dim, ": should be ", 
              deparse(ref.attrs.noclass[[i]][non.nulls][[wrong.dim]]), ")"
        ) ) }
      } else {
        if(!isTRUE(comp.fun(obj.attr, ref.attrs.noclass[[i]], ...))) {
          return(paste0("has incorrect value for attribute `", ref.attrs.noclass.names[[i]], "`"))
  } } } }
  TRUE
}
#' @rdname alike
#' @export

class_alike <- function(obj.reference, obj, class.mode="common.ancestry", comp.fun=all_equal, ...) {
  UseMethod("class_alike")
}
#' @method class_alike default
#' @S3method class_alike default

class_alike.default <- function(obj.reference, obj, class.mode="common.ancestry", comp.fun=all_equal, ...) {
  valid.class.modes <- c("equal", "common.ancestry")
  if(!is.character(class.mode) || length(class.mode) > 1L || !(class.mode %in% valid.class.modes)) {
    stop("Argument `class.mode` must be a 1 length character vector with values in ", deparse(valid.class.modes), ").")
  }
  if(is.matrix(obj.reference) && !is.matrix(obj)) return("should be a matrix")
  if(is.array(obj.reference) && !is.array(obj)) return("should be an array")
  if(!is.null(ref.class <- attr(obj.reference, "class"))) {
    obj.class <- attr(obj, "class")
    if(identical(class.mode, "equal")) {
      if(!isTRUE(comp.fun(ref.class, obj.class, ...))) {
        return("should have class equal to", deparse(ref.class))
      }
    } else if (identical(class.mode, "common.ancestry")) {
      class.err <- FALSE
      if((length.diff <- length(obj.class) - length(ref.class) + 1L) < 1L ||
        !isTRUE(comp.fun(attributes(obj.class), attributes(ref.class), ...)) ||
        !isTRUE(comp.fun(obj.class[length.diff:(length(ref.class) + length.diff - 1L)], c(ref.class), ...))
      ) {
        if(length(ref.class) > 1L) {
          msg <- paste(deparse(ref.class), "with", ref.class[length(ref.class)], "as the final ancestor")
        } else {
          msg <- if(is.null(attributes(ref.class))) ref.class else deparse(ref.class)
        }
        return(paste("should inherit from", msg))
      }
    } else {
      stop("Logic error, unexpected value for `class.mode`; contact package maintainer.")
  } }
  TRUE
}