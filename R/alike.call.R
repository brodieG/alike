#' Alike Methods For Calls, Formulas, and Expressions
#'
#' These functions attempt to hew to the spirit of \code{`\link{alike}`}
#' by comparing structure rather than contents of \code{`obj.reference`}
#' and \code{`obj`}.  Exactly what is considered \dQuote{structure} rather than
#' \dQuote{contents} varies with each method.
#' 
#' As with the other \code{`\link{alike}`} functions, alikeness is complex
#' to describe, but is generally intuitive expressed in the examples.
#'  
#' @section \code{`alike.formula`}:
#' 
#' Formulas are considered alike if they contain all the same calls
#' and the same constants in the same slots.  Variable names need not be
#' the same, but they must be consistent.  
#' 
#' See examples.
#' 
#' @section \code{`alike.call`}:
#' 
#' \code{`obj`} calls are considered alike to \code{`obj.reference`} calls
#' if every call and nested call contained in \code{`obj.reference`} exists
#' in \code{`obj`}.  Additionally, every call and sub-call must have the
#' same structure (i.e. same number of arguments).
#' 
#' Constants and variables within calls or sub-calls need not match, 
#' and if \code{`obj.reference`} contains a constant or variable in a 
#' particular position, \code{`obj`} may contain anything in that position,
#' including a call.  However, if \code{`obj.reference`} contains a call in a 
#' particular position, then \code{`obj`} must also contain a call to the same
#' function in that position.
#' 
#' If a call involves a function defined in the calling environment, then
#' the call will be re-arranged with \code{`\link{match.call}`} before
#' comparison.
#' 
#' See examples.
#' 
#' @section \code{`alike.expression`}:
#' 
#' Expressions are considered alike if they contain the same number of subobjects,
#' and if each subobject is \code{`\link{alike}`}.
#' 
#' Note that attributes \dQuote{srcref}, \dQuote{srcfile}, and \dQuote{wholeSrcref}
#' are ignored in the comparison as otherwise the comparisons would fail on those
#' attributes rather than diving into the expressions and detailing the failure
#' point.
#' 
#' @aliases alike.call, alike.formula, alike.expression
#' @inheritParams alike
#' @param exclude.parens if TRUE will remove parens before comparing (note this does 
#'   not affect operator priority since parens are removed after parsing), which means
#'   calls that are equivalent and only different due to parense (e.g
#'   \code{(1 + 1)} vs \code{1 + 1L}) will not be considered different
#' @seealso alike
#' @method alike call
#' @S3method alike call
#' @return TRUE if alike, character string describing first discrepancy otherwise
#' @examples
#' alike(x ~ y, z ~ w)                # TRUE
#' alike(x ~ y, z ~ w + 1)            # Incorrect
#' alike(x ~ y + 2, z ~ w + 1)        # Incorrect; constants must be equal  
#' alike(x ~ y + z:y, w ~ v + u:v)    # TRUE, var names consistent
#'  
#' alike(quote(1 + 1), quote(x + y))                 # TRUE
#' alike(quote(fun(1 + 1)), quote(fun(x + y, 9)))    # Nope, structures not the same
#' alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(NULL, fun2(a, b), 1)))  # Nope, not using same named args
#' fun <- function(a, b, c) NULL      # define `fun`
#' alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(NULL, fun2(a, b), 1)))  # TRUE, Because fun defined, uses match.call() to re-arrange args so it works

alike.call <- function(
  obj.reference, obj, int.strict=0L, class.mode="common.ancestry", 
  special.attrs=TRUE, all.attrs=FALSE, exclude.parens=TRUE, ...
) {
  if(!is.call(obj.reference)) stop("Argument `obj.reference` must be a call")
  if(!is.call(obj)) return("is not a call")
  if(!is.logical(exclude.parens) || length(exclude.parens) != 1L) stop("Argument `exclude.parens` must be a 1 length logical.")
  if(inherits(comp_fun <- try(make_comp_fun(obj.reference, obj, int.strict, ...), silent=TRUE), "try-error")) stop(conditionMessage(attr(comp_fun, "condition")))
  
  if(exclude.parens) {
    if(inherits(try(obj.reference <- rem_parens(obj.reference)), "try-error")) stop("Failed attempting to remove parens from `obj.reference`.")
    if(inherits(try(obj <- rem_parens(obj)), "try-error")) stop("Failed attempting to remove parens from `obj`.")
  }
  obj.ref.ord <- rearrange_funs(obj.reference, parent.frame())
  obj.ord <- rearrange_funs(obj, parent.frame())
  obj.ref.clean <- reduce_to_funs(obj.ref.ord)
  obj.clean <- reduce_to_funs(obj.ord)

  all_comps <- function(obj.reference, obj) {
    if(is.null(obj.reference)) return(TRUE)
    if(!isTRUE(check <- run_comps(
      obj.reference, obj, compare.mode=compare.mode, class.mode=class.mode, 
      special.attrs=special.attrs, all.attrs=all.attrs, integers.match.double=integers.match.double, ...
    ) ) ) return(check)
    if(is.call(obj.reference) && is.call(obj)) {
      if(!isTRUE(comp_fun(obj.reference[[1]], obj[[1]]))) {
        return(paste0("should be a call to `", deparse(obj.reference[[1]]),"`"))
      } else if (!isTRUE(comp_fun(names(obj.reference), names(obj)))) {
        return(
          paste0(
            "should be a call to `", deparse(obj.reference[[1]]),"` specifying argument", 
            if(length(obj.reference) > 2L) "s", " `", deparse(names(obj.reference[-1L])), "`."
      ) ) }
    } else {
      if(!isTRUE(comp_fun(obj.reference, obj))) {
        return(paste0("should match `", deparse(obj.reference)),"`") 
    } } 
    TRUE
  }
  # Unfortunately, because possibility that match.call will re-order call
  # from what was submitted from user, error reporting gets super complex;
  # what follows is an attempt to provide a meaningful error by tracking down
  # the argument of the call that caused the error

  if(!isTRUE(msg <- all_comps(obj.ref.clean, obj.clean))) return(msg)
  struct_compare_fun <- function(obj.reference, obj) {
    alike(
      obj.reference, obj, compare.mode=compare.mode, class.mode=class.mode, 
      special.attrs=special.attrs, all.attrs=all.attrs, integers.match.double=integers.match.double,
      strict=FALSE, ...
  ) }
  if(!isTRUE(msg <- alike_recurse(obj.ref.clean, obj.clean, all_comps, struct_compare_fun=struct_compare_fun, compare.terminal.nodes.only=FALSE))) {
    # Parse attributes to begin constructing error message

    if(is.null(err.type <- attr(msg, "err.type"))) stop("Logic Error: could not retrieve error type, contact maintainer.")
    if(is.null(err.index <- attr(msg, "index"))) stop("Logic Error: could not retrieve index.")
    if(length(err.index) == 0L) stop("Logic Error: unexpected length zero index vector; contact maintainer.")
    if(identical(err.type, "structure")) {
      if(is.null(err.missing.in <- attr(msg, "missing.in"))) stop("Logic Error: could not retrieve 'missing.in'. contact package maintainer.")
      if(identical(err.missing.in, "obj")) {
        miss.msg <- "missing"
        obj.lookup <- obj.ref.ord
      } else if(identical(err.missing.in, "obj.reference")) {
        miss.msg <- "shouldn't have"
        obj.lookup <- obj.ord
      } else {
        stop("Logic Error: unexpected 'missing.in'; contact maintainer.")
      }
    } else {
      obj.lookup <- obj.ref.ord
    }
    base.obj <- if(length(err.index) == 1L) obj.lookup else extract_with_index(obj.lookup, head(err.index, -1L))
    arg.name <- if(is.null(names(base.obj)) || length(names(base.obj)) != length(base.obj) || nchar(names(base.obj)[[tail(err.index, 1L)]]) == 0L) {
      if(tail(err.index, 1L) == 1L) "call to" else paste("argument", tail(err.index, 1L) - 1L)
    } else {
      paste0("argument `", names(base.obj)[tail(err.index, 1L)], "`")
    }
    # Adjust message depending on whether a structure or value error
    
    if(identical(err.type, "structure")) {
      return(
        paste0(
          "has incorrect recursive structure; ", miss.msg, " ", arg.name, " `", 
          deparse(base.obj[[1L]]), "` in order to match `", deparse(obj.ref.ord), "`."
      ) )
    } else if (identical(err.type, "value")) {
      return(
        paste0(
          "has value mismatch: expected call `", deparse(base.obj[[tail(err.index, 1L)]]), 
          "` for ", arg.name, " in order to match `", deparse(obj.ref.ord), "`."
      ) )
    } else {
      stop("Logic Error: unknown error type; contact maintainer.")
    }
  }
  TRUE
}
#' @method alike formula
#' @S3method alike formula

alike.formula <- function(
  obj.reference, obj, compare.mode="all.equal", class.mode="common.ancestry", 
  special.attrs=TRUE, all.attrs=FALSE, integers.match.double=TRUE, exclude.parens=TRUE, ...
) {
  if(!is.call(obj.reference) || !inherits(obj.reference, "formula")) stop("Argument `obj.reference` must be a formula")
  if(!is.call(obj) || !inherits(obj, "formula")) stop("Argument `obj` must be a formula")
  if(!isTRUE(check <- run_comps(
    obj.reference, obj, compare.mode=compare.mode, class.mode=class.mode, 
    special.attrs=special.attrs, all.attrs=all.attrs, integers.match.double=integers.match.double, ...
  ) ) ) {
    return(msg)
  }
  if(!is.logical(exclude.parens) || length(exclude.parens) != 1L) stop("Argument `exclude.parens` must be a 1 length logical.")
  if(inherits(comp_fun <- try(make_comp_fun(obj.reference, obj, compare.mode, ...), silent=TRUE), "try-error")) stop(conditionMessage(attr(comp_fun, "condition")))
  
  if(exclude.parens) {
    if(inherits(try(obj.reference <- rem_parens(obj.reference)), "try-error")) stop("Failed attempting to remove parens from `obj.reference`.")
    if(inherits(try(obj <- rem_parens(obj)), "try-error")) stop("Failed attempting to remove parens from `obj`.")
  }
  obj.reference <- reduce_to_formula_template(obj.reference)
  obj <- reduce_to_formula_template(obj)

  if(!isTRUE(msg <- alike_recurse(obj.reference, obj, comp_fun, comp_fun, compare.terminal.nodes.only=TRUE))) c(msg) else TRUE
}
#' @method alike expression
#' @S3method alike expression

alike.expression <- function(
  obj.reference, obj, compare.mode="all.equal", class.mode="common.ancestry", 
  special.attrs=TRUE, all.attrs=FALSE, integers.match.double=TRUE, exclude.parens=TRUE, ...
) {
  if(!is.expression(obj.reference) || !is.expression(obj) || length(obj.reference) != length(obj)) stop("Arguments `obj.reference` and `obj` must be expressions of the same length")
  null.attrs <- c("srcref", "srcfile", "wholeSrcref")
  for(i in seq_along(null.attrs)) {
    attr(obj.reference, null.attrs[[i]]) <- NULL
    attr(obj, null.attrs[[i]]) <- NULL
  }
  if(!isTRUE(msg <- run_comps(
    obj.reference, obj, compare.mode=compare.mode, class.mode=class.mode, 
    special.attrs=special.attrs, all.attrs=all.attrs, integers.match.double=integers.match.double, ...
  ) ) ) {
    return(msg)
  }
  for(i in seq_along(obj.reference)) {
    if(!isTRUE(msg <- alike(
      obj.reference[[i]], obj[[i]], compare.mode=compare.mode, class.mode=class.mode, 
      special.attrs=special.attrs, all.attrs=all.attrs, integers.match.double=integers.match.double,
      exclude.parens=exclude.parens, ...)
    ) ) {
      return(paste0("mistmatch at sub-object [[", i, "]]: ", msg))
  } }
  TRUE
}
