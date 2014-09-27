#' Compare Object Structure
#' 
#' Similar to \code{`\link{all.equal}`}, but compares object structure rather than
#' value.
#' 
#' Exactly what makes two objects \code{`alike`} is complex to explain in words,
#' but should be clear from the examples.  We recommend you review those.  If you
#' are interested in more details, see the vignette.
#' 
#' @section \code{`int.mode`}:
#' 
#' This parameter controls how numerics and integers are compared.  The general
#' idea is that under some cicumstances numerics can match integers and vice versa.
#' The values for \code{`int_mode`} range from \code{`0L`} to \code{`2L`}, with 
#' a lower value corresponding to more relaxed comparison level.
#' 
#' \itemize{
#'   \item 0: integer like reals (e.g. \code{`1.0`}) can match against integer 
#'     templates, and integers always match real templates
#'   \item 1: integers always match against numeric templates
#'   \item 2: integers only match integer templates, and numerics only match
#'     numeric templates
#' }
#' 
#' These comparisons are handled by \code{`\link{type_alike}`}.
#' 
#' @section \code{`int.tol`}:
#' 
#' This parameter controls what is considered "integer-like". "integer-likeness"
#' is roughly defined as occurring when \code{`all.equal(as.integer(x), x) == TRUE`}.
#' The \code{`int_tol`} value corresponds to the value of the \code{`tolerance`}
#' argument used by \code{`all.equal.numeric`}.  Note though this is only an 
#' approximate comparison as \code{`alike`} does not use \code{`all.equal`}.
#' The default value is equal to \code{`.Machine$double.eps ^ .5`}, though note that
#' this value is pre-computed when the package is loaded and stored in 
#' \code{`alike:::MachDblEpsSqrt`} in order to minimize function overhead.
#' 
#' @section \code{`attr.mode`}:
#' 
#' Paramter \code{`attr.mode`} controls how attributes are compared:
#' \itemize{
#'   \item 0: special attributes are compared specially, and only attributes 
#'     present in \code{`target`} are compared (i.e. \code{`current`} may have
#'     additional attributes).  Special attributes include:
#'     \itemize{
#'       \item \code{`dim`}, \code{`dimnames`}, \code{`row.names`}, and \code{`names`}
#'       \item \code{`class`}
#'       \item zero-length attributes
#'       \item reference attributes (e.g. external pointers environments, etc.) 
#'     }
#'   \item 1: all attributes present in \code{`target`} must be present in 
#'     \code{`current`} and be identical.
#'   \item 2: all attributes present in \code{`target`} must be present in 
#'     \code{`current`} and there may be no additional attributes in \code{`current`}
#' }
#' 
#' Please see vignette for details on for how special attributes are compared.
#' Note that attributes on attributes (e.g. \code{`names(dimnames(x))`}) are 
#' generally required to be identical in \code{`target`} and \code{`current`},
#' though \code{`names(dimnames(x))`} itself is a special case.
#' 
#' @export
#' @useDynLib alike, .registration=TRUE, .fixes="ALIKEC_"
#' @param target the template to compare the object to
#' @param current the object to determine alikeness to the template
#' @param int.mode integer(1L) %in% 0:2 determines strictness of integer-real comparison
#' @param int.tol numeric(1L) comparable to the \code{`tolerance`} parameter of \code{`all.equal`}, affects when a real number is considered "integer-like"
#' @param attr.mode integer(1L) %in% 0:2 determines strictness of attribute comparison
#' @return TRUE if target and current are alike, character(1L) describing why they are not if they are not
#' @examples
#' alike(1L, 1.0)         # TRUE, because 1.0 is integer-like
#' alike(1L, 1.1)         # FALSE, 1.1 is not integer-like
#' alike(1.1, 1L)         # TRUE, by default, integers are always considered real
#' alike(integer(), 1:4)  # Zero length `target` matches any length `current`
#' alike(1:4, integer())  # But not vice versa
#' 
#' df.tpl <- data.frame(id=integer(), grade=factor(levels=LETTERS[1:6]))
#' df.cur <- data.frame(id=c(1, 3, 5)), grade=factor(c("A", "F", "B"), levels=LETTERS[1:6]))
#' df.cur2 <- data.frame(id=c(1, 3, 5)), grade=c("A", "F", "B"))
#' 
#' alike(df.tpl, df.cur)    # zero row df as `target` matches any length df
#' alike(df.tpl, df.cur2)   # factor levels must match; makes sense, otherwise it really isn't the same type of data (note this is a recursive comparison)
#' alike(df.cur, df.tpl)    # alike is not "commutative", now `target` is not zero row
#' 
#' alike(list(integer(), df.tpl), list(1:4, df.cur))  # recursive comparison
#' 
#'  

alike <- function(
  target, current, int.mode=0L, int.tol=MachDblEpsSqrt, attr.mode=0L
) 
  .Call(ALIKEC_alike, target, current, int.mode, int.tol, attr.mode)

#' @export

.alike <- function(target, current) .Call(ALIKEC_alike_fast, target, current)

#' C version of \code{`type_of`}
#' 
#' The dot version \code{`.typeof2`} doesn't allow you to specify tolerance 
#' (uses the same default value) so that it can evaluate faster.
#' 
#' @seealso type_of
#' @aliases .typeof2
#' @param object the object to check the type of
#' @param tolerance similar to the \code{`\link{all.equal}`} \code{`tolerance`} argument
#' @export

type_of <- function(object, tolerance=MachDblEpsSqrt)
  .Call(ALIKEC_typeof, object, tolerance)


#' @export

.type_of <- function(object)
  .Call(ALIKEC_typeof_fast, object)

#' C version of \code{`type_alike`}
#' 
#' The dot version \code{`.type_alike`} doesn't allow you to specify tolerance 
#' (uses the same default value) or mode so that it can evaluate faster.
#' 
#' @seealso type_alike
#' @aliases .type_alike2
#' @param tolerance similar to the \code{`\link{all.equal}`} \code{`tolerance`} argument
#' @export

type_alike <- function(target, current, mode=0L, tolerance=MachDblEpsSqrt)
  .Call(ALIKEC_type_alike, target, current, mode, tolerance)

#' @export

.type_alike <- function(target, current)
  .Call(ALIKEC_type_alike_fast, target, current)

#' Compare Attributes
#' 
#' R interface for an internal C functions used by \code{`alike`}.  Provided 
#' primarily for unit testing purposes
#' 
#' @aliases name_compare
#' @keywords internal
#' @param int_mode 

attr_compare <- function(target, current, attr.mode=0L)
  .Call(ALIKEC_compare_attributes, target, current, attr.mode)

name_compare <- function(target, current) {
  .Call(ALIKEC_compare_names, target, current)  
}


#' Used for testing C code
#' 
#' @keywords internal

alike_test <- function(obj1) .Call(ALIKEC_test, obj1)

#' Pre-calculated Precision Level
#' 
#' Used to limit overhead of calls the require use of \code{`.Machine$double.eps ^ 0.5`}

MachDblEpsSqrt <- .Machine$double.eps ^ 0.5
