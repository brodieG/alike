#' Compare Object Structure
#'
#' Similar to \code{\link{all.equal}}, but compares object structure rather than
#' value.  The \code{target} argument defines a template that the \code{current}
#' argument must match.
#'
#' Exactly what makes two objects \code{alike} is complex, but should be
#' intuitive.  The best way to understand "alikenes" is to review the examples.
#' If you are interested in more details, see the vignette.
#'
#' @section Value Comparisons:
#'
#' Values are never compared explicitly by \code{alike}.
#'
#' @section Length Comparisons:
#'
#' The lengths of two objects must be equal in order for them to be considered
#' alike, though in the special case where \code{target} is length zero, then
#' \code{current} may be any length..
#'
#' @section Types:
#'
#' The underlying data types between two objects must be \code{\link{type_alike}}.
#'
#' @section Attribute Comparison:
#'
#' Generally speaking attributes between \code{target} and \code{current}
#' must be identical, but there are some exceptions:
#'
#' Paramter \code{attr.mode} controls how attributes are compared:
#' \itemize{
#'   \item 0: special attributes are compared specially, and only attributes
#'     present in \code{target} are compared (i.e. \code{current} may have
#'     additional attributes).  Special attributes include:
#'     \itemize{
#'       \item \code{dim}, \code{dimnames}, \code{row.names}, and \code{names}
#'       \item \code{class}
#'       \item zero-length attributes
#'       \item reference attributes (e.g. external pointers environments, etc.)
#'     }
#'   \item 1: all attributes present in \code{target} must be present in
#'     \code{current} and be identical.
#'   \item 2: attributes in \code{target} and \code{current} must be identical
#' }
#' Please see vignette for details on for how special attributes are compared.
#' Note that attributes on attributes (e.g. \code{names(dimnames(x))}) are
#' generally required to be identical in \code{target} and \code{current},
#' though \code{names(dimnames(x))} itself is a special case.
#'
#' @section \code{.alike}:
#'
#' \code{.alike} is identical to \code{alike}, except that it doesn't accept
#' any parameters outside of \code{target} and \code{current}, and as a result
#' is slightly faster.
#'
#' @export
#' @import cstringr
#' @useDynLib alike, .registration=TRUE, .fixes="ALIKEC_"
#' @seealso type_alike, type_of
#' @param target the template to compare the object to
#' @param current the object to determine alikeness to the template
#' @param type.mode integer(1L) in 0:2, see \code{mode} parameter to \code{\link{type_alike}}
#' @param int.tol numeric(1L) see \code{tolerance} paramter to \code{\link{type_alike}}
#' @param attr.mode integer(1L) in 0:2 determines strictness of attribute comparison, see details
#' @return TRUE if target and current are alike, character(1L) describing why they are not if they are not
#' @examples
#' alike(1L, 1.0)         # TRUE, because 1.0 is integer-like
#' alike(1L, 1.1)         # FALSE, 1.1 is not integer-like
#' alike(1.1, 1L)         # TRUE, by default, integers are always considered real
#' alike(integer(), 1:4)  # TRUE, Zero length `target` matches any length `current`
#' alike(1:4, integer())  # But not vice versa
#'
#' # Scalarness can now be checked at same time as type
#'
#' x <- 1
#' x.2 <- 1:3
#' y <- TRUE
#' y.2 <- c(TRUE, TRUE)
#'
#' alike(integer(1L), x)
#' alike(logical(1L), y)
#' alike(integer(1L), x.2)
#' alike(logical(1L), y.2)
#'
#' # Zero length match any length of same type
#'
#' alike(integer(), 1:10)
#'
#' # NULL matches anything
#'
#' alike(NULL, mtcars)
#' alike(list(NULL, NULL), list(iris, mtcars))
#'
#' # `alike` will compare data frame columns
#'
#' df.tpl <- data.frame(id=integer(), grade=factor(levels=LETTERS[1:6]))
#' df.cur <- data.frame(id=c(1, 3, 5), grade=factor(c("A", "F", "B"), levels=LETTERS[1:6]))
#' df.cur2 <- data.frame(id=c(1, 3, 5), grade=c("A", "F", "B"))
#'
#' alike(df.tpl, df.cur)    # zero row df as `target` matches any length df
#' alike(df.cur, df.tpl)    # alike is not "commutative", now `target` is not zero row
#'
#' # factor levels must match; makes sense, otherwise it really isn't the same
#' # type of data (note this is a recursive comparison); for better understanding
#' # of error examine `levels(df.tpl[[2]])` and `levels(df.cur2[[2]])`
#'
#' alike(df.tpl, df.cur2)
#'
#' alike(list(integer(), df.tpl), list(1:4, df.cur))  # recursive comparison
#' alike(matrix(integer(), 3), matrix(1:21, ncol=7))  # partially specified dimensions
#'
#' # In order for objects to be alike, they must share a family tree, not just
#' # a common class
#'
#' obj.tpl <- structure(TRUE, class=letters[1:3])
#' obj.cur.1 <-  structure(TRUE, class=c("x", letters[1:3]))
#' obj.cur.2 <-  structure(TRUE, class=c(letters[1:3], "x"))
#'
#' alike(obj.tpl, obj.cur.1)
#' alike(obj.tpl, obj.cur.2)

alike <- function(
  target, current, type.mode=0L, int.tol=MachDblEpsSqrt, attr.mode=0L,
  suppress.warnings=FALSE, match.call.env=parent.frame()
)
  .Call(
    ALIKEC_alike, target, current, type.mode, int.tol, attr.mode,
    suppress.warnings, match.call.env
  )

#' @export

.alike <- function(target, current) .Call(ALIKEC_alike_fast, target, current)

#' Similar to \code{`\link{typeof}`}, but Treats Numerics Differently
#'
#' Numerics that look like integers at tolerance \code{`tolerance`} are reported
#' as integers.  Otherwise the same as \code{`typeof`}.
#'
#' \code{`tolerance`} controls what is considered "integer-like". "integer-likeness"
#' is roughly defined as occurring when \code{`all.equal(as.integer(x), x) == TRUE`}.
#' The \code{`tolerance`} value corresponds to the value of the \code{`tolerance`}
#' argument used by \code{`all.equal.numeric`}.  Note though this is only an
#' approximate comparison as \code{`type_of`} does not use \code{`all.equal`}.
#' The default tolerance value is equal to \code{`.Machine$double.eps ^ .5`},
#' though note that this value is pre-computed when the package is loaded and stored in
#' \code{`alike:::MachDblEpsSqrt`} in order to minimize function overhead.
#'
#' \code{`.typeof`} is a slightly faster version that does not allow you to
#' modify the \code{`tolerance`} parameter.
#'
#' @aliases .type_of
#' @param object the object to check the type of
#' @param tolerance see details
#' @return character(1L) the type of the object
#' @export
#' @examples
#'
#' type_of(1.0001)                     # numeric
#' type_of(1 + 1e-20)                  # integer
#' type_of(1)                          # integer
#' type_of(data.frame(a=1:3))          # list

type_of <- function(object, tolerance=MachDblEpsSqrt)
  .Call(ALIKEC_typeof, object, tolerance)

#' @export

.type_of <- function(object)
  .Call(ALIKEC_typeof_fast, object)

#' Compare Types of Objects
#'
#' By default, checks \code{`\link{type_of}`} objects and two objects are
#' considered \code{`type_alike`} if they have the same type.  There is special
#' handling for integers, reals, and functions.
#'
#' For integers and reals, if \code{`current`} is integer or integer-like
#' (e.g. 1.0) it will match real or integer \code{`target`} values.  Closures,
#' built-ins, and specials are all treated as type function.
#'
#' Specific behavior can be tuned with the \code{`mode`} parameter the values
#' of which range from \code{`0L`} to \code{`2L`}, with a lower value
#' corresponding to more relaxed comparison level.
#'
#' \itemize{
#'   \item 0: integer like reals (e.g. \code{`1.0`}) can match against integer
#'     templates, and integers always match real templates; all
#'     function types are considered of the same type
#'   \item 1: integers always match against numeric templates, but not vice
#'     versa, and integer-like reals are treated only as reals; functions only
#'     match same function type (i.e. closures only match closures, builtins
#'     builtins, and specials specials)
#'   \item 2: types must be equal for all objects types (for functions, this is
#'     unchanged from 1)
#' }
#'
#' @seealso type_of, alike
#' @aliases .type_alike
#' @param target the object to test type alikeness against
#' @param current the object to test the type alikeness of
#' @param mode integer(1L) in 0:2, see details
#' @param tolerance see \code{`tolerance`} parameter for \code{`\link{type_of}`}
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
#' @aliases name_compare, class_compare, dimname_compare, dim_compare
#' @keywords internal
#' @param int_mode

attr_compare <- function(target, current, attr.mode=0L)
  .Call(ALIKEC_compare_attributes, target, current, attr.mode)

#' @export

name_compare <- function(target, current)
  .Call(ALIKEC_compare_names, target, current)

class_compare <- function(target, current, rev)
  .Call(ALIKEC_compare_class, target, current, rev)

dimname_compare <- function(target, current)
  .Call(ALIKEC_compare_dimnames, target, current)

dim_compare <- function(
  target, current, tar_obj=integer(), cur_obj=integer(), rev=0L
)
  .Call(ALIKEC_compare_dims, target, current, tar_obj, cur_obj, rev);

#' @export

lang_alike <- function(target, current, match.call.env=parent.frame())
  .Call(ALIKEC_lang_alike, target, current, match.call.env)

#' @export

fun_alike <- function(target, current)
  .Call(ALIKEC_fun_alike, target, current)

#' @export

dep_alike <- function(obj, lines)
  .Call(ALIKEC_deparse, obj, lines)

#' @export

match_call_alike <- function(call, env)
  .Call(ALIKEC_match_call, call, quote(match.call(NULL, quote(NULL))), env)

#' Used for testing C code
#'
#' @export
#' @keywords internal

alike_test <- function() .Call("ALIKEC_test", PACKAGE="alike")

#' Pre-calculated Precision Level
#'
#' Used to limit overhead of calls the require use of \code{`.Machine$double.eps ^ 0.5`}

MachDblEpsSqrt <- .Machine$double.eps ^ 0.5
