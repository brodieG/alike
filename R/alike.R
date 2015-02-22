#' Compare Object Structure
#'
#' Similar to \code{\link{all.equal}}, but compares object structure rather than
#' value.  The \code{target} argument defines a template that the \code{current}
#' argument must match.
#'
#' @section alikeness:
#'
#' Generally speaking two objects are alike if they are of the same type (as
#' determined by \code{\link{type_alike}}) and length.  Attributes on the
#' objects are required to be recursively \code{alike}, though the following
#' attributes are treated specially: \code{class}, \code{dim}, \code{dimnames},
#' \code{names}, \code{row.names}, \code{levels}, \code{tsp}.
#'
#' Exactly what makes two objects \code{alike} is complex, but should be
#' intuitive.  The best way to understand "alikeness" is to review the examples.
#' For a thorough exposition see \href{../doc/alike.html}{the vignette}.
#'
#' @section \code{.alike} and \code{.alike2}:
#'
#' These are slightly faster versions of \code{alike} that are available if you
#' are trying to squeeze out that last microsecond (literally) from your code.
#' See \href{../doc/alike.html}{the vignette} for details.
#'
#' @export
#' @import cstringr
#' @useDynLib alike, .registration=TRUE, .fixes="ALIKEC_"
#' @seealso \code{\link{type_alike}}, \code{\link{type_of}},
#'   \code{\link{abstract}}
#' @param target the template to compare the object to
#' @param current the object to determine alikeness to the template
#' @param type.mode integer(1L) in 0:2, see \code{mode} parameter to
#'   \code{\link{type_alike}}
#' @param int.tol numeric(1L) see \code{tolerance} parameter to
#'   \code{\link{type_alike}}
#' @param attr.mode integer(1L) in 0:2 determines strictness of attribute
#'   comparison, see \href{../doc/alike.html}{vignette}
#' @param suppress.warnings logical(1L)
#' @param match.call.env when matching calls, what frame to look up functions
#'   definitions in to run \code{match.call} on
#'   (see \href{../doc/alike.html}{vignette})
#' @param settings a substitue for all parameters outside of \code{target} and
#'   \code{current} when using \code{.alike}; generate using
#'   \code{alike_settings}
#' @return TRUE if target and current are alike, character(1L) describing why
#'   they are not if they are not
#' @examples
#' # Type comparison
#'
#' alike(1L, 1.0)         # TRUE, because 1.0 is integer-like
#' alike(1L, 1.1)         # FALSE, 1.1 is not integer-like
#' alike(1.1, 1L)         # TRUE, by default, integers are always considered real
#'
#' # Scalarness can now be checked at same time as type
#'
#' alike(integer(1L), 1)            # integer-like and length 1?
#' alike(logical(1L), TRUE)         # logical and length 1?
#' alike(integer(1L), 1:3)
#' alike(logical(1L), c(TRUE, TRUE))
#'
#' # Zero length match any length of same type
#'
#' alike(integer(), 1:10)
#' alike(1:10, integer())   # but not the other way around
#'
#' # Recursive objects compared recursively
#'
#' alike(
#'   list(integer(), list(character(), logical(1L))),
#'   list(1:10, list(letters, TRUE))
#' )
#' alike(
#'   list(integer(), list(character(), logical(1L))),
#'   list(1:10, list(letters, c(TRUE, FALSE))
#' )
#' # `NULL` is a wild card when nested within recursive objects
#'
#' alike(list(NULL, NULL), list(iris, mtcars))
#' alike(NULL, mtcars)    # but not at top level
#'
#' # Since `data.frame` are lists, we can compare them recursively:
#'
#' iris.fake <- transform(iris, Species=as.character(Species))
#' alike(iris, iris.fake)
#' iris.fake2 <- transform(iris, Species=factor(Species, levels=c(levels(Species, "americana"))))
#' alike(iris, iris.fake2)  # we even check attributes (factor levels must match)!
#'
#' # We can use partially specified objects as templates
#'
#' iris.tpl <- abstract(iris)
#' str(iris)
#' alike(iris.tpl, iris)
#' alike(iris.tpl, iris[sample(1:nrow(iris), 10), ])    # any row sample of iris matches our iris template
#' alike(iris.tpl, iris[c(2, 1, 3, 4, 5)])              # but column order matters
#'
#' # Also works with matrices / arrays
#'
#' alike(matrix(integer(), 3, 3), matrix(1:9, nrow=3))         # 3 x 3 integer
#' alike(matrix(integer(), 3, 3), matrix(runif(9), nrow=3))    # 3 x 3, but not integer!
#' alike(matrix(integer(), 3), matrix(1:12, nrow=3))           # partial spec, any 3 row integer matrix
#' alike(matrix(integer(), 3), matrix(1:12, nrow=4))
#' alike(matrix(logical()), array(rep(TRUE, 8), rep(2, 3)))    # Any logical matrix (but not arrays)
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
#'
#' # You can compare language objects; these are alike if they are self
#' # consistent; we don't care what the symbols are, so long as they are used
#' # consistently across target and current:
#'
#' alike(quote(x + y), quote(a + b))   # TRUE, symbols are consistent (adding two different symbols)
#' alike(quote(x + y), quote(a - b))   # FALSE, different function
#' alike(quote(x + y), quote(a + a))   # FALSE, inconsistent symbols

alike <- function(
  target, current, type.mode=0L, int.tol=MachDblEpsSqrt, attr.mode=0L,
  suppress.warnings=FALSE, match.call.env=parent.frame()
)
  .Call(
    ALIKEC_alike, target, current, type.mode, int.tol, attr.mode,
    suppress.warnings, match.call.env
  )

#' @rdname alike
#' @export

.alike <- function(target, current, settings=NULL)
  .Call(ALIKEC_alike_fast1, target, current, settings)

#' @rdname alike
#' @export

.alike2 <- function(target, current)
  .Call(ALIKEC_alike_fast2, target, current)

#' @rdname alike
#' @export

alike_settings <- function(
  type.mode=0L, int.tol=MachDblEpsSqrt, attr.mode=0L, suppress.warnings=FALSE,
  match.call.env=parent.frame()
)
  list(type.mode, int.tol, attr.mode, suppress.warnings, match.call.env)

#' Similar to \code{\link{typeof}}, but Treats Numerics Differently
#'
#' Numerics that look like integers at tolerance \code{tolerance} are reported
#' as integers.  Otherwise the same as \code{typeof}.
#'
#' \code{tolerance} controls what is considered "integer-like". "integer-likeness"
#' is roughly defined as occurring when \code{all.equal(as.integer(x), x) == TRUE}.
#' The \code{tolerance} value corresponds to the value of the \code{tolerance}
#' argument used by \code{all.equal.numeric}.  Note though this is only an
#' approximate comparison as \code{type_of} does not use \code{all.equal}.
#' The default tolerance value is equal to \code{.Machine$double.eps ^ .5},
#' though note that this value is pre-computed when the package is loaded and stored in
#' \code{alike:::MachDblEpsSqrt} in order to minimize function overhead.
#'
#' \code{.typeof} is a slightly faster version that does not allow you to
#' modify the \code{tolerance} parameter.
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
#' By default, checks \code{\link{type_of}} objects and two objects are
#' considered \code{type_alike} if they have the same type.  There is special
#' handling for integers, reals, and functions.
#'
#' For integers and reals, if \code{current} is integer or integer-like
#' (e.g. 1.0) it will match real or integer \code{target} values.  Closures,
#' built-ins, and specials are all treated as type function.
#'
#' Specific behavior can be tuned with the \code{mode} parameter the values
#' of which range from \code{0L} to \code{2L}, with a lower value
#' corresponding to more relaxed comparison level.
#'
#' \itemize{
#'   \item 0: integer like reals (e.g. \code{1.0}) can match against integer
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
#' @param tolerance see \code{tolerance} parameter for \code{\link{type_of}}
#' @export

type_alike <- function(target, current, mode=0L, tolerance=MachDblEpsSqrt)
  .Call(ALIKEC_type_alike, target, current, mode, tolerance)

#' @export

.type_alike <- function(target, current)
  .Call(ALIKEC_type_alike_fast, target, current)

