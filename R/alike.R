#' Compare Object Structure
#'
#' Similar to \code{\link{all.equal}}, but compares object structure rather than
#' value.  The \code{target} argument defines a template that the \code{current}
#' argument must match.
#'
#' Exactly what makes two objects \code{alike} is complex, but should be
#' intuitive.  The best way to understand "alikeness" is to review the examples.
#' If you are interested in more details, see the vignette
#' (\href{../doc/alike.html}{vignette("alike")}).
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
#' @return TRUE if target and current are alike, character(1L) describing why
#'   they are not if they are not
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
#' # NULL matches anything when nested in a list, but not top level
#'
#' alike(list(NULL, NULL), list(iris, mtcars))
#' alike(NULL, mtcars)
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
#' # Easily create a template to match a particular data frame structure
#'
#' alike(abstract(iris), iris[sample(seq(nrow(iris), 5)), ])     # TRUE
#' alike(abstract(iris), iris[sample(seq(nrow(iris), 5)), -2])   # FALSE, missing second column
#'
#' # factor levels must match; makes sense, otherwise it really is n9t the same
#' # type of data (note this is a recursive comparison); for better
#' # understanding of error examine `levels(df.tpl[[2]])` and
#' # `levels(df.cur2[[2]])`
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

.alike <- function(target, current, .settings=NULL)
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

