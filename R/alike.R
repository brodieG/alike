# ARE THERE ANY PROBLEMS WHEN TESTING EXPRESSIONS? I.E. ARE WE TESTING THE
# EXPRESSION THE USER PASSED AS AN ARGUMENT, OR THE EVALUATION OF THAT
# EXPRESSION?  I ACTUALLY DON'T THINK THIS IS A PROBLEM BECAUSE IN THIS
# CASE WE'RE NOT TRYING TO RETRIEVE THE UNEVALUATED EXPRESSION, BUT NEED 
# TO MAKE SURE DUE TO WEIRD STUFF SUCH AS 5 ACTUALLY BEING AN EXPRESSION
# IN ADDITION TO AN INTEGER.

#' Compare R Objects
#' 
#' The \code{`*_alike`} functions facilitate the comparison of object structure 
#' rather than comparison of exact object values.  For example:
#' \preformatted{
#' alike(matrix(numeric(), ncol=7), matrix(1:14, nrow=2)) == TRUE
#' }
#' Clearly the two objects are not equal, but the second object has the
#' structure as defined by the first object: a numeric matrix with seven
#' columns.  Note also that even though the second object \code{`obj`}
#' is of type "integer" it is allowed to match against the type "double"
#' of \code{`obj.reference`}.
#' 
#' When two objects are considered "alike", the function will
#' return TRUE, and otherwise the function will return a 1 length character
#' vector describing the reason for the inequality (exception: \code{`type_alike`}.
#' which returns FALSE).  Because of this, if you are using these 
#' functions in a process, you should consider the following structure:
#' 
#' \preformatted{
#' if(!isTRUE(msg <- alike(obj2, obj1))) stop(\dQuote{msg})
#' } 
#' 
#' Describing what constitutes alikeness for the \code{`*_alike`} functions is
#' relatively complex, but the net result is intended to be intuitive to grasp.
#' The best way to understand these functions is to look at the examples,
#' and you are encouraged to do so rather than read the rest of the documentation.
#' 
#' @section \code{`alike`}:
#' 
#' Recursively compares object structure, attributes, and classes.  
#' 
#' Structure alikeness is determined with \code{`struct_alike`}, attribute
#' alikeness with \code{`attribute_alike`}, and class alikeness with 
#' with \code{`class_alike`} (note that class is NOT checked by \code{`attribute_alike`}).
#' 
#' The recursive nature of the comparison means that, among other things,
#' \code{`alike`} will recurse into \code{`\link{data.frame}`}s
#' and compare the component vectors (see examples).
#' 
#' \code{`alike`} retrieves the recursive index structure of objects by
#' using \code{`\link{index_structure}`}, which means \code{`alike`}
#' will only recurse on objects that have \code{`\link{index_structure}`}
#' methods defined.
#' 
#' @section \code{`struct_alike`}:
#' 
#' In order for structure between two objects to be considered equal, the
#' following must be true:
#' \itemize{
#'   \item \code{type_alike(obj.reference, obj)}
#'   \item \code{`obj.reference`} is length zero, then \code{`obj`} may
#'     be any length, otherwise both objects must have the same length
#' }
#' @section \code{`class_alike`}:
#' 
#' Currently supports two comparison types:
#' \itemize{
#'   \item \code{class.mode=="common.ancenstry"}: which will allow 
#'     classes to match so long as \code{`obj`} has all the classes 
#'     in \code{`obj.reference`} in the same order and ending
#'     with the same ancestor (i.e. if \code{attr(obj.reference, "class") == c('a', 'b', 'c')} 
#'     then the following \code{`obj`} class could be \code{c('a', 'b', 'c')}, 
#'     or \code{c('x', 'a', 'b', 'c')}, but not \code{c('b', 'a', 'c')} or 
#'     \code{c('a', 'b', 'c', 'x')})
#'   \item \code{class.mode=="equal"}: then the classes must be equal between 
#'     the two objects as per \code{`compare.mode`}
#' } 
#' Note that the attributes of the class are also compared (relevant primarily 
#' for S4 classes where the attribute references the environment of the class).
#' 
#' @section \code{`attribute_alike`}:
#' 
#' In order for function to succeed the following must be true:
#' \itemize{
#'   \item any attributes present in \code{`obj.reference`} must be present in 
#'     \code{`obj`}, and if \code{`all.attrs`==TRUE}, then all attributes present
#'     in \code{`obj`} must be present in \code{`obj.reference`}.
#'   \item compared attributes must be equal unless \code{`obj.reference`} is zero 
#'     length in which case they must be \code{`type_alike`} (note 
#'     exceptions below).
#'   \item if \code{`special.attrs == TRUE`}, then: 
#'   \itemize{
#'     \item \code{`dim`} must be identical for both objects, except that if a value in 
#'       \code{`dim`} for \code{`obj.reference`} is zero, the corresponding value in 
#'       \code{`obj`} may be any positive integer
#'     \item \code{`dimnames`} must be identical in both objects, except that if a value 
#'       in \code{`dimnames`} for \code{`obj.reference`} is NULL, the corresponding value
#'       in \code{`obj`} may be any character vector of appropriate length for the
#'       corresponding \code{`dim`} value.
#'   }
#' }
#' Attributes need not be in the same order, and as of the time of this
#' writing there is no way to force the check to require the same order.
#' 
#' @section S3 Methods:
#' 
#' The \code{`*_alike`} functions are implemented as S3 methods so that 
#' you can provide methods for objects that require special treatment.  
#' This documentation focuses on the default methods.
#'
#' @section Low Level Comparisons:
#' 
#' Objects often have multiple components (values, attributes, classes, 
#' recursive structures, etc.), so when comparing two objects we will need to
#' compare the objects themselves, their components, and so on.  Within
#' these comparisons, there are two types of comparisons: type comparisons
#' and value comparisons.  Even though the \code{`*_alike`} functions focus
#' on structure comparisons, value comparisons are also necessary.  For
#' example, when verifying that the length of two objects are equal, we 
#' compare the values of their lengths.
#' 
#' Type comparisons are executed with \code{`type_alike`} and
#' \code{`\link{type_of}`}.  The latter function is very similar to 
#' \code{`\link{typeof}`}, but it allow doubles for which:
#' \preformatted{
#' all.equal(as.integer(x), x) == TRUE
#' }
#' to be considered as integers (i.e. 1.0 would be considered an integer).
#' This behavior can be modifed by the \code{`int.strict`} parameter.  The 
#' default behavior occurs when \code{`int.strict` == 0L}, but you can force
#' distinguishing between integers and integer like numerics with 
#' \code{`int.strict` == 1L} or \code{`int.strict` == 2L}. \code{`type_of`}
#' treats the two strictness levels outside of \code{`0L`} equivalently; the
#' option to use \code{`c(1L, 2L`} is there for compatibility with 
#' \code{`type_alike`}.
#'
#' \code{`type_alike`} is mostly a wrapper around \code{`type_of`}, though 
#' please note that it is assymetric. For instance, if \code{`obj.reference`}
#' is an integer, \code{`obj`} must also be an integer, but if 
#' \code{`obj.reference`} is a double, then \code{`obj`} may be either an 
#' integer or a double.  In the special case that \code{`obj.reference`} is an 
#' integer-like double (i.e. \code{type_of(x) == "integer" && typeof(x) == "double"}) 
#' then \code{`obj`} may be either a double or an integer.  You can disable
#' this special case by setting \code{`int.strict` == 2L} at which point an int 
#' \code{`obj`} will no longer match an integer like double \code{`ref`}.
#' 
#' Value comparisons for atomic vectors are executed with \code{`comp.fun`},
#' which by default is the function \code{`\link{all_equal}`}, which itself is
#' similar, but not identical, to \code{`\link{all.equal}`}.  \code{`comp.fun`}
#' is used primarily when comparing the values of attributes.  One difference
#' between 
#' 
#' \code{`==`} if 
#' \code{`int.strict` == 0L}, otherwise they are exectued with 
#' \code{`\link{identical}`}.  Other objects are always compared with 
#' \code{`identical`}.  Originally the code was designed to use 
#' \code{`\link{all.equal}`}, but this was too slow.
#' 
#' Note that this is how the default method handles comparisons; other
#' methods may do things differently, but always with the objective of
#' providing an intuitive answer to whether two objects are \code{`alike`}.
#' 
#' @note \code{`\link{all.equal}`} is a generic function, so you can 
#'   potentially write methods for your objects, though we caution this may 
#'   cause unpredictable behavior since we compare both your objects AND
#'   its components, and the latter are probably not going to be classed 
#'   (so if your custom method compares your object values and attributes,
#'   we're still going to compare the attributes separately).
#' 
#' @aliases alike, struct_alike, class_alike, attributes_alike, type_alike, type_of
#'
#' @export
#' @param obj an R object
#' @param obj.reference an R object
#' @param int.strict 1 length int %in% 0:2(see "Low Level Comparisons" in details)
#' @param special.attrs 1 length logical (see \code{`attributes_alike`} section 
#'   in details)
#' @param all.attrs 1 length logical (see \code{`attributes_alike`} section 
#'   in details)
#' @param class.mode 1 length character, one of c("common.ancestry", "equal") 
#'   (see \code{`class_alike`} section in details)
#' @param comp.fun function compares values, particularly of the attributes of 
#'   the objects being checked; must return 1 length logical; anything other than
#'   TRUE as a return value is considered a failed comparison; default is 
#'   \code{`\link{all_equal}`}
#' @param ... arguments to pass on to \code{`comp.fun`}
#' @return TRUE if attributes are equal, 1 length character vector describing the first reason
#'   encountered as to why \code{`obj`} is not \code{`alike`} to \code{`obj.reference`}
#'  
#' @examples
#' alike(1L:3L, c(1.0, 2.0, 3.0))        # TRUE
#' alike(numeric(), c(1.0, 2.0, 3.0))    # TRUE
#' alike(numeric(3L), c(1.0, 2.0, 3.0))  # TRUE
#' alike(numeric(4L), c(1.0, 2.0, 3.0))  # "should have length 4 instead of 3"
#' alike(integer(), c(1.0, 2.0, 3.0))    # TRUE
#' alike(integer(), c(1.01, 2.0, 3.0))   # "should be integer instead of double"
#' alike(matrix(integer(), ncol=4), matrix(1:12, nrow=3))  # TRUE 
#' alike(data.frame(), data.frame(a=1:3, b=letters[1:3]))  # TRUE
#' alike(data.frame(a=integer(), b=factor()), data.frame(a=1:3, b=letters[1:3]))    # TRUE, note this is recursive
#' alike(data.frame(a=factor(), b=factor()), data.frame(a=1:3, b=letters[1:3]))     # "has structure mis-match at index [[1]]; item should have class \"factor\""
#' alike(sd, median)                     # TRUE
#' alike(sd, cor)                        # "does not have the expected formals structure"
#' 
#' type_of(1.0001)                     # numeric
#' type_of(1 + 1e-20)                  # integer
#' type_of(1)                          # integer  
#' type_of(1, int.strict=1L)           # numeric
#' 
#' type_alike("hello", letters)        # TRUE
#' type_alike("hello", list())         # FALSE
#' type_alike(1L, 2L)                  # TRUE
#' type_alike(1L, 2.0)                 # TRUE
#' type_alike(1.0, 2L, int.strict=1L)  # TRUE, 2L can be numeric
#' type_alike(1L, 2.0, int.strict=1L)  # FALSE, 2.0 cannot be integer
#' type_alike(1.0, 2L, int.strict=2L)  # FALSE, 2L cannot be numeric w/ strict=2L

alike <- function(obj.reference, obj,  ...) {
  UseMethod("alike")
}

# NEED TO TEST ALL MODIFICATIONS TO alike.default AND alike.call.  FOR THE LATTER NEED
# TO CHECK THAT reduce_to_funs DOESN'T STRIP ATTRIBUTES, ETC.  ALSO, NEED TO CHECK
# THAT WE GET DESIRED OUTCOMES FOR ALL THE ALIKE METHODS
# 
#' @rdname alike
#' @method alike default
#' @S3method alike default

alike.default <- function(
  obj.reference, obj, int.strict=0L, class.mode="common.ancestry", 
  special.attrs=TRUE, all.attrs=FALSE, comp.fun=all_equal, ...
) {
  if(!isTRUE(check <- run_comps(
    obj.reference, obj, int.strict=int.strict, class.mode=class.mode, 
    special.attrs=special.attrs, all.attrs=all.attrs, comp.fun=comp.fun, ...
  ) ) ) return(check)
  struct_compare_fun <- function(obj.reference, obj) {
    alike(
      obj.reference, obj, int.strict=int.strict, class.mode=class.mode, 
      special.attrs=special.attrs, all.attrs=all.attrs, ...
  ) }
  val_compare_fun <- function(obj.reference, obj) {
    run_comps(
      obj.reference, obj, int.strict=int.strict, class.mode=class.mode, 
      special.attrs=special.attrs, all.attrs=all.attrs, comp.fun=comp.fun, ...
    )      
  }
  c(    
    alike_recurse(   # test recursive structure
      obj.reference, obj, compare_fun=val_compare_fun, 
      struct_compare_fun=struct_compare_fun, compare.terminal.nodes.only=FALSE
  ) )  
}
# Helper function for several alike.* methods
# @param compare.terminal.nodes.only, will only do the "recursive" comparison on terminal nodes,
#   but keep in mind that the index structures will be compared in full

alike_recurse <- function(
  obj.reference, obj, compare_fun, struct_compare_fun, 
  compare.terminal.nodes.only
) {
  if(
    isTRUE(
      tryCatch(  # If non_struct_method error, keep going without recursion
        {  
          struct <- index_structure(obj, terminal.nodes.only=FALSE)
          struct.ref <- index_structure(obj.reference, terminal.nodes.only=FALSE)
          length(struct.ref) > 0L
        },
        no_struct_method=function(e) FALSE,
        error=function(e) {stop(e)}
  ) ) ) {
    if(!isTRUE(msg <- struct_compare_fun(struct.ref, struct))) return(msg)  # Are structures alike
    if(compare.terminal.nodes.only) {
      struct <- terminal_indices(struct)
      struct.ref <- terminal_indices(struct.ref)
    }
    ref.call.base <- quote(obj.reference)
    obj.call.base <- quote(obj)

    for(i in seq_along(struct.ref)) {    # Construct call to get each item in structure
      ref.call <- ref.call.base
      obj.call <- obj.call.base
      for(j in seq_along(struct.ref[[i]])) {
        ref.call <- call("[[", ref.call, struct.ref[[i]][[j]])
        obj.call <- call("[[", obj.call, struct.ref[[i]][[j]])
      }
      if(!isTRUE(msg <- eval(call("compare_fun", ref.call, obj.call)))) {
        msg <- paste0("mis-match at index ", paste0("[[", struct.ref[[i]][1L:j], "]]", collapse=""), ": ", msg) 
        return(structure(msg, err.type="value", index=struct.ref[[i]][1L:j]))
      }
  } }
  return(TRUE)
}