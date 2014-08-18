#' Recursively Extract Elements and Indeces From List
#' 
#' \code{`index_structure`} produces a list with a vector for every object in \code{`x`},
#' where each vector contains the indeces required to extract the corresponding object
#' from \code{`x`}.  For example, in:
#' \code{
#' lst <- list(1, 2, list(1:3, "hello"))
#' }
#' the vector corresponding to "hello" would be c(3L, 2L) because
#' \code{
#' lst[[3L]][[2L]] == "hello"
#' }
#' These index vectors are "recursive" index vectors, not index vectors to use with
#' \code{`\link{[}`}.
#' 
#' By default \code{`index_structure`} will return "recursive" indeces only for
#' terminal nodes.  Zero length recursive structures will be treated as terminal
#' nodes (e.g., in \code{list(1, list(), 3)}, the second element is considered
#' terminal).  You can get all intermediate objects indeces as well with 
#' \code{`terminal.nodes.only`=FALSE}.
#' 
#' \code{`extract_with_indices`} will extract items from lists using the recursive index 
#' vectors produced by \code{`index_structure`} (see examples for more details). This
#' essentially results in a "flat" list, though it is not the same as what you
#' get with a simple application of \code{`\link{rapply}`} or \code{`\link{unlist}`}
#' because vectors are not collapsed together (e.g. \code{list(1:3, letters[5:7]))} is
#' considered flat and won't be simplified further).
#' 
#' The flattening process is implemented as two separate functions rather than 
#' just a single function because the intermediate list of "recursive"
#' index vectors captures the structure of \code{`x`}, whereas the flat list only
#' captures its values (i.e. values only wouldn't distinguish between 
#' \code{list(list("a", "b"), "c")}) and \code{list("a", "b", "c")}.
#' 
#' If you only wish to extract one item from the list use \code{`extract_with_index`}.
#' 
#' \code{`index_structure`} is implemented as an S3 generic, with methods available
#' for lists and calls. \code{`extract_with_indices`} and \code{`extract_with_index`}
#' are also generic, although at the time of this writing only the default method
#' is implemented (and will work with both lists and calls).
#' 
#' \code{`\link{alike}`} methods are implemented for recursive indeces in
#' package.
#' 
#' @note can probably be optimized quite a bit in terms of performance, but jury's
#'   out on whether this is necessary (probably will be, TBD).  Main performance
#'   improvement would come from sizing list for index vectors ahead of time and
#'   populating it with <<- (as per stack overflow answer to flatten/rapply list Q.)
#' @aliases extract_with_index extract_with_indices
#' @seealso \code{`\link{terminal_indices}`}, \code{`\link{is.recursive_index}`}
#' @export
#' @param x the recursive object to generate indeces for
#' @param terminal.nodes.only set to FALSE if you want all intermediate recursive
#'   objects in addition to the "leaves" of the recursive structure (see examples).
#'   Be careful with this setting if you are dealing with a recursive structure
#'   containing large objects as this could lead to the large object being copied
#'   many, many times over.
#' @param recurse.exclude function that will evaluate whether an element of \code{`x`}
#'   should be recursed into or not, should accept only one argument
#' @param index.list a list of "recursive" index vectors as produced by \code{`index_structure`}
#' @param index a "recursive" index as contained in \code{`index.list`} 
#' @param ... included for compatibility with future methods
#' @return \itemize{
#'   \item for \code{`index_structure`}, a list of "recursive" index vectors (numeric vectors
#'     of class "recursive_index") for every object in \code{`x`}.  The list will inherit 
#'     from "recursive_index_list", and also from either "recursive_terminal_index_list"
#'     or "recursive_full_index_list" depending on the value of \code{`terminal.nodes.only`}).
#'   \item for \code{`extract_with_index`} and \code{`extract_with_indeces`}, the extracted
#'     object or a list of extracted objects, respectively
#' }
#' @examples
#' lst <- list(list(1L, "a"), TRUE, data.frame(nums=1L:3L, alfs=letters[5L:7L]))
#' 
#' index_structure(lst)
#' extract_with_indices(lst, index_structure(lst))   # data.frame is a list, so it is recursed
#' extract_with_indices(lst, index_structure(lst, terminal.nodes.only=FALSE))  # can also see intermediate objects
#' extract_with_indices(lst, index_structure(lst, terminal.nodes.only=FALSE, recurse.exclude=is.data.frame))  # and not recurse into data.frames
#' try(extract_with_indices(lst, c(5, index_structure(lst, terminal.nodes.only=FALSE, recurse.exclude=is.data.frame))))  # testing error handling
#'
#' call <- quote(sum(1:3)/mean(1:3))
#' extract_with_indices(call, index_structure(call))
#' extract_with_indices(call, index_structure(call, terminal.nodes.only=FALSE))

index_structure <- function(x, terminal.nodes.only=TRUE, recurse.exclude=function(y) FALSE, ...) {
  UseMethod("index_structure")
}
#' @method index_structure default
#' @S3method index_structure default

index_structure.default <- function(x, terminal.nodes.only=TRUE, recurse.exclude=function(y) FALSE, ...) {
   
  # Things with classes, but that are still lists or calls should be evaluated with appropriate
  # functions

  if(is.list(x)) {
    return(index_structure.list(x, terminal.nodes.only, recurse.exclude, ...))
  } else if (is.call(x)) {
    return(index_structure.call(x, terminal.nodes.only, recurse.exclude, ...))    
  } else {
    # Possible implementation would be to recursively `as.list` anything that has a length
    # and can be as.listed
    err <- simpleError(paste0("No `index_structure` method for objects of class ", deparse(class(x))))
    class(err) <- c("no_struct_method", class(err))
    stop(err)    
  } 
}
#' @method index_structure list
#' @S3method index_structure list

index_structure.list <- function(x, terminal.nodes.only=TRUE, recurse.exclude=function(y) FALSE, ...) {
  if(!is.list(x)) stop("Argument `x` must be a list.")
  index_structure_common(x, terminal.nodes.only=terminal.nodes.only, recurse.exclude=recurse.exclude, recurse.include=is.list, ...)
}
#' @method index_structure call
#' @S3method index_structure call

index_structure.call <- function(x, terminal.nodes.only=TRUE, recurse.exclude=function(y) FALSE, ...) {
  if(!is.call(x)) stop("Argument `x` must be a call.")
  index_structure_common(x, terminal.nodes.only=terminal.nodes.only, recurse.exclude=recurse.exclude, recurse.include=is.call, ...)
}
#' @method index_structure formula
#' @S3method index_structure formula

index_structure.formula <- function(x, terminal.nodes.only=TRUE, recurse.exclude=function(y) FALSE, ...) {
  if(!is.call(x) || !inherits(x, "formula")) stop("Argument `x` must be a formula.")
  index_structure_common(x, terminal.nodes.only=terminal.nodes.only, recurse.exclude=recurse.exclude, recurse.include=is.call, ...)
}
#' @method index_structure expression
#' @S3method index_structure expression

index_structure.expression <- function(x, terminal.nodes.only=TRUE, recurse.exclude=function(y) FALSE, ...) {
  stop("Not implemented yet.")
}
#' Helper function for index_structure
#' 
#' Only intended for use by other functions in package.
#' 
#' Currently used for \code{`index_structure.list`} and
#' \code{`index_structure.call`}
#' 
#' @keywords internal
#' @inheritParams index_structure
#' @param recurse.include 1 parameter function that determines whether an object is a recursive
#'   element and should be recursed into (e.g., use \code{`\link{is.list}`}) to recurse into
#'   list like objects.

index_structure_common <- function(
  x, terminal.nodes.only=FALSE, recurse.include, 
  recurse.exclude=function(y) FALSE, ...
) {
  if(!is.logical(terminal.nodes.only) || length(terminal.nodes.only) != 1L) {
    stop("Argument `terminal.nodes.only` should be a 1 length Logical.")
  }
  if(mode(recurse.exclude) != "function") {
    stop("Argument `recurse.exclude` should be a function.")
  }
  if(mode(recurse.include) != "function") {
    stop("Argument `recurse.include` should be a function.")
  }
  struct.class <-  if(terminal.nodes.only) {
    c("recursive_terminal_index_list", "recursive_index_list") 
  } else {
    c("recursive_full_index_list", "recursive_index_list") 
  }
  # Recursively run anonymous function below, which will dig into the list and 
  # collect the index structure; general logic is that if object has length, 
  # recurse, and then concatenate the return value from recursion to current
  # index.

  tryCatch(
    {
      if(!recurse.include(x) || recurse.include(x) && length(x) == 0L) {
        return(structure(list(), class=struct.class))  # NOTHING TO RECURSE INTO
      } 
      index.rec <- (
        function(x) {
          index <- list()
          dont.exclude <- !isTRUE(recurse.exclude(x))
          include <- isTRUE(recurse.include(x))
          if(include && dont.exclude) {   # Only dive in if a list, and user function doesn't exclude it
            for(i in seq_along(x)) {
              if(length(x[[i]]) != 0L) {
                res <- Recall(x[[i]])
              } else {
                res <- integer(0L) # not sure about this
                # next  # If next level in is an empty list and we only want terminal nodes, don't return index vector
              }
              if(terminal.nodes.only) j <- integer(0L) else j <- i  # prevents current list from being returned
              if(length(res) == 0L) { # next object is a terminal node
                # NA here acts as tag that we will remove later but we need now to 
                # easily track which indices are terminal
                
                index <- c(index, list(c(i, NA_integer_)))
              } else {
                # add items to index list, including the original index list, as 
                # well as the current level if we want non-terminal nodes, as well
                # as the recursed nodes

                index <- c(
                  index, j,      # j is current level, only if including non terminal
                  lapply(        # for each recursed node, add current level concat to the recursed stuff     
                    res,
                    function(x, i) c(i, x),
                    i
            ) ) } }
          } else {   # Terminal node, index should be recorded at previous recursion level,
            return(integer(0L))  
          }
          return(index)
      } ) (x)
    },
    error=function(e) {
      if(
        identical(conditionCall(e), quote(recurse.exclude(x))) ||
        identical(conditionCall(e), quote(recurse.include(x)))
      ) {
        stop(
          "Failed executing `recurse.include` or `recurse.exclude`; 
          please make sure supplied function works properly"
        )
      } else {
        stop("Logic Error: unhandled exception; please contact package maintainer")
  } } )
  # Apply classes to elements and to the list of elements

  rec.len <- length(index.rec)
  terminal <- logical(rec.len)
  for(i in seq(rec.len)) {
    sub.len <- length(index.rec[[i]])
    if(is.na(index.rec[[i]][[sub.len]])) {
      index.rec[[i]] <- index.rec[[i]][-sub.len]
      terminal[[i]] <- TRUE
  } }
  class(index.rec) <- struct.class
  attr(index.rec, "terminal.indices") <- terminal
  index.rec
}
#' Remove Non-Terminal Indeces From Index List
#' 
#' Any recursive indices that are fully contained in another index in the index
#' list (starting with the first element of this other index) are by definition
#' non-terminal, and are removed.  For example, in:
#' \preformatted{
#' list(
#'   c(1, 2, 3),
#'   c(1, 2, 3, 4),
#'   c(2, 1, 2, 3, 4)
#' )
#' }  
#' the first recursive index is fully contained in the second, so it is not a
#' terminal index.  However, the second index is terminal because the \bold{starting}
#' sequence of the third index doesn't fully contain it.
#' 
#' @seealso \code{`\link{index_structure}`}
#' @export
#' @param index.list list of recursive index vectors
#' @return recursive_terminal_index_list
#' 
terminal_indices <- function(index.list) {
  if(!is.recursive_index_list(index.list)) 
    stop("Argument `index.list` must be a recursive index list.")
  if(is.recursive_terminal_index_list(index.list)) return(index.list)
  index.list.new <- index.list[attr(index.list, "terminal.indices")]
  index.list.attrs <- attributes(index.list)
  do.call(
    structure,
    c(
      list(
        index.list.new,
        class=c(
          "recursive_terminal_index_list",
          class(index.list)[class(index.list) != "recursive_full_index_list"]
        ),
        terminal.indices=rep(TRUE, length(index.list.new))
      ),
      index.list.attrs[! names(index.list.attrs) %in% c("class", "terminal.indices")]
  ) )
}
#' Test Whether Objects Are Recursive Index Objects
#' 
#' @export
#' @aliases is.recursive_index_list is.recursive_full_index_list is.recursive_terminal_index_list
#' @seealso \code{`\link{index_structure}`}
#' @param x object to test
#' @return logical 1 length TRUE if object is what is expected, FALSE otherwise

is.recursive_index <- function(x) {
  is.numeric(x) && min(as.integer(x) > 0L && inherits(x, "recursive_index"))
}
#' @export

is.recursive_index_list <- function(x) {
  if(!is.list(x) || !inherits(x, "recursive_index_list")) return(FALSE)
  # if(!all(vapply(x, is.recursive_index, logical(1L)))) return(FALSE)  # too expensive
  TRUE
}
#' @export

is.recursive_full_index_list <- function(x) {
  if(is.recursive_index_list(x) && inherits(x, "recursive_full_index_list")) return(TRUE)
  FALSE
}
#' @export

is.recursive_terminal_index_list <- function(x) {
  if(is.recursive_index_list(x) && inherits(x, "recursive_terminal_index_list")) return(TRUE)
  FALSE
}
