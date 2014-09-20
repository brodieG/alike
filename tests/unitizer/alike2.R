library(alike)

alike <- alike2  # over-write alike with the new version so we can just keep the tests for now

unitizer_sect("Atomic", {
  alike(integer(), 1:3)    # TRUE
  alike(integer(5L), 1:3)  # FALSE
  alike(integer(3L), 1:3)  # TRUE
  alike(integer(), 1:3, int.mode=1L)         # TRUE, b/c `:` coerces to integer
  alike(integer(), c(1, 2, 3), int.mode=1L)  # FALSE (compare to above)
  alike(numeric(), c(1, 2, 3))         # TRUE
  alike(numeric(), 1L)                 # TRUE
  alike(numeric(), 1L, int.mode=2L)    # FALSE
  alike(numeric(), c(1.1,.053,41.8))   # TRUE
  alike(integer(3L), 1:3 + .01)
  alike(integer(3L), 1:3 + .Machine$double.eps ^ .5 * 2) # FALSE, integer like Numerics must be under this
  alike(integer(3L), 1:3 + .Machine$double.eps ^ .5)     # TRUE
  alike(integer(4L), letters[1:4])  
  alike(letters[1:4], c("hello", "goodbye", "ba", "da"))  # TRUE
} )
unitizer_sect("lists", {
  lst <-   list(list( 1,  2), list( 3, list( 4, list( 5, list(6, 6.1, 6.2)))))
  lst.2 <- list(list(11, 21), list(31, list(41, list(51, list(61          )))))

  alike(lst, lst.2)     # length mismatch

  lst.3 <- lst.2
  lst.3[[2]][[2]][[2]][[2]] <- matrix(1:9, nrow=3)

  alike(lst, lst.3)     # object type mismatch
  alike(1:10, "hello")  # object type mismatch, no dive

  alike(lst, lst)       # obvious match

  lst.4 <- lst
  lst.4[[2]][[2]] <- list()

  alike(lst.4, lst)     # should match
  alike(lst, lst.4)     # should not match because template has more detail   
} )
unitizer_sect("Matrix & Data Frames", {
  alike(matrix(integer(), ncol=7), matrix(1:21, nrow=3))
  alike(matrix(integer(), nrow=3), matrix(1:21, nrow=3))
  alike(matrix(character(), nrow=3), matrix(1:21, nrow=3))
  alike(matrix(integer(), nrow=4), matrix(1:21, nrow=3))
  alike(matrix(integer(), ncol=3, dimnames=list(NULL, c("R", "G", "B"))), matrix(1:21, ncol=3, dimnames=list(NULL, c("R", "G", "B"))))
  alike(matrix(integer(), nrow=3, dimnames=list(c("R", "G", "B"), NULL)), matrix(1:21, ncol=3, dimnames=list(NULL, c("R", "G", "B"))))
  alike(matrix(integer(), nrow=3, dimnames=list(c("R", "G", "B"), NULL)), matrix(1:9, nrow=3, dimnames=list(NULL, c("R", "G", "B"))))
  alike(matrix(integer(), nrow=3, dimnames=list(c("R", "G", "B"), NULL)), matrix(1:9, nrow=3, dimnames=list(c("R", "G", "B"), c("bacon", "turkey", "bravo"))))
  alike(data.frame(), data.frame(a=1:3, b=letters[1:3]))  # TRUE
  alike(data.frame(a=integer(), b=factor()), data.frame(a=1:3, b=letters[1:3]))        # TRUE, note this is recursive
  alike(data.frame(a=factor(), b=factor()), data.frame(a=1:3, b=letters[1:3]))         # FALSE mis-match at index[[1]]
  alike(list(NULL, structure("a", class="x")), list(NULL, structure("a", class="y")))  # FALSE mis-match at index[[2]] (class)
  
  # TRUE, more complex nested structure

  alike(
    list(integer(), data.frame(a=integer(), b=numeric()), matrix(integer(), nrow=3)),
    list(1:10, data.frame(a=1:200, b=runif(20)), matrix(1:27, nrow=3))
  )
} )
unitizer_sect("Class Matching", {
  obj2 <- structure(numeric())
  obj1 <- structure(numeric(), class="hello")
  alike(obj1, obj2)
  obj2 <- structure(numeric(), class=c(letters[10:12], letters[1:3], letters[8:9]))
  obj1 <- structure(numeric(), class=letters[1:3])
  alike(obj1, obj2)
  alike(obj2, obj1)
  obj2 <- structure(numeric(), class=c("b", "a", "c"))
  alike(obj1, obj2)
  obj2 <- structure(numeric(), class=c("a", "b", "x", "c"))
  alike(obj1, obj2)
  obj2 <- structure(numeric(), class=c("a", "b", "c"))
  alike(obj1, obj2)      # TRUE 
  obj2 <- structure(numeric(), class=c("x", "a", "b", "c"))
  alike(obj1, obj2)      # TRUE
  alike(obj1, obj2, attr.mode=1)   # FALSE
} )
unitizer_sect("S4", {
  setClass("foo", representation(a = "character", b = "numeric"))
  setClass("bar", representation(d = "numeric", c = "numeric"))
  setClass("baz", contains="foo", list(c="character"))

  x <- new("foo")
  y <- new("foo")
  z <- new("bar")
  v <- new("baz")
  w <- structure(list(a=character(), b=numeric()), class="foo")

  alike(x, y)  # TRUE
  alike(x, z)  # FALSE
  alike(x, w)  # FALSE
  alike(w, x)  # FALSE
  alike(x, v)  # TRUE, because v contains x
  alike(v, x)  # FALSE

  # S4 nested in list

  lst.5 <- lst.6 <- lst.2
  lst.5[[2]][[2]][[1]] <- x
  lst.6[[2]][[2]][[1]] <- v

  alike(lst.5, lst.6)  # TRUE
  alike(lst.6, lst.5)  # FALSE, child class is target, so parent can't match

  # Borked S4
  v2 <- v
  class(v2) <- c("baz", "foo")
  alike(x, v2)
} )
unitizer_sect("Non-Standard Class", {
  # Basically ensure that stuff still recurses even if they are lists/calls
  # but have another class
   
  var.1 <- list(1, 2, 3)
  var.2 <- list("hello", list(1, 2, 3), 5)
  class(var.1) <- "marbles"
  class(var.2) <- "marbles"
  alike(var.1, var.2)        # "mis-match at index [[1]]: should be integer instead of character"
} )
unitizer_sect("Functions", {
  alike(sd, median)                     # TRUE
  alike(sd, cor)                        # "does not have the expected formals structure"
} )  
unitizer_sect("Calls / Formulas", {
  alike(quote(1 + 1), quote(x + y))
  alike(quote(fun(1 + 1)), quote(fun(x + y, 9)))
  alike(quote(fun(x + y, 9)), quote(fun(1 + 1)))

  if(exists("fun")) rm(fun)
  alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(NULL, fun2(a, b), 1)))   # sub-optimal error message, but necessary to handle cases that engage match.call
  alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(b=NULL, fun2(a, b), 1))) # clearer what's going on here
  fun <- function(a, b, c) NULL
  alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(NULL, fun2(a, b), 1)))  # Because fun defined, uses match.call() to re-arrange args
  alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(fun2(a, b), NULL, 1)))
  alike(quote(fun(a=1)), quote(fun(b=1)))

  alike(quote(fun(1, 2)), quote(fun(1)))
  alike(quote(fun(1)), quote(fun(1, 2)))

  alike(quote(fun(1, 2)), quote(fun2(1, 2)))
  alike(quote(fun(1, fun2(3))), quote(fun(1, fun(3))))

  alike(x ~ y, z ~ w)
  alike(x ~ y, z ~ w + 1)
  alike(x ~ y + 2, z ~ w + 1)
  alike(x ~ y + z:y, w ~ v + u:v)
  alike(z ~ w + 1, x ~ y)
  alike(y ~ x ^ 2 + x * z + z + w:z, q ~ l ^ 2 + l * j + j + w:j)
  alike(y ~ x ^ 2 + x * z + z + w:z, q ~ l ^ 3 + l * j + j + w:j)

  exp.1 <- parse(text="x + y; fun2(fun(1, 2, 3), z)")
  exp.2 <- parse(text="z + 2; fun(fun2(1, 2, 3), q)")
  exp.3 <- parse(text="z + fun(3); fun(fun2(a, b, c), 3)")

  alike(exp.1, exp.2)
  alike(exp.2, exp.3)
  alike(exp.3, exp.2)
} )
