library(alike)

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
unitizer_sect("NULL values as wildcards", {
  alike(NULL, 1:3)
  alike(1:3, NULL)
  alike(list(NULL, NULL), list(list(list(1, 2, 3)), 1:25))
  alike(list(NULL), list(1, 2))
  alike(list(), list(1, 2))
})
unitizer_sect("Matrix / Arrays", {
  alike(matrix(integer(), ncol=7), matrix(1:21, nrow=3))
  alike(matrix(integer(), nrow=3), matrix(1:21, nrow=3))
  alike(matrix(character(), nrow=3), matrix(1:21, nrow=3))
  alike(matrix(integer(), nrow=4), matrix(1:21, nrow=3))
  alike(matrix(integer(), ncol=3, dimnames=list(NULL, c("R", "G", "B"))), matrix(1:21, ncol=3, dimnames=list(NULL, c("R", "G", "B"))))
  alike(matrix(integer(), nrow=3, dimnames=list(c("R", "G", "B"), NULL)), matrix(1:21, ncol=3, dimnames=list(NULL, c("R", "G", "B"))))
  alike(matrix(integer(), nrow=3, dimnames=list(c("R", "G", "B"), NULL)), matrix(1:9, nrow=3, dimnames=list(NULL, c("R", "G", "B"))))
  alike(matrix(integer(), nrow=3, dimnames=list(c("R", "G", "B"), NULL)), matrix(1:9, nrow=3, dimnames=list(c("R", "G", "B"), c("bacon", "turkey", "bravo"))))

  alike(matrix(1:9, nrow = 3), 1:9)

  # Adding tests from docs

  mx.tpl <- matrix(integer(), ncol=3, dimnames=list(row.id=NULL, c("R", "G", "B")))
  mx.cur <- matrix(sample(0:255, 12), ncol=3, dimnames=list(row.id=1:4, rgb=c("R", "G", "B")))
  mx.cur2 <- matrix(sample(0:255, 12), ncol=3, dimnames=list(1:4, c("R", "G", "B")))

  alike(mx.tpl, mx.cur)
  alike(mx.tpl, mx.cur2)
} )
unitizer_sect("Data Frames", {
  alike(mtcars, 1:3)
  alike(1:3, mtcars)
  alike(data.frame(), data.frame(a=1:3, b=letters[1:3]))  # TRUE
  alike(data.frame(a=integer(), b=factor()), data.frame(a=1:3, b=letters[1:3]))        # TRUE, note this is recursive
  alike(data.frame(a=factor(), b=factor()), data.frame(a=1:3, b=letters[1:3]))         # FALSE mis-match at index[[1]]
  alike(list(NULL, structure("a", class="x")), list(NULL, structure("a", class="y")))  # FALSE mis-match at index[[2]] (class)

  # TRUE, more complex nested structure

  alike(
    list(integer(), data.frame(a=integer(), b=numeric()), matrix(integer(), nrow=3)),
    list(1:10, data.frame(a=1:200, b=runif(20)), matrix(1:27, nrow=3))  # row.names / names (note use `structure` to get around `data.frame` checks)
  )

  df.tpl <- structure(list(1:4, factor(LETTERS[1:4], levels=LETTERS)), row.names=c("one", "", "", ""), names=c("id", ""), class="data.frame")
  df.cur <- `row.names<-`(data.frame(id=5:8, val=factor(LETTERS[21:24], levels=LETTERS)), c("one", "two", "tre", "qtr"))
  df.cur2 <- `row.names<-`(data.frame(id=5:8, val=factor(LETTERS[21:24], levels=LETTERS)), c("uno", "due", "tre", "qtr"))

  alike(df.tpl, df.cur)   # TRUE
  alike(df.cur, df.tpl)   # Nope, names won't match reversed
  alike(df.tpl, df.cur2)  # Nope, row.names won't match

  # NA names

  df.tpl <- structure(list(1:4, letters[1:4]), names=c("id", NA), class="data.frame")
  df.cur <- structure(list(1:4, letters[1:4]), names=c("id", "val"), class="data.frame")

  alike(df.tpl, df.tpl)
  alike(df.tpl, df.cur)

  # special treatment

  alike(mtcars, iris)
  alike(mtcars, mtcars[1:10,])
  alike(mtcars[-5], mtcars)
})

unitizer_sect("Errors", {
  alike(1, 1, int.mode="hello")
  alike(1, 1, int.mode=3)
  alike(1, 1, int.tol="hello")
  alike(1, 1, attr.mode=3)
} )
# Subset of tests for "fast" version

unitizer_sect(".alike", {
  .alike(integer(), 1:3)    # TRUE
  .alike(integer(5L), 1:3)  # FALSE
  .alike(integer(3L), 1:3)  # TRUE
  .alike(integer(), 1:3, int.mode=1L)         # Error, this arg isn't available
  .alike(letters[1:4], c("hello", "goodbye", "ba", "da"))  # TRUE

  .alike(lst, lst.2)     # length mismatch
  .alike(lst, lst.3)     # object type mismatch
  .alike(1:10, "hello")  # object type mismatch, no dive
  .alike(matrix(integer(), ncol=7), matrix(1:21, nrow=3))
  .alike(matrix(integer(), ncol=3, dimnames=list(NULL, c("R", "G", "B"))), matrix(1:21, ncol=3, dimnames=list(NULL, c("R", "G", "B"))))
} )
# These are also part of the examples, but here as well so that issues are
# detected during development and not the last minute package checks

unitizer_sect("Examples", {
 alike(1L, 1.0)         # TRUE, because 1.0 is integer-like
 alike(1L, 1.1)         # FALSE, 1.1 is not integer-like
 alike(1.1, 1L)         # TRUE, by default, integers are always considered real
 alike(integer(), 1:4)  # TRUE, Zero length `target` matches any length `current`
 alike(1:4, integer())  # But not vice versa

 # Scalarness can now be checked at same time as type

 x <- 1
 x.2 <- 1:3
 y <- TRUE
 y.2 <- c(TRUE, TRUE)

 alike(integer(1L), x)
 alike(logical(1L), y)
 alike(integer(1L), x.2)
 alike(logical(1L), y.2)

 # Zero length match any length of same type

 alike(integer(), 1:10)

 # NULL matches anything

 alike(NULL, mtcars)
 alike(list(NULL, NULL), list(iris, mtcars))

 # `alike` will compare data frame columns

 df.tpl <- data.frame(id=integer(), grade=factor(levels=LETTERS[1:6]))
 df.cur <- data.frame(id=c(1, 3, 5), grade=factor(c("A", "F", "B"), levels=LETTERS[1:6]))
 df.cur2 <- data.frame(id=c(1, 3, 5), grade=c("A", "F", "B"))

 alike(df.tpl, df.cur)    # zero row df as `target` matches any length df
 alike(df.cur, df.tpl)    # alike is not "commutative", now `target` is not zero row

 # factor levels must match; makes sense, otherwise it really isn't the same
 # type of data (note this is a recursive comparison); for better understanding
 # of error examine `levels(df.tpl[[2]])` and `levels(df.cur2[[2]])`

 alike(df.tpl, df.cur2)

 alike(list(integer(), df.tpl), list(1:4, df.cur))  # recursive comparison
 alike(matrix(integer(), 3), matrix(1:21, ncol=7))  # partially specified dimensions

 # In order for objects to be alike, they must share a family tree, not just
 # a common class

 obj.tpl <- structure(TRUE, class=letters[1:3])
 obj.cur.1 <-  structure(TRUE, class=c("x", letters[1:3]))
 obj.cur.2 <-  structure(TRUE, class=c(letters[1:3], "x"))

 alike(obj.tpl, obj.cur.1)
 alike(obj.tpl, obj.cur.2)
} )
