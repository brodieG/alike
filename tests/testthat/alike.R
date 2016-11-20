library(alike)

test_that("Atomic", {
  expect_equal(alike(integer(), 1:3), list(1, 2, 3))    # TRUE
  expect_equal(alike(integer(5L), 1:3), list(1, 2, 3))  # FALSE
 expect_equal( alike(integer(3L), 1:3), list(1, 2, 3))  # TRUE
 expect_equal( alike(numeric(), c(1, 2, 3)), list(1, 2, 3))         # TRUE
 expect_equal( alike(numeric(), 1L), list(1, 2, 3))                 # TRUE
 expect_equal( alike(numeric(), c(1.1,.053,41.8)), list(1, 2, 3))   # TRUE
 expect_equal( alike(integer(3L), 1:3 + .01), list(1, 2, 3))
 expect_equal( alike(integer(6L), seq(1/6, 1, 1/6) * 6), list(1, 2, 3))     # FALSE, not true integers
 expect_equal( alike(integer(4L), letters[1:4]), list(1, 2, 3))
 expect_equal( alike(letters[1:4], c("hello", "goodbye", "ba", "da")), list(1, 2, 3))  # TRUE

 expect_equal( alike(c(a=1, b=2), 3), list(1, 2, 3))         # Length mismatch
 expect_equal( alike(c(a=1, b=2), c(1, 2)), list(1, 2, 3))   # Names
} )
test_that("lists", {
  lst <-   list(list( 1,  2), list( 3, list( 4, list( 5, list(6, 6.1, 6.2)))))
  lst.2 <- list(list(11, 21), list(31, list(41, list(51, list(61          )))))

 expect_equal( alike(lst, lst.2), list(1, 2, 3))     # length mismatch

  lst.3 <- lst.2
  lst.3[[2]][[2]][[2]][[2]] <- matrix(1:9, nrow=3)

 expect_equal( alike(lst, lst.3), list(1, 2, 3))     # object type mismatch
 expect_equal( alike(1:10, "hello"), list(1, 2, 3))  # object type mismatch, no dive

 expect_equal( alike(lst, lst), list(1, 2, 3))       # obvious match

  lst.4 <- lst
  lst.4[[2]][[2]] <- list()

 expect_equal( alike(lst.4, lst), list(1, 2, 3))     # should match
 expect_equal( alike(lst, lst.4), list(1, 2, 3))     # should not match because template has more detail

  # Named lists

  lst.5 <- list(1, list(a = 1, b = 2, c = list(d = 1)))
  lst.6 <- list(1, list(a = 1, b = 2, c = list(d = "hello")))
  lst.5.1 <- list(1, list(a = 1, b = 2, `c d` = list(d = 1)))
  lst.6.1 <- list(1, list(a = 1, b = 2, `c d` = list(d = "hello")))

 expect_equal( alike(lst.5, lst.6), list(1, 2, 3))
 expect_equal( alike(lst.6, lst.5), list(1, 2, 3))

 expect_equal( alike(lst.5.1, lst.6.1), list(1, 2, 3))
 expect_equal( alike(lst.6.1, lst.5.1), list(1, 2, 3))

})
test_that("NULL values as wildcards", {
 expect_equal( alike(NULL, 1:3), list(1, 2, 3))                  # not a wild card at top level
 expect_equal( alike(list(NULL), list(1:3)), list(1, 2, 3))      # but yes when nested
 expect_equal( alike(list(NULL, NULL), list(list(list(1, 2, 3)), 1:25)), list(1, 2, 3))
 expect_equal( alike(list(NULL), list(1, 2)), list(1, 2, 3))
 expect_equal( alike(list(), list(1, 2)), list(1, 2, 3))
})
test_that("Matrix / Arrays", {
 expect_equal( alike(matrix(integer(), ncol=7), matrix(1:21, nrow=3)), list(1, 2, 3))
 expect_equal( alike(matrix(integer(), nrow=3), matrix(1:21, nrow=3)), list(1, 2, 3))
 expect_equal( alike(matrix(character(), nrow=3), matrix(1:21, nrow=3)), list(1, 2, 3))
 expect_equal( alike(matrix(integer(), nrow=4), matrix(1:21, nrow=3)), list(1, 2, 3))
 expect_equal( alike(matrix(integer(), ncol=3, dimnames=list(NULL, c("R", "G", "B"))), matrix(1:21, ncol=3, dimnames=list(NULL, c("R", "G", "B")))), list(1, 2, 3))
 expect_equal( alike(matrix(integer(), nrow=3, dimnames=list(c("R", "G", "B"), NULL)), matrix(1:21, ncol=3, dimnames=list(NULL, c("R", "G", "B")))), list(1, 2, 3))
 expect_equal( alike(matrix(integer(), nrow=3, dimnames=list(c("R", "G", "B"), NULL)), matrix(1:9, nrow=3, dimnames=list(NULL, c("R", "G", "B")))), list(1, 2, 3))
 expect_equal( alike(matrix(integer(), nrow=3, dimnames=list(c("R", "G", "B"), NULL)), matrix(1:9, nrow=3, dimnames=list(c("R", "G", "B"), c("bacon", "turkey", "bravo")))), list(1, 2, 3))

 expect_equal( alike(matrix(1:9, nrow = 3), 1:9), list(1, 2, 3))

  # Adding tests from docs

  mx.tpl <- matrix(integer(), ncol=3, dimnames=list(row.id=NULL, c("R", "G", "B")))
  mx.cur <- matrix(sample(0:255, 12), ncol=3, dimnames=list(row.id=1:4, rgb=c("R", "G", "B")))
  mx.cur2 <- matrix(sample(0:255, 12), ncol=3, dimnames=list(1:4, c("R", "G", "B")))

 expect_equal( alike(mx.tpl, mx.cur), list(1, 2, 3))
 expect_equal( alike(mx.tpl, mx.cur2), list(1, 2, 3))
} )
test_that("Data Frames", {
 expect_equal( alike(mtcars, 1:3), list(1, 2, 3))
 expect_equal( alike(1:3, mtcars), list(1, 2, 3))
 expect_equal( alike(data.frame(), data.frame(a=1:3, b=letters[1:3])), list(1, 2, 3))  # TRUE
 expect_equal( alike(data.frame(a=integer(), b=factor()), data.frame(a=1:3, b=letters[1:3])), list(1, 2, 3))        # TRUE, note this is recursive
 expect_equal( alike(data.frame(a=factor(), b=factor()), data.frame(a=1:3, b=letters[1:3])), list(1, 2, 3))         # FALSE mis-match at index[[1]]
 expect_equal( alike(list(NULL, structure("a", class="x")), list(NULL, structure("a", class="y"))),  list(1, 2, 3))

  # TRUE, more complex nested structure

  alike(
    list(integer(), data.frame(a=integer(), b=numeric()), matrix(integer(), nrow=3)),
    list(1:10, data.frame(a=1:200, b=runif(20)), matrix(1:27, nrow=3))  # row.names / names (note use `structure` to get around `data.frame` checks)
  )

  df.tpl <- structure(list(1:4, factor(LETTERS[1:4], levels=LETTERS)), row.names=c("one", "", "", ""), names=c("id", ""), class="data.frame")
  df.cur <- `row.names<-`(data.frame(id=5:8, val=factor(LETTERS[21:24], levels=LETTERS)), c("one", "two", "tre", "qtr"))
  df.cur2 <- `row.names<-`(data.frame(id=5:8, val=factor(LETTERS[21:24], levels=LETTERS)), c("uno", "due", "tre", "qtr"))

 expect_equal( alike(df.tpl, df.cur), list(1, 2, 3))   # TRUE
 expect_equal( alike(df.cur, df.tpl), list(1, 2, 3))   # Nope, names won't match reversed
 expect_equal( alike(df.tpl, df.cur2), list(1, 2, 3))  # Nope, row.names won't match

  # NA names

  df.tpl <- structure(list(1:4, letters[1:4]), names=c("id", NA), class="data.frame")
  df.cur <- structure(list(1:4, letters[1:4]), names=c("id", "val"), class="data.frame")

 expect_equal( alike(df.tpl, df.tpl), list(1, 2, 3))
 expect_equal( alike(df.tpl, df.cur), list(1, 2, 3))

  # special treatment

 expect_equal( alike(mtcars, iris), list(1, 2, 3))
 expect_equal( alike(mtcars, mtcars[1:10,]), list(1, 2, 3))
 expect_equal( alike(mtcars[-5], mtcars), list(1, 2, 3))
})
test_that("Time Series", {
  ts.1 <- ts(runif(24), 1970, frequency=12)
  ts.2 <- ts(runif(24), 1970, frequency=4)
  ts.3 <- abstract(ts.1, "end")
  ts.4 <- abstract(ts.2, "frequency")

 expect_equal( alike(ts.1, ts.2), list(1, 2, 3))
 expect_equal( alike(ts.3, ts.1), list(1, 2, 3))
 expect_equal( alike(ts.1, ts.3), list(1, 2, 3))
 expect_equal( alike(ts.3, ts.2), list(1, 2, 3))

  ts.5 <- ts(matrix(runif(24 * 3), ncol=3), 1970, frequency=12)
  ts.6 <- ts(matrix(runif(12 * 3), ncol=3), 1970, frequency=12)

 expect_equal( alike(ts.5, ts.6), list(1, 2, 3))
 expect_equal( alike(ts.5, matrix(runif(24 * 3), ncol=3)), list(1, 2, 3))

})
test_that("Factors", {
  f1 <- factor(letters[1:5])
  f2 <- factor(letters[1:5], levels=letters[5:1])
  f3 <- f1
  levels(f3)[[5]] <- ""
  f4 <- factor(c(letters[1:4], "f"))

 expect_equal( alike(f1, f2), list(1, 2, 3))   # FALSE
 expect_equal( alike(f1, f3), list(1, 2, 3))   # FALSE
 expect_equal( alike(f1, f4), list(1, 2, 3))   # FALSE
 expect_equal( alike(f3, f1), list(1, 2, 3))   # TRUE, wildcard matches anything
 expect_equal( alike(f3, f4), list(1, 2, 3))   # TRUE, wildcard matches anything
})
test_that("Environments / Pairlists", {
  env0 <- new.env()
  env1 <- list2env(list(a=character(), b=list(), c=NULL))
  env2 <- list2env(list(a="hello", b=iris, c=matrix(1:3)))
  env3 <- list2env(list(a="hello", b=iris))
  env4 <- list2env(list(a="hello", b=iris, c=logical(1L), d=logical(1L)))
  env5 <- list2env(list(b=iris, a="hello", c=matrix(1:3)))

 expect_equal( alike(env0, env2), list(1, 2, 3))  # zero length, matches anything
 expect_equal( alike(env1, env2), list(1, 2, 3))  # TRUE
 expect_equal( alike(env1, env3), list(1, 2, 3))  # length mismatch
 expect_equal( alike(env3, env1), list(1, 2, 3))  # component mismatch
 expect_equal( alike(env1, env4), list(1, 2, 3))  # TRUE length mismatch but longer allowed
 expect_equal( alike(env1, env5), list(1, 2, 3))  # order change, should still match

  # Test infinite recursion protection

  rec.env <- rec.env.cpy <- new.env()

  for(i in 1:50) {
    rec.env.cpy$a <- new.env()
    rec.env.cpy <- rec.env.cpy$a
  }
  rec.env.cpy$a <- rec.env;
 expect_equal( alike(rec.env, rec.env), list(1, 2, 3))

  plst1 <- pairlist(a=character(), b=list(), c=NULL)
  plst2 <- pairlist(a="hello", b=iris, c=matrix(1:3))
  plst3 <- pairlist(a="hello", b=iris)
  plst4 <- pairlist(a="hello", b=iris, c=logical(1L), d=logical(1L))
  plst5 <- pairlist(a=character(), b=list(), integer())
  plst6 <- pairlist(a=character(), b=list(), boogey=1:3)
  plst7 <- pairlist(a=character(), boogey=1:3, b=list())

 expect_equal( alike(plst1, plst2), list(1, 2, 3))  # TRUE
 expect_equal( alike(plst1, plst3), list(1, 2, 3))  # length mismatch
 expect_equal( alike(plst1, plst4), list(1, 2, 3))  # length mismatch
 expect_equal( alike(plst1, plst5), list(1, 2, 3))  # fail, missing name
 expect_equal( alike(plst5, plst6), list(1, 2, 3))  # TRUE, no name matches anything
 expect_equal( alike(plst5, plst7), list(1, 2, 3))  # FALSE, order matters in pair lists

  # Nesting

  env7 <- list2env(list(a=character(), b=plst1))
  env8 <- list2env(list(a=letters[1:3], b=plst2))
  env9 <- list2env(list(a=letters[1:3], b=plst5))

 expect_equal( alike(env7, env8), list(1, 2, 3))   # pass
 expect_equal( alike(env7, env9), list(1, 2, 3))   # fail
})
test_that("Calls / Formulas", {
 expect_equal( alike(quote(1 + 1), quote(x + y)), list(1, 2, 3))
 expect_equal( alike(quote(fun(1 + 1)), quote(fun(x + y, 9))), list(1, 2, 3))
 expect_equal( alike(quote(fun(x + y, 9)), quote(fun(1 + 1))), list(1, 2, 3))

  fun <- function(a, b, c) NULL
 expect_equal( alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(NULL, fun2(a, b), 1))), list(1, 2, 3)) # TRUE, since constants including NULL match any constants
  .alike(  # FALSE, match.call disabled
    quote(fun(b=fun2(x, y), 1, 3)), quote(fun(NULL, fun2(a, b), 1)),
    alike_settings(lang.mode=1)
  )
 expect_equal( alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(fun2(a, b), NULL, 1))), list(1, 2, 3)) # FALSE, mismatch
 expect_equal( alike(quote(fun(a=1)), quote(fun(b=1))), list(1, 2, 3))  # FALSE, name mismatch

 expect_equal( alike(quote(fun(1, 2)), quote(fun(1))), list(1, 2, 3))   # FALSE
 expect_equal( alike(quote(fun(1)), quote(fun(1, 2))), list(1, 2, 3))   # FALSE

 expect_equal( alike(quote(fun(1, 2)), quote(fun2(1, 2))), list(1, 2, 3))            # FALSE, fun mismatch
 expect_equal( alike(quote(fun(1, fun2(3))), quote(fun(1, fun(3)))), list(1, 2, 3))  # FALSE, fun mismatch, nested

  # zero len matches anything

 expect_equal( alike(quote(fun()), quote(fun(a, b, c))), list(1, 2, 3))    # TRUE
 expect_equal( alike(quote(fun()), quote(fun2(a, b, c))), list(1, 2, 3))   # FALSE, still need match fun names
 expect_equal( alike(quote(fun(a, fun2())), quote(fun(b, fun2(a, b, c)))), list(1, 2, 3))    # TRUE

  # Attributes on sub-components should not affect anything
  # actually, these tests need to be with alike since lang_alike doesn't check
  # attributes

  c0 <- quote(fun(a, b, a, 25))
  c0.1 <- c0.2 <- c0.3 <- c0
  attr(c0.1, "blah") <- "hello"
  attr(c0.2, "blah") <- 1:3
  attr(c0.3[[1L]], "blah") <- "hello"

 expect_equal( alike(c0, c0.1), list(1, 2, 3))     # TRUE
 expect_equal( alike(c0.1, c0), list(1, 2, 3))     # Missing attribute
 expect_equal( alike(c0.1, c0.2), list(1, 2, 3))   # Attribute mismatch
 expect_equal( alike(c0.3, c0), list(1, 2, 3))     # TRUE, sub-attr shouldn't cause problem

  # Formulas

 expect_equal( alike(x ~ y, z ~ w), list(1, 2, 3))
 expect_equal( alike(x ~ y, z ~ w + 1), list(1, 2, 3))
 expect_equal( alike(x ~ y + 2, z ~ w + 1), list(1, 2, 3))
 expect_equal( alike(x ~ y + z:y, w ~ v + u:v), list(1, 2, 3))
 expect_equal( alike(z ~ w + 1, x ~ y), list(1, 2, 3))
 expect_equal( alike(y ~ x ^ 2 + x * z + z + w:z, q ~ l ^ 2 + l * j + j + w:j), list(1, 2, 3))
 expect_equal( alike(y ~ x ^ 2 + x * z + z + w:z, q ~ l ^ 3 + l * j + j + w:j), list(1, 2, 3))

  # # Repeating parses to deal with potential parse issues in clean R runs

  exp.1 <- parse(text="x + y; fun2(fun(1, 2, 3), z)", keep.source=TRUE)
  exp.2 <- parse(text="z + 2; fun(fun2(1, 2, 3), q)", keep.source=TRUE)
  exp.3 <- parse(text="z + fun(3); fun(fun2(a, b, c), 3)", keep.source=TRUE)

 expect_equal( alike(exp.1, exp.2), list(1, 2, 3))
 expect_equal( alike(exp.2, exp.3), list(1, 2, 3))
 expect_equal( alike(exp.3, exp.2), list(1, 2, 3))

  exp.4 <- expression(1 + 1, 2 + x)
  exp.5 <- expression(1 + 1, 5 + y)
  exp.6 <- expression(1 + 1, 2 + 2)

 expect_equal( alike(exp.4, exp.5), list(1, 2, 3)) # TRUE
 expect_equal( alike(exp.4, exp.6), list(1, 2, 3)) # FALSE

  # Symbols

 expect_equal( alike(quote(x), quote(y)), list(1, 2, 3))      # TRUE
 expect_equal( alike(NULL, quote(x)), list(1, 2, 3))          # FALSE, overridden by type comparison
 expect_equal( alike(quote((NULL)), quote(y)), list(1, 2, 3)) # TRUE, NULL matches anything as language object
 expect_equal( alike(quote(NULL), quote(x)), list(1, 2, 3))   # FALSE, quoting NULL doesn't make it language
 expect_equal( alike(quote(x), c0), list(1, 2, 3))          # FALSE
 expect_equal( alike(c0, quote(x)), list(1, 2, 3))          # FALSE
 expect_equal( alike(quote((x)), quote(y)), list(1, 2, 3))    # TRUE, parens shouldn't matter
} )
# Most fun tests in internal/type, here to make sure interface working
test_that("Functions", {
 expect_equal( alike(print, print.data.frame), list(1, 2, 3))              # TRUE
 expect_equal( alike(print.data.frame, print), list(1, 2, 3))              # FALSE
 expect_equal( alike(`&&`, function() NULL), list(1, 2, 3))                # TRUE
})
# Subset of tests for version with settings

test_that(".alike", {
  expect_equal(.alike(1L, 1.0, alike_settings(type.mode=1L)), list(1, 2, 3))
  expect_equal(.alike(1.0, 1L, alike_settings(type.mode=1L)), list(1, 2, 3))
  expect_equal(.alike(1.0, 1L, alike_settings(type.mode=2L)), list(1, 2, 3))   # FALSE
  expect_equal(.alike(1:101, 1:101 + 0.0), list(1, 2, 3))  # FALSE
  expect_equal(.alike(1:101, 1:101 + 0.0, alike_settings(fuzzy.int.max.len=200)), list(1, 2, 3)) # TRUE
  expect_equal(.alike(1:101, 1:101 + 0.0, alike_settings(fuzzy.int.max.len=-1)), list(1, 2, 3))  # TRUE
  expect_equal(.alike(list(a=1:10), data.frame(a=1:10)), list(1, 2, 3))
  expect_equal(.alike(list(a=1:10), data.frame(a=1:10), alike_settings(attr.mode=1L)), list(1, 2, 3))
  expect_equal(.alike(list(a=1:10), data.frame(a=1:10), alike_settings(attr.mode=2L)), list(1, 2, 3))  # FALSE
  fun <- function(a, b, c) NULL
  expect_equal(.alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(NULL, fun2(a, b), 1)), alike_settings(env=NULL)), list(1, 2, 3))   # FALSE
  expect_equal(.alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(NULL, fun2(a, b), 1))), list(1, 2, 3))                             # TRUE
  expect_equal(.alike(`&&`, function() NULL, alike_settings(type.mode=1)), list(1, 2, 3))   # FALSE
} )
# These are also part of the examples, but here as well so that issues are
# detected during development and not the last minute package checks

test_that("Examples", {
 expect_equal( alike(1L, 1.0), list(1, 2, 3))         # TRUE, because 1.0 is integer-like
 expect_equal( alike(1L, 1.1), list(1, 2, 3))         # FALSE, 1.1 is not integer-like
 expect_equal( alike(1.1, 1L), list(1, 2, 3))         # TRUE, by default, integers are always considered real

 expect_equal( alike(1:100, 1:100 + 0.0), list(1, 2, 3))  # TRUE
 expect_equal( alike(1:101, 1:101 + 0.0), list(1, 2, 3))  # FALSE, we do not check numerics for integerness if longer than 100

  # Scalarness can now be checked at same time as type

 expect_equal( alike(integer(1L), 1), list(1, 2, 3))            # integer-like and length 1?
 expect_equal( alike(logical(1L), TRUE), list(1, 2, 3))         # logical and length 1?
 expect_equal( alike(integer(1L), 1:3), list(1, 2, 3))
 expect_equal( alike(logical(1L), c(TRUE, TRUE)), list(1, 2, 3))

  # Zero length match any length of same type

 expect_equal( alike(integer(), 1:10), list(1, 2, 3))
 expect_equal( alike(1:10, integer()), list(1, 2, 3))   # but not the other way around

  # Recursive objects compared recursively

  alike(
    list(integer(), list(character(), logical(1L))),
    list(1:10, list(letters, TRUE))
  )
  alike(
    list(integer(), list(character(), logical(1L))),
    list(1:10, list(letters, c(TRUE, FALSE)))
  )
  # `NULL` is a wild card when nested within recursive objects

 expect_equal( alike(list(NULL, NULL), list(iris, mtcars)), list(1, 2, 3))
 expect_equal( alike(NULL, mtcars), list(1, 2, 3))    # but not at top level

  # Since `data.frame` are lists, we can compare them recursively:

  iris.fake <- transform(iris, Species=as.character(Species))
 expect_equal( alike(iris, iris.fake), list(1, 2, 3))
  iris.fake2 <- transform(iris, Species=factor(Species, levels=`[[<-`(levels(Species), 3, "americana")))
 expect_equal( alike(iris, iris.fake2)  , list(1, 2, 3))

  # We can use partially specified objects as templates

  iris.tpl <- abstract(iris)
  str(iris.tpl)
 expect_equal( alike(iris.tpl, iris), list(1, 2, 3))
 expect_equal( alike(iris.tpl, iris[sample(1:nrow(iris), 10), ]), list(1, 2, 3))    # any row sample of iris matches our iris template
 expect_equal( alike(iris.tpl, iris[c(2, 1, 3, 4, 5)]), list(1, 2, 3))              # but column order matters

  # Also works with matrices / arrays

 expect_equal( alike(matrix(integer(), 3, 3), matrix(1:9, nrow=3)), list(1, 2, 3))         # 3 x 3 integer
 expect_equal( alike(matrix(integer(), 3, 3), matrix(runif(9), nrow=3)), list(1, 2, 3))    # 3 x 3, but not integer!
 expect_equal( alike(matrix(integer(), 3), matrix(1:12, nrow=3)), list(1, 2, 3))           # partial spec, any 3 row integer matrix
 expect_equal( alike(matrix(integer(), 3), matrix(1:12, nrow=4)), list(1, 2, 3))
 expect_equal( alike(matrix(logical()), array(rep(TRUE, 8), rep(2, 3)))    , list(1, 2, 3))

  # In order for objects to be alike, they must share a family tree, not just
  # a common class

  obj.tpl <- structure(TRUE, class=letters[1:3])
  obj.cur.1 <-  structure(TRUE, class=c("x", letters[1:3]))
  obj.cur.2 <-  structure(TRUE, class=c(letters[1:3], "x"))

 expect_equal( alike(obj.tpl, obj.cur.1), list(1, 2, 3))
 expect_equal( alike(obj.tpl, obj.cur.2), list(1, 2, 3))

  # You can compare language objects; these are alike if they are self
  # consistent; we don't care what the symbols are, so long as they are used
  # consistently across target and current:

 expect_equal( alike(quote(x + y), quote(a + b))   , list(1, 2, 3))
 expect_equal( 
   alike(quote(x + y), quote(a - b)), list(1, 2, 3)
 )   # FALSE, inconsistent symbols
} )
