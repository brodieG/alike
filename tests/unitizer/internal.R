# redefine funs to give us flexibility if we change packages without having
# to export the internal functions

library(alike)

unitizer_sect("Name like attributes", {
  alike:::name_compare(c("", "hello"), c("abc", "hello"))
  alike:::name_compare(c("ab", "hello"), c("abc", "hello"))
  alike:::name_compare(c(NA_character_, "hello"), c("abc", "hello"))
  alike:::name_compare(c("ab", "hello"), c(NA_character_, "hello"))
  alike:::name_compare(c(NA_character_, "hello"), c(NA_character_, "hello"))
} )

unitizer_sect("S3 Classes", {
  class1 <- letters[1:5]
  class2 <- letters[3:5]
  class3 <- letters[c(4, 3, 5)]
  class4 <- character()
  class5 <- NULL
  class6 <- list("a", "b", "c")

  # Note third argument is left in for legacy reasons but doesn't actually do
  # anything

  alike:::class_compare(class2, class1, 0);
  alike:::class_compare(class1, class2, 0);
  alike:::class_compare(class1, class1[1:3], 0);
  alike:::class_compare(class3, class2, 0);
  alike:::class_compare(class3, class1, 0);
  alike:::class_compare(class5, class2, 0);  # this should never happen in reality, but use alike check since not char char comparison
  alike:::class_compare(class2, class5, 0);  # idem
  alike:::class_compare(class5, class5, 0);
  alike:::class_compare(class6, class2, 0);
  alike:::class_compare(class2, class6, 0);

  class7 <- c("a", "data.frame")

  alike:::class_compare(class7, class1, 0)
  alike:::class_compare(class1, class7, 0)
})
unitizer_sect("Dimnames", {
  dimn1 <- list(NULL, NULL, NULL)
  dimn2 <- list(a=letters[1:3], b=letters[4:6], c=letters[7:9])
  dimn3 <- list(letters[1:3], b=letters[4:6], c=letters[7:9])
  dimn4 <- list(letters[1:3], B=letters[4:6], C=letters[7:9])
  dimn5 <- list(a=LETTERS[1:3], b=letters[4:6], c=letters[7:9])
  dimn6 <- list(a="", b=letters[4:6], c=letters[7:9])
  dimn7 <- list()
  dimn8 <- list(a=LETTERS[1:3], b=letters[4:6], c=letters[7:9], d=letters[24:26])
  dimn9 <- list(a=1:3, b=letters[4:6], c=letters[7:9])
  dimn10 <- list(a=list("a", "b", "c"), b=letters[4:6], c=letters[7:9])
  dimn11 <- NULL
  dimn12 <- matrix(letters[1:9], nrow=3)
  dimn13 <- `attr<-`(dimn2, "bar", "yowza")
  dimn14 <- `attr<-`(dimn2, "bar", "yowz")

  # baseline cases

  alike:::dimname_compare(dimn3, dimn2)
  alike:::dimname_compare(dimn2, dimn3)
  alike:::dimname_compare(dimn3, dimn4)
  alike:::dimname_compare(dimn2, dimn5)
  alike:::dimname_compare(dimn6, dimn5)
  alike:::dimname_compare(dimn5, dimn6)

  # "empty-ish" cases

  alike:::dimname_compare(dimn2, dimn1)
  alike:::dimname_compare(dimn1, dimn2)
  alike:::dimname_compare(dimn11, dimn2)
  alike:::dimname_compare(dimn11, dimn11)
  alike:::dimname_compare(dimn7, dimn2)
  alike:::dimname_compare(dimn2, dimn7)
  alike:::dimname_compare(dimn7, dimn7)

  # Breaking cases

  alike:::dimname_compare(dimn5, dimn8)
  alike:::dimname_compare(dimn8, dimn5)
  alike:::dimname_compare(dimn9, dimn2)
  alike:::dimname_compare(dimn2, dimn9)
  alike:::dimname_compare(dimn2, dimn12)
  alike:::dimname_compare(dimn12, dimn12)

  # Attr on attr

  alike:::dimname_compare(dimn2, dimn13)
  alike:::dimname_compare(dimn13, dimn2)
  alike:::dimname_compare(dimn13, dimn14)
  alike:::dimname_compare(dimn14, dimn13)
})
unitizer_sect("Dims", {
  dim1 <- rep(2L, 2)
  dim2 <- rep(2L, 3)
  dim3 <- rep(2L, 4)
  dim4 <- c(1L, 1L)
  dim5 <- 2L
  dim6 <- c(1L, 2L, 3L)
  dim7 <- rep(0L, 2)
  dim8 <- c(0L, 0L, 2L)
  dim9 <- NULL
  dim10 <- letters[1:2]
  dim11 <- list(2L, 2L)

  alike:::dim_compare(dim1, dim2)  # fail
  alike:::dim_compare(dim2, dim3)  # fail
  alike:::dim_compare(dim1, dim4)  # fail
  alike:::dim_compare(dim2, dim6)  # fail
  alike:::dim_compare(dim7, dim1)  # works
  alike:::dim_compare(dim7, dim4)  # works
  alike:::dim_compare(dim1, dim7)  # fail
  alike:::dim_compare(dim7, dim2)  # works
  alike:::dim_compare(dim8, dim2)  # works
  alike:::dim_compare(dim8, dim6)  # fail
  alike:::dim_compare(dim6, dim9)  # works

  # With non atomic objects

  alike:::dim_compare(dim1, dim2, list())          # fail
  alike:::dim_compare(dim1, dim2, cur_obj=list())  # fail
  alike:::dim_compare(dim1, dim2, list(), list())  # fail

  # Errors

  alike:::dim_compare(dim9, dim6)  # fail
  alike:::dim_compare(dim10, dim1) # fail
})
unitizer_sect("Time Series", {
  ts.1 <- attr(ts(runif(24), 1970, frequency=12), "ts")
  ts.2 <- attr(ts(runif(24), 1970, frequency=4), "ts")
  ts.3 <- ts.4 <- ts.1
  ts.3[[2L]] <- 0
  ts.4[[3L]] <- 0

  alike:::ts_compare(ts.1, ts.2)
  alike:::ts_compare(ts.3, ts.2)  # zero is wildcard
  alike:::ts_compare(ts.4, ts.2)
  alike:::ts_compare(ts.4, ts.1)  # zero is wildcard
  alike:::ts_compare(ts.1, ts.4)  # but not in reverse

  # non-ts comparisons

  alike:::ts_compare(ts.4, "hello")
  alike:::ts_compare("hello", 1:3)
  alike:::ts_compare(ts.1, 1:3)   # TRUE, because second param is not REAL, kicks off to standard alike comparison
  alike:::ts_compare(ts.4, 1:4)
})
unitizer_sect("All Attributes, default", {
  alike:::attr_compare(1, 1)                                           # TRUE
  alike:::attr_compare(matrix(integer(), 3), matrix(integer(), 3, 3))  # TRUE
  alike:::attr_compare(matrix(integer(), 3), matrix(integer(), 3, 3), "hello")  # Error
  alike:::attr_compare(matrix(integer(), 3), matrix(integer(), 3, 3), 1.1)      # Error
  alike:::attr_compare(matrix(integer(), 4), matrix(integer(), 3, 3))           # Dim 1 error
  alike:::attr_compare(matrix(integer(), ncol=4), matrix(integer(), 3, 3))      # Dim 2 error
  alike:::attr_compare(                                                         # TRUE
    matrix(integer(), 3, 3, dimnames=list(NULL, letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3]))
  )
  alike:::attr_compare(                                                         # dimnames error
    matrix(integer(), 3, 3, dimnames=list(NULL, letters[2:4])),
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3]))
  )
  alike:::attr_compare(                                                         # dimnames error
    matrix(integer(), 3, 3, dimnames=list(letters[1:3], letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3]))
  )
  alike:::attr_compare(                                                         # TRUE
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(a=LETTERS[1:3], b=letters[1:3]))
  )
  alike:::attr_compare(                                                         # dimnames error
    matrix(integer(), 3, 3, dimnames=list(A=LETTERS[1:3], letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(a=LETTERS[1:3], b=letters[1:3]))
  )
  alike:::attr_compare(                                                         # TRUE
    structure(list(integer(), character())),
    data.frame(a=1:10, b=letters[1:10])
  )
  alike:::attr_compare(                                                         # TRUE
    structure(list(integer(), character()), class="data.frame"),
    data.frame(a=1:10, b=letters[1:10])
  )
  alike:::attr_compare(                                                         # TRUE
    structure(unname(data.frame(integer(), character())), class="data.frame"),
    data.frame(a=1:10, b=letters[1:10])
  )
  alike:::attr_compare(                                                         # TRUE, zero length attr
    structure(list(), welp=list()),
    structure(list("hello"), welp=list(NULL, 1:3), belp=1:3)
  )
  alike:::attr_compare(                                                         # Attr length mismatch
    structure(list(), welp=list(NULL)),
    structure(list("hello"), welp=list(NULL, 1:3), belp=1:3)
  )
  alike:::attr_compare(                                                         # Missing attr
    structure(list(), welp=list(), belp=1:3),
    structure(list("hello"), welp=list(NULL, 1:3))
  )
  alike:::attr_compare(                                                         # TRUE
    structure(list(), class=letters[1:3]),
    structure(list("hello"), class=letters[1:3])
  )
  alike:::attr_compare(                                                         # class mismatch
    structure(list(), class=letters[1:3]),
    structure(list("hello"), class=letters[1:4])
  )
  alike:::attr_compare(                                                         # TRUE
    structure(list(), class=letters[2:4]),
    structure(list("hello"), class=letters[1:4])
  )
} )
unitizer_sect("All attributes, strict", {
  alike:::attr_compare(matrix(integer(), 3), matrix(integer(), 3, 3), 1)        # dim mismatch, but passes because comparison is `alike`
  alike:::attr_compare(matrix(integer(), 3, 3), matrix(integer(), 3, 3), 1)     # TRUE
  alike:::attr_compare(                                                         # dimnames mismatch, but alike so passes
    matrix(integer(), 3, 3, dimnames=list(NULL, letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3])),
    attr.mode=1
  )
  alike:::attr_compare(                                                         # dimnames mismatch, but passes because target has NULL names
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(a=LETTERS[1:3], b=letters[1:3])),
    attr.mode=1
  )
  alike:::attr_compare(                                                         # dimnames mismatch, but here fails because target has them but current doesnt
    matrix(integer(), 3, 3, dimnames=list(a=LETTERS[1:3], b=letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3])),
    attr.mode=1
  )
  alike:::attr_compare(                                                         # actually passes because both have 2 length character name attrs, which are alike
    matrix(integer(), 3, 3, dimnames=list(A=LETTERS[1:3], letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(a=LETTERS[1:3], b=letters[1:3])),
    attr.mode=1
  )
  alike:::attr_compare(                                                         # TRUE
    structure(list(integer(), character())),
    data.frame(a=1:10, b=letters[1:10]),
    attr.mode=1
  )
  alike:::attr_compare(                                                         # TRUE
    structure(list(integer(), character()), class="data.frame"),
    data.frame(a=1:10, b=letters[1:10]),
    attr.mode=1
  )
  alike:::attr_compare(                                                         # Class mismatch
    structure(list(), class=letters[2:4]),
    structure(list("hello"), class=letters[1:4]),
    attr.mode=1
  )
  alike:::attr_compare(                                                         # Too many attrs
    structure(list(integer(), character())),
    data.frame(a=1:10, b=letters[1:10]),
    attr.mode=2
  )
  alike:::attr_compare(                                                         # Too many attrs
    structure(list(integer(), character()), class="data.frame"),
    data.frame(a=1:10, b=letters[1:10]),
    attr.mode=2
  )
  alike:::attr_compare(                                                         # Missing attr
    structure(list(), welp=list(NULL, 1:3), belp=1:3),
    structure(list("hello"), welp=list(NULL, 1:3)),
    attr.mode=2
  )
  alike:::attr_compare(                                                         # Missing attr, but attr count same
    structure(list(), welp=list(NULL, 1:3), belp=1:3),
    structure(list("hello"), welp=list(NULL, 1:3), kelp=20),
    attr.mode=2
  )
} )
unitizer_sect("Match Calls", {
  alike:::match_call_alike(quote(var(y=1:10, runif(10))), baseenv())
  env0 <- new.env()
  env0$var <- function(yollo, zambia) NULL
  alike:::match_call_alike(quote(var(y=1:10, runif(10))), env0)
})
unitizer_sect("Calls", {
  c0 <- quote(fun(a, b, a, 25))
  c1 <- quote(fun(x, y, x, "hello"))
  c2 <- quote(fun(x, y, z, "hello"))
  c3 <- quote(FUN(x, y, x, 1.01))
  c4 <- quote(fun(x, y, x, z))
  c5 <- quote(fun(a + b + a, FUN(z, a + 1)))
  c6 <- quote(fun(x + y + x, FUN(w, x + 2)))
  c7 <- quote(fun(x + y + x, FUN(w, y + 2)))
  c8 <- quote(fun(x + y + x, FUN(w, x - 2)))
  c9 <- quote(fun(x + y + x, FUN(w, x + "hello")))

  alike:::lang_alike(c0, c1, NULL)  # TRUE
  alike:::lang_alike(c0, c2, NULL)  # no, inconsistent
  alike:::lang_alike(c0, c3, NULL)  # no, wrong fun name
  alike:::lang_alike(c0, c4, NULL)  # extra symbol
  alike:::lang_alike(c5, c6, NULL)  # TRUE
  alike:::lang_alike(c5, c7, NULL)  # inconsistent
  alike:::lang_alike(c5, c8, NULL)  # wrong call `-`
  alike:::lang_alike(c5, c9, NULL)  # TRUE

  fun <- function(abc, bcd, efg) NULL

  ca <- quote(fun(a, b, a))
  cb <- quote(fun(x, e=x, y))

  alike:::lang_alike(ca, cb, NULL)      # shouldn't match without match.call
  alike:::lang_alike(cb, ca, NULL)      # false, different error
  alike:::lang_alike(ca, cb)            # TRUE, should match

  # test nested match.call

  cc <- quote(fun(a, b, fun(b=1)))
  cd <- quote(fun(a, b, fun(c=1)))

  alike:::lang_alike(cc, cd)

  # NULL in target matches anything

  ce <- quote(fun(a, b, NULL))

  alike:::lang_alike(cc, ce)  # FALSE
  alike:::lang_alike(ce, cc)  # TRUE

  # Formulas

  f0 <- y ~ x + 1
  f1 <- a ~ b + 1
  f2 <- a ~ b + 2
  f3 <- y ~ x + log(x) + z - 1
  f4 <- a ~ b + log(b) + c - 1
  f5 <- a ~ b + log(c) + b - 1
  f6 <- a ~ b + ln(b) + c - 1
  f7 <- a ~ b + log(b) + c + 1

  alike:::lang_alike(f0, f1, NULL)        # TRUE
  alike:::lang_alike(f0, f2, NULL)        # FALSE
  alike:::lang_alike(f3, f4, NULL)        # TRUE
  alike:::lang_alike(f3, f5, NULL)        # FALSE
  alike:::lang_alike(f3, f6, NULL)        # FALSE
  alike:::lang_alike(f3, f7, NULL)        # FALSE
})
unitizer_sect("Closures", {
  alike:::fun_alike(print, print.data.frame)  # TRUE, methods should always match generics
  alike:::fun_alike(print.data.frame, print)  # FALSE, but generics won't match methods with more arguments
  alike:::fun_alike(summary, summary.lm)
  alike:::fun_alike(summary.lm, summary)

  fn0 <- function(x, y) NULL
  fn1 <- function(x, y, z) NULL
  fn2 <- function(y, x) NULL
  fn3 <- function(x=1, y=2) NULL
  fn4 <- function(x, ...) NULL
  fn5 <- function(x) NULL
  fn6 <- function(x, y, z, ...) NULL
  fn7 <- function(x, ..., y) NULL
  fn8 <- function(x, a, ..., g, y) NULL
  fn9 <- function(x, a, ..., g, y, w) NULL

  alike:::fun_alike(fn0, fn1)  # FALSE
  alike:::fun_alike(fn1, fn0)  # FALSE
  alike:::fun_alike(fn4, fn1)  # FALSE, dots must be matched
  alike:::fun_alike(fn0, fn2)  # FALSE
  alike:::fun_alike(fn0, fn3)  # TRUE
  alike:::fun_alike(fn3, fn0)  # FALSE - defaults in target must be specified in current as well
  alike:::fun_alike(fn4, fn5)  # FALSE dots in target must exit in current
  alike:::fun_alike(fn4, fn6)  # TRUE
  alike:::fun_alike(fn4, fn7)  # TRUE
  alike:::fun_alike(fn7, fn4)  # FALSE - all arguments in target must be in current, even with dots
  alike:::fun_alike(fn7, fn8)  # TRUE
  alike:::fun_alike(fn7, fn9)  # FALSE - extra arguments in current must be adjacent to dots

  # Try some builtins / specials

  alike:::fun_alike(`+`, `-`)  # TRUE, builtins
  alike:::fun_alike(substitute, function(expr, env) NULL)  # TRUE, special
  alike:::fun_alike(function(expr, env) NULL, substitute)  # TRUE, special
  alike:::fun_alike(substitute, on.exit)  # FALSE, specials
  alike:::fun_alike(on.exit, substitute)  # FALSE, specials
  alike:::fun_alike(`[`, substitute)      # FALSE, argless specials
  alike:::fun_alike(`[`, `&&`)          # TRUE, argless specials
})
unitizer_sect("Deparse", {
  l0 <- quote(
    a + b + fun(x + funz(
      matrix_over[25, 32]) + transform(iris, x = Sepal.Width * 3) /
      the_donkey_ate_a_carrot %in% {
        paste0(
          match(letter, LETTERS),
          c("hello there")
  ) } ) )
  # simple deparse

  (dep.txt <- alike:::dep_alike(l0))
  alike:::dep_alike(l0, 30)

  # manip the deparse

  alike:::pad(dep.txt)
  alike:::pad(dep.txt, pad=4)
  alike:::pad(dep.txt, pad=4, lines=2)

  # oneline

  alike:::dep_oneline(quote(1 + 1 + 3 + 944254235), 10)
  alike:::dep_oneline(quote(1 + 1 + 3), 10)
  alike:::dep_oneline(quote(1 + 1 + 3), "hello")
})
unitizer_sect("Env Track", {
  el.1 <- replicate(5, new.env())
  el.2 <- el.1[c(1, 1, 2, 3, 4, 1, 2, 3, 5, 1)]
  alike:::env_track(el.1, 1L)  # first env a freebie, so should be 1
  alike:::env_track(el.2, 1L)
} )
unitizer_sect("valid names", {
  alike:::is_valid_name("hello")
  alike:::is_valid_name(".hello")
  alike:::is_valid_name("123")
  alike:::is_valid_name("hello there")
  alike:::is_valid_name("h1ello")
  alike:::is_valid_name("_hello")
  alike:::is_valid_name(".1fail")
  alike:::is_valid_name("NULL")
  alike:::is_valid_name("FALSE")
} )
