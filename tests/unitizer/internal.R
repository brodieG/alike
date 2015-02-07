# redefine funs to give us flexibility if we change packages without having
# to export the internal functions

library(alike)

attr_compare <- alike:::attr_compare
name_compare <- alike:::name_compare
class_compare <- alike:::class_compare
dimname_compare <- alike:::dimname_compare
dim_compare <- alike:::dim_compare

unitizer_sect("Name like attributes", {
  name_compare(c("", "hello"), c("abc", "hello"))
  name_compare(c("ab", "hello"), c("abc", "hello"))
  name_compare(c(NA_character_, "hello"), c("abc", "hello"))
  name_compare(c("ab", "hello"), c(NA_character_, "hello"))
  name_compare(c(NA_character_, "hello"), c(NA_character_, "hello"))
} )

unitizer_sect("S3 Classes", {
  class1 <- letters[1:5]
  class2 <- letters[3:5]
  class3 <- letters[c(4, 3, 5)]
  class4 <- character()
  class5 <- NULL
  class6 <- list("a", "b", "c")

  class_compare(class2, class1, 0);
  class_compare(class1, class2, 0);
  class_compare(class1, class1[1:3], 0);
  class_compare(class3, class2, 0);
  class_compare(class3, class1, 0);
  class_compare(class5, class2, 0);
  class_compare(class2, class5, 0);
  class_compare(class5, class5, 0);
  class_compare(class6, class2, 0);
  class_compare(class2, class6, 0);

  class_compare(class2, class1, 1);
  class_compare(class1, class2, 1);
  class_compare(class1, class1[1:3], 1);
  class_compare(class3, class2, 1);
  class_compare(class3, class1, 1);
  class_compare(class5, class2, 1);
  class_compare(class2, class5, 1);
  class_compare(class5, class5, 1);
  class_compare(class6, class2, 1);
  class_compare(class2, class6, 1);

  class_compare(class2, class1, 2);
  class_compare(class2, class1, -1);
  class_compare(class2, class1, NA_integer_);

  class7 <- c("a", "data.frame")

  class_compare(class7, class1, 0)
  class_compare(class1, class7, 0)
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

  dimname_compare(dimn3, dimn2)
  dimname_compare(dimn2, dimn3)
  dimname_compare(dimn3, dimn4)
  dimname_compare(dimn2, dimn5)
  dimname_compare(dimn6, dimn5)
  dimname_compare(dimn5, dimn6)

  # "empty-ish" cases

  dimname_compare(dimn2, dimn1)
  dimname_compare(dimn1, dimn2)
  dimname_compare(dimn11, dimn2)
  dimname_compare(dimn11, dimn11)
  dimname_compare(dimn7, dimn2)
  dimname_compare(dimn2, dimn7)
  dimname_compare(dimn7, dimn7)

  # Breaking cases

  dimname_compare(dimn5, dimn8)
  dimname_compare(dimn8, dimn5)
  dimname_compare(dimn9, dimn2)
  dimname_compare(dimn2, dimn9)
  dimname_compare(dimn2, dimn12)
  dimname_compare(dimn12, dimn12)

  # Attr on attr

  dimname_compare(dimn2, dimn13)
  dimname_compare(dimn13, dimn2)
  dimname_compare(dimn13, dimn14)
  dimname_compare(dimn14, dimn13)
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

  dim_compare(dim1, dim2)  # fail
  dim_compare(dim2, dim3)  # fail
  dim_compare(dim1, dim4)  # fail
  dim_compare(dim2, dim6)  # fail
  dim_compare(dim7, dim1)  # works
  dim_compare(dim7, dim4)  # works
  dim_compare(dim1, dim7)  # fail
  dim_compare(dim7, dim2)  # works
  dim_compare(dim8, dim2)  # works
  dim_compare(dim8, dim6)  # fail
  dim_compare(dim6, dim9)  # works

  # With non atomic objects

  dim_compare(dim1, dim2, list())          # fail
  dim_compare(dim1, dim2, cur_obj=list())  # fail
  dim_compare(dim1, dim2, list(), list())  # fail

  # Errors

  dim_compare(dim6, dim9, rev=1L)  # fail
  dim_compare(dim9, dim6)  # fail
  dim_compare(dim10, dim1) # fail
})

unitizer_sect("All Attributes, default", {
  attr_compare(1, 1)                                           # TRUE
  attr_compare(matrix(integer(), 3), matrix(integer(), 3, 3))  # TRUE
  attr_compare(matrix(integer(), 3), matrix(integer(), 3, 3), "hello")  # Error
  attr_compare(matrix(integer(), 3), matrix(integer(), 3, 3), 1.1)      # Error
  attr_compare(matrix(integer(), 4), matrix(integer(), 3, 3))           # Dim 1 error
  attr_compare(matrix(integer(), ncol=4), matrix(integer(), 3, 3))      # Dim 2 error
  attr_compare(                                                         # TRUE
    matrix(integer(), 3, 3, dimnames=list(NULL, letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3]))
  )
  attr_compare(                                                         # dimnames error
    matrix(integer(), 3, 3, dimnames=list(NULL, letters[2:4])),
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3]))
  )
  attr_compare(                                                         # dimnames error
    matrix(integer(), 3, 3, dimnames=list(letters[1:3], letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3]))
  )
  attr_compare(                                                         # TRUE
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(a=LETTERS[1:3], b=letters[1:3]))
  )
  attr_compare(                                                         # dimnames error
    matrix(integer(), 3, 3, dimnames=list(A=LETTERS[1:3], letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(a=LETTERS[1:3], b=letters[1:3]))
  )
  attr_compare(                                                         # TRUE
    structure(list(integer(), character())),
    data.frame(a=1:10, b=letters[1:10])
  )
  attr_compare(                                                         # TRUE
    structure(list(integer(), character()), class="data.frame"),
    data.frame(a=1:10, b=letters[1:10])
  )
  attr_compare(                                                         # TRUE
    structure(unname(data.frame(integer(), character())), class="data.frame"),
    data.frame(a=1:10, b=letters[1:10])
  )
  attr_compare(                                                         # TRUE, zero length attr
    structure(list(), welp=list()),
    structure(list("hello"), welp=list(NULL, 1:3), belp=1:3)
  )
  attr_compare(                                                         # Attr length mismatch
    structure(list(), welp=list(NULL)),
    structure(list("hello"), welp=list(NULL, 1:3), belp=1:3)
  )
  attr_compare(                                                         # Missing attr
    structure(list(), welp=list(), belp=1:3),
    structure(list("hello"), welp=list(NULL, 1:3))
  )
  attr_compare(                                                         # TRUE
    structure(list(), class=letters[1:3]),
    structure(list("hello"), class=letters[1:3])
  )
  attr_compare(                                                         # class mismatch
    structure(list(), class=letters[1:3]),
    structure(list("hello"), class=letters[1:4])
  )
  attr_compare(                                                         # TRUE
    structure(list(), class=letters[2:4]),
    structure(list("hello"), class=letters[1:4])
  )
} )
unitizer_sect("All attributes, strict", {
  attr_compare(matrix(integer(), 3), matrix(integer(), 3, 3), 1)        # dim mismatch
  attr_compare(matrix(integer(), 3, 3), matrix(integer(), 3, 3), 1)     # TRUE
  attr_compare(                                                         # dimnames mismatch
    matrix(integer(), 3, 3, dimnames=list(NULL, letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3])),
    attr.mode=1
  )
  attr_compare(                                                         # dimnames mismatch
    matrix(integer(), 3, 3, dimnames=list(LETTERS[1:3], letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(a=LETTERS[1:3], b=letters[1:3])),
    attr.mode=1
  )
  attr_compare(                                                         # dimnames error
    matrix(integer(), 3, 3, dimnames=list(A=LETTERS[1:3], letters[1:3])),
    matrix(integer(), 3, 3, dimnames=list(a=LETTERS[1:3], b=letters[1:3])),
    attr.mode=1
  )
  attr_compare(                                                         # TRUE
    structure(list(integer(), character())),
    data.frame(a=1:10, b=letters[1:10]),
    attr.mode=1
  )
  attr_compare(                                                         # TRUE
    structure(list(integer(), character()), class="data.frame"),
    data.frame(a=1:10, b=letters[1:10]),
    attr.mode=1
  )
  attr_compare(                                                         # Class mismatch
    structure(list(), class=letters[2:4]),
    structure(list("hello"), class=letters[1:4]),
    attr.mode=1
  )
  attr_compare(                                                         # Too many attrs
    structure(list(integer(), character())),
    data.frame(a=1:10, b=letters[1:10]),
    attr.mode=2
  )
  attr_compare(                                                         # Too many attrs
    structure(list(integer(), character()), class="data.frame"),
    data.frame(a=1:10, b=letters[1:10]),
    attr.mode=2
  )
  attr_compare(                                                         # Missing attr
    structure(list(), welp=list(NULL, 1:3), belp=1:3),
    structure(list("hello"), welp=list(NULL, 1:3)),
    attr.mode=2
  )
  attr_compare(                                                         # Missing attr, but attr count same
    structure(list(), welp=list(NULL, 1:3), belp=1:3),
    structure(list("hello"), welp=list(NULL, 1:3), kelp=20),
    attr.mode=2
  )
} )
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

  alike:::lang_alike(c0, c1)  # TRUE
  alike:::lang_alike(c0, c2)  # no, inconsistent
  alike:::lang_alike(c0, c3)  # no, wrong fun name
  alike:::lang_alike(c0, c4)  # extra symbol
  alike:::lang_alike(c5, c6)  # TRUE
  alike:::lang_alike(c5, c7)  # inconsistent
  alike:::lang_alike(c5, c8)  # wrong call `-`
  alike:::lang_alike(c5, c9)  # TRUE

  # Attributes on sub-components should not affect anything
  # actually, these tests need to be with alike since lang_alike doesn't check
  # attributes

  # c0.1 <- c0.2 <- c0.3 <- c0
  # attr(c0.1, "blah") <- "hello"
  # attr(c0.2, "blah") <- 1:3
  # attr(c0.3[[1L]], "blah") <- "hello"

  # alike:::lang_alike(c0, c0.1)     # TRUE
  # alike:::lang_alike(c0.1, c0)     # Missing attribute
  # alike:::lang_alike(c0.1, c0.2)   # Attribute mismatch
  # alike:::lang_alike(c0.3, c0)     # TRUE, sub-attr shouldn't cause problem

  f0 <- y ~ x + 1
  f1 <- a ~ b + 1
  f2 <- a ~ b + 2
  f3 <- y ~ x + log(x) + z - 1
  f4 <- a ~ b + log(b) + c - 1
  f5 <- a ~ b + log(c) + b - 1
  f6 <- a ~ b + ln(b) + c - 1
  f7 <- a ~ b + log(b) + c + 1

  alike:::lang_alike(f0, f1)        # TRUE
  alike:::lang_alike(f0, f2)        # FALSE
  alike:::lang_alike(f3, f4)        # TRUE
  alike:::lang_alike(f3, f5)        # FALSE
  alike:::lang_alike(f3, f6)        # FALSE
  alike:::lang_alike(f3, f7)        # FALSE
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
  alike:::dep_alike(l0, -1)
  alike:::dep_alike(l0, 1)
  alike:::dep_alike(l0, 2)
})

