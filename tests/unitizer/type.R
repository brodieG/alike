library(alike)

# Redefine names; necessary because we renamed functions when moving to C
# versions

.typeof2 <- .type_of
typeof2 <- type_of
.type_alike2 <- .type_alike
type_alike2 <- type_alike

unitizer_sect(".typeof", {
  .Machine$double.eps    # just to record precision these tests were run in
  a <- b <- 1:100
  a[[1]] <- a[[1]] + .0000001
  b[[1]] <- b[[1]] + .00000001

  .typeof2(1 + .Machine$double.eps ^ .5)          # integer, this is the threshold
  .typeof2(1 + .Machine$double.eps ^ .5 * 1.0001) # double
  .typeof2(1:100)
  .typeof2(1.1)
  .typeof2(1.0000001)           # double
  .typeof2(a)                   # double
  .typeof2(1.00000001)          # integer
  .typeof2(b)                   # integer
  .typeof2(1e6 + .1)            # double
  .typeof2(1e7 + .1)            # integer
  .typeof2(NA_real_)            # integer
  .typeof2(Inf)                 # integer
  .typeof2(-Inf)                # integer
  .typeof2(list())
  .typeof2(data.frame(a=1:10))
  .typeof2(matrix("", 10, 10))
} )
unitizer_sect("typeof", {
  typeof2(1:100)
  typeof2(1.1)
  typeof2(1.0000001)           # double
  typeof2(a)                   # double
  typeof2(1.00000001)          # integer
  typeof2(b)                   # integer
  typeof2(1e6 + .1)            # double
  typeof2(1e7 + .1)            # integer
  typeof2(NA_real_)            # integer
  typeof2(Inf)                 # integer
  typeof2(-Inf)                # integer
  typeof2(1e7 + .1, tolerance = .Machine$double.eps ^ .5 / 10) # double
  typeof2(1e7 + .1, tolerance=1L)       # error, might change in future
  typeof2(1e7 + .1, tolerance="hello")  # error
} )
unitizer_sect(".type_alike", {
  .type_alike2(1, 1.1)
  .type_alike2(1, 1.00000001)
  .type_alike2(list(), integer())
  .type_alike2(a, 1:100)        # TRUE, integer can be considered numeric
  .type_alike2(1:100, a)        # FALSE
  .type_alike2(1:100, b)        # TRUE
  .type_alike2(1000000L, 1000000L + .1)     # FALSE
  .type_alike2(10000000L, 10000000L + .1)   # TRUE
  .type_alike2(data.frame(a=1:10), list())
  .type_alike2(NULL, NULL)
  .type_alike2(1/0, NA)
} )
unitizer_sect("type_alike", {
  type_alike2(1, 1.1)          # TRUE, 1 is numeric
  type_alike2(1L, 1.1)         # FALSE
  type_alike2(1L, 1.00000001)  # TRUE
  type_alike2(numeric(), c(1.1, 0.053, 41.8))  # TRUE
  type_alike2(numeric(), list(1.1))  # FALSE
  type_alike2(list(), integer())     # FALSE
  type_alike2(a, 1:100)        # TRUE, integer can be considered numeric
  type_alike2(1:100, a)        # FALSE
  type_alike2(1:100, b)        # TRUE
  type_alike2(1000000L, 1000000L + .1)     # FALSE
  type_alike2(10000000L, 10000000L + .1)   # TRUE
  type_alike2(data.frame(a=1:10), list())
  type_alike2(NULL, NULL)
  type_alike2(1/0, NA)

  type_alike2(1:100, a, tolerance=.Machine$double.eps ^ .5 * 10)  # TRUE
  type_alike2(a, 1:100, mode=1.0)      # TRUE
  type_alike2(a, 1:100, mode=1L)       # TRUE
  type_alike2(1:100, b, mode=1L)       # FALSE
  type_alike2(a, 1:100, mode=2L)       # FALSE

  type_alike2(1:100, a, tolerance=1L)       # error
  type_alike2(1:100, a, tolerance=1:2)      # error
  type_alike2(1:100, a, tolerance="hello")  # error
  type_alike2(1:100, a, mode=1.1)           # error
  type_alike2(1:100, a, mode=1:2)           # error
} )
