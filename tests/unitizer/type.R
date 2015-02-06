library(alike)

# Redefine names; necessary because we renamed functions when moving to C
# versions

unitizer_sect(".type_of", {
  .Machine$double.eps    # just to record precision these tests were run in
  a <- b <- 1:100
  a[[1]] <- a[[1]] + .0000001
  b[[1]] <- b[[1]] + .00000001

  .type_of(1 + .Machine$double.eps ^ .5)          # integer, this is the threshold
  .type_of(1 + .Machine$double.eps ^ .5 * 1.0001) # double
  .type_of(1:100)
  .type_of(1.1)
  .type_of(1.0000001)           # double
  .type_of(a)                   # double
  .type_of(1.00000001)          # integer
  .type_of(b)                   # integer
  .type_of(1e6 + .1)            # double
  .type_of(1e7 + .1)            # integer
  .type_of(NA_real_)            # integer
  .type_of(Inf)                 # integer
  .type_of(-Inf)                # integer
  .type_of(list())
  .type_of(data.frame(a=1:10))
  .type_of(matrix("", 10, 10))
} )
unitizer_sect("type_of", {
  type_of(1:100)
  type_of(1.1)
  type_of(1.0000001)           # double
  type_of(a)                   # double
  type_of(1.00000001)          # integer
  type_of(b)                   # integer
  type_of(1e6 + .1)            # double
  type_of(1e7 + .1)            # integer
  type_of(NA_real_)            # integer
  type_of(Inf)                 # integer
  type_of(-Inf)                # integer
  type_of(1e7 + .1, tolerance = .Machine$double.eps ^ .5 / 10) # double
  type_of(1e7 + .1, tolerance=1L)       # error, might change in future
  type_of(1e7 + .1, tolerance="hello")  # error
} )
unitizer_sect(".type_alike", {
  .type_alike(1, 1.1)
  .type_alike(1, 1.00000001)
  .type_alike(list(), integer())
  .type_alike(a, 1:100)        # TRUE, integer can be considered numeric
  .type_alike(1:100, a)        # FALSE
  .type_alike(1:100, b)        # TRUE
  .type_alike(1000000L, 1000000L + .1)     # FALSE
  .type_alike(10000000L, 10000000L + .1)   # TRUE
  .type_alike(data.frame(a=1:10), list())
  .type_alike(NULL, NULL)
  .type_alike(1/0, NA)
} )
unitizer_sect("type_alike", {
  type_alike(1, 1.1)          # TRUE, 1 is numeric
  type_alike(1L, 1.1)         # FALSE
  type_alike(1L, 1.00000001)  # TRUE
  type_alike(numeric(), c(1.1, 0.053, 41.8))  # TRUE
  type_alike(numeric(), list(1.1))  # FALSE
  type_alike(list(), integer())     # FALSE
  type_alike(a, 1:100)        # TRUE, integer can be considered numeric
  type_alike(1:100, a)        # FALSE
  type_alike(1:100, b)        # TRUE
  type_alike(1000000L, 1000000L + .1)     # FALSE
  type_alike(10000000L, 10000000L + .1)   # TRUE
  type_alike(data.frame(a=1:10), list())
  type_alike(NULL, NULL)
  type_alike(1/0, NA)

  type_alike(1:100, a, tolerance=.Machine$double.eps ^ .5 * 10)  # TRUE
  type_alike(a, 1:100, mode=1.0)      # TRUE
  type_alike(a, 1:100, mode=1L)       # TRUE
  type_alike(1:100, b, mode=1L)       # FALSE
  type_alike(a, 1:100, mode=2L)       # FALSE

  type_alike(1:100, a, tolerance=1L)       # error
  type_alike(1:100, a, tolerance=1:2)      # error
  type_alike(1:100, a, tolerance="hello")  # error
  type_alike(1:100, a, mode=1.1)           # error
  type_alike(1:100, a, mode=1:2)           # error
} )
unitizer_sect("functions", {
  type_alike(sd, var)     # clo-clo
  type_alike(`&&`, sd)    # spe-clo
  type_alike(`&&`, sum)   # spe-blt
  type_alike(sum, sd)     # blt-clo
  type_alike(sum, c)      # blt-blt
  type_alike(`&&`, `[`)   # spe-spe

  type_alike(sd, var, mode=1)     # clo-clo
  type_alike(`&&`, sd, mode=1)    # spe-clo
  type_alike(`&&`, sum, mode=1)   # spe-blt
  type_alike(sum, sd, mode=1)     # blt-clo
  type_alike(sum, c, mode=1)      # blt-blt
  type_alike(`&&`, `[`, mode=1)   # spe-spe
} )
