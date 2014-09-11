library(alike)

unitizer_sect("alike", {
  lst <-   list(list( 1,  2), list( 3, list( 4, list( 5, list(6, 6.1, 6.2)))))
  lst.2 <- list(list(11, 21), list(31, list(41, list(51, list(61          )))))

  alike2(lst, lst.2)     # length mismatch

  lst.3 <- lst.2
  lst.3[[2]][[2]][[2]][[2]] <- matrix(1:9, nrow=3)

  alike2(lst, lst.3)     # object type mismatch
  alike2(1:10, "hello")  # object type mismatch, no dive

  alike2(lst, lst)       # obvious match

  lst.4 <- lst
  lst.4[[2]][[2]] <- list()

  alike2(lst.4, lst)     # should match
  alike2(lst, lst.4)     # should not match because template has more detail   
} )

unitizer_sect(".typeof", {
  .Machine$double.eps    # just to record precision these tests were run in
  a <- b <- 1:100
  a[[1]] <- a[[1]] + .0000001
  b[[1]] <- b[[1]] + .00000001

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
  .type_alike2(1e6, 1e6 + .1)   # FALSE
  .type_alike2(1e7, 1e7 + .1)   # TRUE
  .type_alike2(data.frame(a=1:10), list())
  .type_alike2(NULL, NULL)
  .type_alike2(1/0, NA)
} )

unitizer_sect("type_alike", {
  type_alike2(1, 1.1)
  type_alike2(1, 1.00000001)
  type_alike2(list(), integer())
  type_alike2(a, 1:100)        # TRUE, integer can be considered numeric
  type_alike2(1:100, a)        # FALSE
  type_alike2(1:100, b)        # TRUE
  type_alike2(1e6, 1e6 + .1)   # FALSE
  type_alike2(1e7, 1e7 + .1)   # TRUE
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