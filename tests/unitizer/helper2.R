library(alike)

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
unitizer_sect("compare attributes, default", {
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
unitizer_sect("compare attributes, strict", {
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
