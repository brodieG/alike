library(alike)

unitizer_sect("type_of", {
  type_of(1)           # integer
  type_of(1.0001)      # numeric
  type_of(1L)          # integer
  type_of(1L + 1e-20)  # integer

  type_of(1, int.strict=1L)           # numeric
  type_of(1.0001, int.strict=1L)      # numeric
  type_of(1L, int.strict=1L)          # integer
  type_of(1L + 1e-20, int.strict=1L)  # numeric  
} )

unitizer_sect("type_alike", {
  type_alike("hello", letters)      # TRUE
  type_alike("hello", list())       # FALSE

  type_alike(1L, 2L)        # TRUE
  type_alike(1L, 2.0)       # TRUE
  type_alike(1.0, 2.0)      # TRUE
  type_alike(1.0, 2L)       # TRUE

  type_alike(1.5, 2L)       # TRUE
  type_alike(2L, 1.5)       # FALSE, 1.5 is not integer like
  
  type_alike(1.0, 2L, int.strict=1L)       # TRUE, 2L can be numeric
  type_alike(1L, 2.0, int.strict=1L)       # FALSE, 2.0 cannot be integer

  type_alike(1.0, 2L, int.strict=2L)       # FALSE, 2L cannot be numeric w/ strict=2L

  type_alike("hello", 1L)
  type_alike("hello", 1.1)
})