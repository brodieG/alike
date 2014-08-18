library(alike)

# - Attribute Matching ------------

obj1 <- structure(numeric(), dim.sizes=integer())
obj2 <- structure(numeric(), height=10.9, color="red", dim.sizes=c(4L, 2L, 8L))
attributes_alike(obj1, obj2)  # TRUE
attributes_alike(obj2, obj1)  # "is missing required attribute `height`"
obj1 <- structure(numeric(), dim.sizes=integer(4L), height=character())
attributes_alike(obj1, obj2)  # "has incorrect value for attribute `dim.sizes`"
obj1 <- structure(numeric(), dim.sizes=numeric(), height=numeric(), color=character(1L))
attributes_alike(obj1, obj2)  # "has incorrect value for attribute `color`"
obj1 <- structure(numeric(), dim.sizes=numeric(), height=numeric(), color=character())
attributes_alike(obj1, obj2)  # TRUE
obj1 <- structure(numeric(), dim.sizes=c(4L, 2L, 8L), color="red")
attributes_alike(obj1, obj2)  # TRUE
obj1 <- structure(numeric(), dim.sizes=c(4, 2, 8), color="red")
attributes_alike(obj1, obj2, compare.mode="identical") # "has incorrect value for attribute `dim.sizes`"
obj1 <- structure(numeric(), dim.sizes=c(4.01, 2.01, 8.01), color="red")
attributes_alike(obj1, obj2, tolerance=0.1)  # TRUE

obj1 <- matrix(numeric(), nrow=7)
obj2 <- matrix(1:21, nrow=7)

attributes_alike(obj1, obj2)  # TRUE
attributes_alike(obj1, obj2, special.attrs=FALSE)  # "has incorrect value for attribute `dim`"

# - struct_alike ------------------

struct_alike(1L:3L, c(1.0, 2.0, 3.0))        # TRUE
struct_alike(numeric(), c(1.0, 2.0, 3.0))    # TRUE
struct_alike(numeric(3L), c(1.0, 2.0, 3.0))  # TRUE
struct_alike(numeric(4L), c(1.0, 2.0, 3.0))  # "should have length 4 instead of 3"
struct_alike(integer(), c(1.0, 2.0, 3.0))    # TRUE
struct_alike(integer(), c(1.01, 2.0, 3.0))   # "should be integer instead of double"
struct_alike(matrix(integer(), ncol=4), matrix(1:12, nrow=3)) # TRUE 
struct_alike(data.frame(), data.frame(a=1:3, b=letters[1:3])) # TRUE
struct_alike(data.frame(a=integer(), b=factor()), data.frame(a=1:3, b=letters[1:3]))    # TRUE, note this is recursive
struct_alike(data.frame(a=integer(), b=character()), data.frame(a=1:3, b=letters[1:3])) # 
struct_alike(data.frame(a=factor(), b=factor()), data.frame(a=1:3, b=letters[1:3]))
struct_alike(sd, median)                     # TRUE
struct_alike(sd, cor)                        # "does not have the expected formals structure"
