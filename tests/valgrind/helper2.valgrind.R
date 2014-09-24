# version of helper unitizer tests for use with valgrind

# library(alike)
# gctorture(TRUE)

# a <- b <- 1:100
# a[[1]] <- a[[1]] + .0000001
# b[[1]] <- b[[1]] + .00000001

# .type_of(data.frame(a=1:10))
# .type_alike(list(), integer())
# .type_alike(10000000L, 10000000L + .1)   # TRUE



library(alike)
gctorture(TRUE)

# Error seems to happen only when have two SEXPs assigned in one statement
# And they are both modified with `[[<-`
# And then we do .type_of on a data.frame

a <- b <- 1:100
a[[1]] <- a[[1]] + .0000001
b[[1]] <- b[[1]] + .00000001

df <- data.frame(c=1:10)   # has to be data.frame, list doesn't cause problem
.type_of(df)
.type_alike(list(), integer())