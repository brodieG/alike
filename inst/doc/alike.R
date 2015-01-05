## ------------------------------------------------------------------------
library(alike)
alike(
  matrix(integer(), ncol=3),
  matrix(1:12, nrow=4)
)

## ------------------------------------------------------------------------
alike(matrix(integer(), nrow=3), matrix(1:12, ncol=4))   # same as in intro
alike(matrix(1:12, ncol=4), matrix(integer(), nrow=3))   # reverse args

## ------------------------------------------------------------------------
alike(integer(), 1:10)
alike(NULL, mtcars)
alike(list(), list(iris, mtcars))
alike(list(NULL, NULL), list(iris, mtcars))
alike(list(NULL, NULL), list(iris, mtcars, warpbreaks))

## ------------------------------------------------------------------------
alike(1L, 1)     # note 1 is not integer
alike(1L, 1.1)   # 1.1 is not integer-like
alike(1.1, 1L)   # but integers can match numerics

## ------------------------------------------------------------------------
# we use `structure` to circumvent default data.frame names
df.tpl <- structure(list(id=integer(), numeric(), numeric()), class="data.frame")
df.dat1 <- data.frame(id=1:3, x=runif(3), y=runif(3))
df.dat2 <- data.frame(bogus=1:3, x=runif(3), y=runif(3))

alike(df.tpl, df.dat1)
names(df.tpl)
names(df.dat1)
rownames(df.tpl)
rownames(df.dat1)

alike(df.tpl, df.dat2)

## ------------------------------------------------------------------------
mx.tpl <- matrix(integer(), ncol=3);
mx.cur <- matrix(1:12, nrow=4)
mx.cur2 <- matrix(1:12, nrow=3)
alike(mx.tpl, mx.cur)

dim(mx.tpl)    # notice the 0 for the first value in the `dim` vector
dim(mx.cur)

alike(mx.tpl, mx.cur2)

## ------------------------------------------------------------------------
mx.tpl <- matrix(integer(), ncol=3, dimnames=list(row.id=NULL, c("R", "G", "B")))
mx.cur <- matrix(sample(0:255, 12), ncol=3, dimnames=list(row.id=1:4, rgb=c("R", "G", "B")))
mx.cur2 <- matrix(sample(0:255, 12), ncol=3, dimnames=list(1:4, c("R", "G", "B")))

alike(mx.tpl, mx.cur)
alike(mx.tpl, mx.cur2)
names(dimnames(mx.tpl))

## ------------------------------------------------------------------------
tpl <- structure(NULL, class=c("a", "b", "c"))
cur <- structure(NULL, class=c("x", "a", "b", "c"))
cur2 <- structure(NULL, class=c("a", "b", "c", "x"))

alike(tpl, cur)
alike(tpl, cur2)

## ------------------------------------------------------------------------
library(microbenchmark)
microbenchmark(
  all.equal(1:10, 1:10),
  identical(1:10, 1:10),
  alike(1:10, 1:10),
  all.equal(mtcars, mtcars),
  identical(mtcars, mtcars),
  alike(mtcars, mtcars)
)

## ------------------------------------------------------------------------
library(microbenchmark)
microbenchmark(
  alike(1:10, 1:10),
  .alike(1:10, 1:10)
)

## ------------------------------------------------------------------------
df.tpl <- data.frame(integer(), numeric())
df.cur <- data.frame(a=1:10, b=1:10 + .1)

library(microbenchmark)
mb <- microbenchmark(
  alike(df.tpl, df.cur),
  alike(data.frame(integer(), numeric()), df.cur)
)
summary(mb)[1:4]

