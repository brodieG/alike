# Alike

`alike` is similar to `all.equal` from base R, except it focuses on comparing object structure rather than actual values.  A simple example:

```{r}
library(alike)
alike(
  matrix(integer(), ncol=3),
  matrix(1:12, nrow=4)
)
```

The structure of these objects is the same: they are both integer matrices with three columns.

`alike` was written to bring some of the guarantees about object structure that S4 provides to S3 objects.  While the best way to achieve that objective is to use S4 classes, so much existing R code relies on S3 classes that a backstop approach is warranted.

See package vignette and function documentation for more details.