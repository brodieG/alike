# alike - Verify S3 Object Structure

**NOTE**: `alike` will likely cease to exist as a stand alone package and become part of `validate` at some point in the future.  If this happens the interface will be unchanged, but the functions will be in a different package.

## Introduction

**NOTE**: this is a snippet from the **[vignette](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/alike/master/inst/doc/alike.html)**; we recommend you look at the actual vignette as it will evaluate the expressions below.

`alike` is similar to `all.equal` from base R, except it focuses on comparing object structure rather than actual values:

```{r}
library(alike)
alike(integer(), 1:5)
alike(integer(), letters[1:5])
alike(integer(4), 1:5)
alike(integer(5), 1:5)
```
`alike` only compares the structure of the objects (e.g. type, length, class, attributes, etc.), which is how we can match `1:5` to `integer(5)` even though these have clearly different values.

`alike` will also compare recursive structures element by element, which allows things such as:

```{r}
df.tpl <- data.frame(id=integer(), grade=factor(levels=LETTERS[1:6]))
df.correct <- data.frame(id=1:3, grade=factor(c("F", "A", "C"), levels=LETTERS[1:6]))
df.wrong <- data.frame(id=4:6, grade=c("F", "A", "C"))

alike(df.tpl, df.correct)
alike(df.tpl, df.wrong)
```
`alike` will be used in `validate` to provide a template based mechanism for validating user inputs to functions, but can be used in any place that verifying object structure is useful.

## Installation

Currently this package is only available from github:

```
library(devtools)
install_github("brodieg/alike")
```
