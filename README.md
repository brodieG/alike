<!-- README.md is generated from README.Rmd. Please edit that file -->


# alike - Verify S3 Object Structure

[![](https://travis-ci.org/brodieG/alike.svg?branch=master)](https://travis-ci.org/brodieG/alike)
[![](https://codecov.io/github/brodieG/alike/coverage.svg?branch=master)](https://codecov.io/github/brodieG/alike?branch=master)
[![Project Status: Unsupported – The project has reached a stable, usable state but the author(s) have ceased all work on it.](http://www.repostatus.org/badges/latest/unsupported.svg)](http://www.repostatus.org/#unsupported)

> This project is being integrated into [vetr](https://github.com/brodieG/vetr).
> Please submit but reports, feature requests, etc., on that project's home
> page.

## What is Alikeness?

`alike` is similar to `all.equal` from base R except it only compares object structure.  As with `all.equal`, the first argument (`target`) must be matched by the second (`current`).


```r
library(alike)
alike(integer(5), 1:5)      # different values, but same structure
## [1] TRUE
alike(integer(5), 1:4)      # wrong size
## [1] "`1:4` should be length 5 (is 4)"
alike(integer(26), letters) # same size, but different types
## [1] "`letters` should be type \"integer-like\" (is \"character\")"
```

`alike` only compares structural elements that are defined in `target` (a.k.a. the template).  This allows "wildcard" templates.  For example, we consider length zero vectors to have undefined length so those match vectors of any length:


```r
alike(integer(), 1:5)
## [1] TRUE
alike(integer(), 1:4)
## [1] TRUE
alike(integer(), letters)  # type is still defined and must match
## [1] "`letters` should be type \"integer-like\" (is \"character\")"
```

Similarly, if a template does not specify an attribute, objects with any value for that attribute will match:


```r
alike(list(), data.frame())  # a data frame is a list with a attributes
## [1] TRUE
alike(data.frame(), list())  # but a list does not have the data.frame attributes
## [1] "`list()` should be class \"data.frame\" (is \"list\")"
```

As an extension to the wildcard concept, we interpret partially specified [core R attributes](#Special Attributes).  Here we allow any three column integer matrix to match:


```r
mx.tpl <- matrix(integer(), ncol=3)          # partially specified matrix
alike(mx.tpl, matrix(sample(1:12), nrow=4))  # any number of rows match
## [1] TRUE
alike(mx.tpl, matrix(sample(1:12), nrow=3))  # but column count must match
## [1] "`matrix(sample(1:12), nrow = 3)` should have 3 columns (has 4)"
```

or a data frame of arbitrary number of rows, but same column structure as `iris`:


```r
iris.tpl <- iris[0, ]                        # no rows, but structure is defined
alike(iris.tpl, iris[1:10, ])                # any number of rows match
## [1] TRUE
alike(iris.tpl, CO2)                         # but column structure must match
## [1] "`names(CO2)[1]` should be \"Sepal.Length\" (is \"Plant\")"
```

"alikeness" is complex to describe, but should be intuitive to grasp. We recommend you look at the examples in the documentation for `alike` to get a sense for alikeness.  If you want to understand the specifics, read on.

**NOTE**: this is a snippet from the **[vignette](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/alike/master/inst/doc/alike.html)**; we recommend you look at the actual vignette as it will show the evaluated R expressions and the links will work.

## Installation

Currently this package is only available from github:

```
library(devtools)
install_github("brodieg/cstringr")  # required dependency
install_github("brodieg/alike")
```
