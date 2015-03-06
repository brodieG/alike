# alike - Verify S3 Object Structure

**NOTE**: this is a snippet from the **[vignette](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/alike/master/inst/doc/alike.html)**; we recommend you look at the actual vignette as it will show the evaluated R expressions and the links will work.

## What is Alikeness?

`alike` is similar to `all.equal` from base R except it only compares object structure.  As with `all.equal`, the first argument (`target`) must be matched by the second (`current`).

```{r}
library(alike)
alike(integer(5), 1:5)      # different values, but same structure
alike(integer(5), 1:4)      # wrong size
alike(integer(26), letters) # same size, but different types
```

`alike` only compares structural elements that are defined in `target` (a.k.a. the template).  This allows "wildcard" templates.  For example, we consider length zero vectors to have undefined length so those match vectors of any length:

```{r}
alike(integer(), 1:5)
alike(integer(), 1:4)
alike(integer(), letters)  # type is still defined and must match
```

Similarly, if a template does not specify an attribute, objects with any value for that attribute will match:

```{r}
alike(list(), data.frame())  # a data frame is a list with a attributes
alike(data.frame(), list())  # but a list does not have the data.frame attributes
```

As an extension to the wildcard concept, we interpret partially specified [core R attributes](#Special Attributes).  Here we allow any three column integer matrix to match:

```{r}
mx.tpl <- matrix(integer(), ncol=3)          # partially specified matrix
alike(mx.tpl, matrix(sample(1:12), nrow=4))  # any number of rows match
alike(mx.tpl, matrix(sample(1:12), nrow=3))  # but column count must match
```

or a data frame of arbitrary number of rows, but same column structure as `iris`:

```{r}
iris.tpl <- iris[0, ]                        # no rows, but structure is defined
alike(iris.tpl, iris[1:10, ])                # any number of rows match
alike(iris.tpl, CO2)                         # but column structure must match
```

"alikeness" is complex to describe, but should be intuitive to grasp. We recommend you look at the examples in the documentation for `alike` to get a sense for alikeness.  If you want to understand the specifics, read on.

## Motivation

`alike` allows for template based validation of function arguments, and is used for that purpose by `validate`. S3 object templates represent structural requirements in a manner akin to prototypes in some OOP languages.  Verifying the structural similarity of an S3 object as it enters our functions allows us to write simpler and more robust code.  The S4 system does this and more, but S3 objects are still used extensively in R code.

An alternative to template based validation is to directly check the structure of objects (e.g. `is.numeric(.) && length(.) == .`).  There are a few advantages to the template based approach:

* Often times it is simpler to define a template than to write out all the checks to confirm an object conforms to a particular structure
* We can generate the template from a known correct instance of an object and [abstract away](#Abstracting-Existing-Objects) the elements that are not specific to the prototype (this is particularly valuable for otherwise complex objects)
* We can produce plainish-english interpretations of structural mismatches since we are dealing with a known limited set of comparisons
* Much of the code we would repeat in every function is encapsulated within `alike`

## Installation

Currently this package is only available from github:

```
library(devtools)
install_github("brodieg/alike")
```
