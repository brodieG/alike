These are internal developer notes; don't expect any of it to make much sense.

## C Benchmarking

### Assessing General Overhead

microbenchmark(typeof(5.1), typeof2(5.1), type_of(5.1))
Unit: nanoseconds
         expr  min   lq median     uq    max neval
  typeof(5.1)  492  635  723.0  787.0  20245   100
 typeof2(5.1) 2469 2734 2945.0 3255.5  83482   100
 type_of(5.1) 7257 8260 8606.5 9315.0 105588   100

A big chunk of eval time is computing the tolerance value in R

typeof is internal

More details:

fun0 <- function() .Call(al, 1, 1, 1)
fun1 <- function() .Call(al, a, b, c)
fun2 <- function(a, b, c) .Call(al, 1, 1, 1)
fun3 <- function(a, b, c) .Call(al, a, b, c)
fun4 <- function(a, b, c) .Call(al, a, 1, 1)

Unit: nanoseconds
                         expr  min     lq median     uq   max neval
 al <- alike:::ALIKEC_typeof2 5225 5679.5 5930.0 6222.5 32741   500
           .Call(al, 1, 1, 1)  427  495.0  543.0  631.0  1336   500
           .Call(al, a, b, c)  569  628.0  670.0  739.5  1888   500
                       fun0()  631  699.0  772.5  896.0 16414   500
                       fun1()  756  822.0  881.5  994.0  3504   500
                fun2(a, b, c)  799  929.5 1055.0 1220.0  2990   500
                fun3(a, b, c) 1115 1229.5 1358.0 1533.5  3330   500
                fun4(a, b, c)  921 1036.5 1188.0 1292.5  2158   100

fun overhead is ~200ns
vars in .Call is ~130ns
fetching fun args in .Call ~300ns! (this is fun3 vs. fun2), but cleary this is
a per var issue (see fun4, likely on the order of 100ns per var!!!)

In C function calls negligible (seems to be on the order of 15ns)

internal accept C level non main arguments coerced by the externals interface
functions

Using PACKAGE argument to .Call seems to slow things down a fair bit, though
this is with fully registered functions.

Rcpp seems to be generally 2-3x slower than inline, and not just in terms of
overhead (tested on simple loop sum).  Though this may be faster with RcppSugar.
Most of Rcpp overhead seems to be related to the wrapper that converts the
Rcpp value back to an R value.  Perhaps this wrapper can be avoided?  Also,
looks like RCPP doesn't register entry points as objects, so that could make
things a smidge faster as well (instead, relies on PACKAGE argument).  This is
just looking at the default "rcpp_hello_world" example.

typeof:

- base version with no adjustments for speed
- alternate with just tolerance (since typeof is basically like setting mode
  greater than zero? at which point tolerance isn't meaningful?)

typealike:

- base version with no adjustments for speed, default mode (0), and tolerance
- alternate with both tolerance and mode

### Initial Benchmarks

Initial benchmarks (before attr checking):

  lst <-   list(list( 1,  2), list( 3, list( 4, list( 5, list(6, 6.1, 6.2)))))
  lst.2 <- list(list(11, 21), list(31, list(41, list(51, list(61          )))))

  microbenchmark(alike2(lst, lst.2), alike(lst, lst.2), .alike2(lst, lst.2))

  Unit: microseconds
                  expr      min        lq    median       uq      max neval
    alike2(lst, lst.2)    5.644    6.7105    8.4745   11.085   19.995   100
     alike(lst, lst.2) 1106.624 1120.6535 1133.5815 1159.470 2245.159   100
   .alike2(lst, lst.2)    4.012    4.5560    5.4650    7.905   66.953   100

  > microbenchmark(alike2(lst, lst), alike(lst, lst), .alike2(lst, lst))
  Unit: microseconds
                expr      min        lq    median        uq      max neval
    alike2(lst, lst)    3.850    4.7085    6.7295    9.8175   22.973   100
     alike(lst, lst) 2762.135 2823.9385 2865.7025 2957.9535 5773.793   100
   .alike2(lst, lst)    2.235    2.7315    3.3835    5.8075   12.835   100

After adding S4 checks (and also upgrading to OSX 10.9 and R 3.1.1:

    setClass("x", list(a="integer"))
    setClass("z", contains="x", list(b="integer"))
    y <- new("x")
    w <- new("z")

    microbenchmark(alike2(lst, lst), alike(lst, lst), .alike2(lst, lst), .alike2(y, w))

    Unit: microseconds
                  expr      min       lq    median        uq      max neval
      alike2(lst, lst)    3.925    4.693    6.9255    9.0355   18.768   100
       alike(lst, lst) 2326.880 2365.172 2395.5355 2417.4255 3800.582   100
     .alike2(lst, lst)    1.961    2.272    2.5050    3.1080   20.000   100
         .alike2(y, w)    1.934    2.796    3.3625    6.5495   13.822   100

And some that cause errors:

    microbenchmark(.alike2(lst, lst.2), .alike2(w, y))
    Unit: microseconds
                    expr   min     lq median     uq    max neval
     .alike2(lst, lst.2) 3.662 3.7625 3.8665 4.0015 19.204   100
           .alike2(w, y) 2.944 3.2205 3.3315 3.5265 37.653   100

### As of v0.2.2

```
lst <-   list(list( 1,  2), list( 3, list( 4, list( 5, list(6, 6.1, 6.2)))))
lst.2 <- list(list(11, 21), list(31, list(41, list(51, list(61, 62, 63)))))
lst.3 <- list(list(11, 21), list(31, list(41, list(51, list(61         )))))

library(alike)
library(microbenchmark)
microbenchmark(times=1000, .alike(lst.2, lst), .alike(lst.3, lst))
Unit: microseconds
               expr   min    lq median    uq    max neval
 .alike(lst.2, lst) 1.769 1.865  1.947 2.012  4.253  1000   # no error
 .alike(lst.3, lst) 3.226 3.372  3.440 3.535 55.883  1000   # with error
```
### As of Interim v0.3.0

Here we actually start profiling.  Baseline:

    Unit: microseconds
                      expr   min    lq median   uq    max neval
     alike(mtcars, mtcars) 4.351 4.512 4.6065 4.78 32.948  1000

Get rid of unnecessary prepend copy:

    > microbenchmark(alike(mtcars, mtcars), times=1000)
    Unit: microseconds
                      expr   min   lq median    uq    max neval
     alike(mtcars, mtcars) 3.973 4.16 4.3135 4.638 21.119  1000

Astonishingly, getting rid of the index business didn't appear to improve things
much at all:

    > microbenchmark(alike(mtcars, mtcars), times=1000)
    Unit: microseconds
                      expr   min    lq median    uq   max neval
     alike(mtcars, mtcars) 3.954 4.097  4.159 4.325 30.94  1000

Actually, maybe it did.  Had a few bug fixes in between above and below, but
don't see why it would have been slower as a result of bugs:

    > microbenchmark(alike(mtcars, mtcars), times=1000)
    Unit: microseconds
                      expr   min    lq median     uq    max neval
     alike(mtcars, mtcars) 3.719 3.828  3.888 3.9645 20.667  1000

Upon testing a bit more, could be starting with a fresh R process (or alternatively having a bogged down one) that makes the difference?

Some more improvements of swapping out `strcmp` for direct symbol comparison:

    > microbenchmark(alike(mtcars, mtcars), times=1000)
    Unit: microseconds
                      expr   min    lq median    uq    max neval
     alike(mtcars, mtcars) 3.591 3.746 3.8215 3.953 30.225  1000

One issue with all this, laboriously we benchmarked `strcmp` on 8 character strings, and found that 4MM comparisons, it took:

    Unit: microseconds
                    expr       min        lq     median         uq       max neval
     alike_test(1, 2, 3) 16707.573 16806.983 16870.4480 16994.7620 21516.922   100
     alike_test(0, 2, 3)   207.384   209.116   212.1385   221.7965   403.504   100

Or about 4 ns per comparison.  In `mtcars`, based on profiling information from instruments, we found that about 7.3% of the time was spent on `strcmp` comparing names, and there are 43 names (8 cols, 35 rows) or so, so that adds up to about 6.1ns per comparison, which ties out.

When comparing language objects, the hash mechanism we use that needs to allocate a bunch of names is pretty costly.  In order to fix this we would need substantial upgrades to the hashing system so that we can store stuff other than strings.  In particular, we'd want to predifine about 30 or so symbols to avoid having to allocate them at run time.  Looks like we could hash the memory addresses of the symbols (but need to figure out if good hash is portable, etc).

Also, we pre-allocate the object used to deparse the error message even before error occurs.  Is there a way to only do so if an error occurs?  Might be difficult since we need both the starting pointer as well as the location of error pointer in the same pointer chain.

After removing `strcmp` in the cases where the names are known to be identical:

    > microbenchmark(alike(mtcars, mtcars), alike(mtcars.a, mtcars), times=1000)
    Unit: microseconds
                        expr   min     lq median     uq    max neval
       alike(mtcars, mtcars) 2.966 3.1465 3.2430 3.4005  4.728  1000
     alike(mtcars.a, mtcars) 2.931 3.1060 3.2075 3.3820 30.654  1000

Finally, collapsing all the settings into one argument:

    > sets <- list(0L, alike:::MachDblEpsSqrt, 0L, FALSE, sys.frame(sys.nframe()))
    > microbenchmark(alike(mtcars, mtcars), alike_test(mtcars, mtcars, sets), alike_test(mtcars, mtcars, NULL), .alike(mtcars, mtcars), times=10000)
    Unit: microseconds
                                 expr   min    lq median    uq     max neval
                alike(mtcars, mtcars) 2.934 3.161  3.319 3.486 808.905 10000
     alike_test(mtcars, mtcars, sets) 2.157 2.313  2.394 2.534  12.154 10000
     alike_test(mtcars, mtcars, NULL) 2.070 2.243  2.335 2.480 655.997 10000
               .alike(mtcars, mtcars) 1.951 2.124  2.199 2.318  13.585 10000

The extra argument over the original "fast" version costs ~120ns, and the argument validation another ~80ns.


### Stack Manipulation

The is the baseline:

    microbenchmark(alike_test(x, w))
    Unit: nanoseconds
                 expr min  lq median   uq   max neval
     alike_test(x, w) 832 888 1050.5 1212 17721   100

Now let's add a call to `parent.frame` in the R function to pass through `.Call`.

    microbenchmark(alike_test(x, w))
    Unit: microseconds
                 expr   min     lq median     uq     max neval
     alike_test(x, w) 1.114 1.2005  1.311 1.5275 163.422   100

And add a `substitute`:

    microbenchmark(alike_test(x, w))
    Unit: microseconds
                 expr   min    lq median    uq     max neval
     alike_test(x, w) 1.435 1.586  1.672 1.879 160.893   100

At this point we've replicated a promise of sorts, by capturing the expression,
as well as the evaluation environment, but we've add 600ns in evaluation time.

## Recursion

Need to track indices, use a list?  Prototype

    index = CONS(x, R_NilValue)
    alike_rec(tar, cur, index.pl)
    ...
    curr.ind = SEXP
    if(err) {
      res = {1, mkString("Error Message")}
    }
    else if(recurse) {
      SETCDR(index.pl, CONS(curr.ind))
      return(alike_rec(rec(tar.child, cur.child, CDR(index.pl))))
    }
    else
    return {0, R_NilValue}

## Duplication

Duplication is somewhat expensive, with:

    y <- quote(x + fun(1, 2, fun2(a, b)) / 3 + news(23))
    microbenchmark(alike_test(y, 1))
    Unit: microseconds
                 expr   min     lq median    uq    max neval
     alike_test(y, 1) 1.253 1.4035 1.5475 1.754 15.181   100

Reference

    Unit: nanoseconds
                 expr min    lq median   uq   max neval
     alike_test(y, 1) 739 780.5    909 1074 89326  1000

So about half a microsecond to duplicate a somewhat complex object.  Is this tolerable?

## Comparisons

### Language

How do we handle paren calls?  Ideally we would just ignore them, but they change length of call too...

### NULL

Matches NULL at top level, anything when nested?  Should it match any pairlist at top level since zero length pair list is basically NULL?  Hmm...

### Environments

Probably should have contents compared by name, not just sequentially?  How do we treat hashed environments vs non hashed?  Looks like we just use built in functions to do this.

Should we test if environments `identical` before we dive into a full comparison?  Almost certainly yes.

### Attribute Comparison

Exceptions to typical treatment:

* class
* names/row.names
* dimnames
* dim

Examples in favor of `identical`:

* levels
* tsp: start, end, frequency seem akin to dimensions (special treatment for zeroes?)

Examples against

* many of the attributes in the result of `lm`
* zoo
    * index, seems like it just tracks order
    * when have date indices, does alikeness require the same date ranges, or only the same type of index and number of values?
    * what about zoo.reg? frequency seems like it is closer to a dimension variable
    * index is probably closest to row.names, but unfortunately it can be just about anything
* proto objects, urgh
* environments as attributes (e.g. .Environment for formula objects)

### Most Meaningful Elements

1. Check class
2. Check dimensions: infer implicit matrix class

### row.names

We need to treat row.names specially here because of the totally bullshit, well, not truly, way data.frame row names are stored c(NA, n) where "n" is the number of rows; the `getAttrib` access expands that to the full sequence; entirely unclear why we can't just compare the two `c(NA, n)` and just be done; do we really want to allow two row.names attributes that are stored differently but compute to the same value to be identical???

Actually, for now we're just treating this as any other attribute and we'll see if it causes problems.

### Rf_identical

not entirely sure what the "default" flag is for `identical`, seems to be 16 by the convention that all the flags that are TRUE get set to zero, but very confusing why `ignore.environment` basically is the opposite of all the others.  Otherwise it would have made sense to have the default flag be 0  NEED TO TEST CLOSURE COMPARISON!!

## `alike` Arguments

TBD how many arguments we want to pile up.  Here are some to discuss:

1. match.call.env = .GlobalEnv
2. lang.match = fast or slow?

Actually, could combine match.fun.env to be either an environment or NULL, and if NULL don't do the match!

`type.mode`

Probably don't want to check more than 100 numbers by default

Need to handle:

* wanting to specify how many elements before we kill integer-alike tests
* whether to generically allow integer-alikeness
* whether ints can be considered numeric

Before we had 0 (inf integer-alikeness), 1 (must be int), 2 (must be num)

Note this actually affects closures / functions as well

Proposal:
* 0 strictest
* 1 strict, but subset types (e.g. INT) can be considered part of superset (how does this affect functions)
* 2 - N how may 1eN elements we allow before we switch to mode 1

Blergh.  Attempting to combine two dimensions (strictness and allowable size) into one number is really shit.  Really don't want to add another parameter to `alike` though.  Maybe the right answer is to use a fixed allowable size (e.g. 100), and if you really want to use larger then you have to use `.alike` with `settings`.

* attr.mode
* type.mode
* match.call.env
* fuzzy.int.max.len
* suppress.warnings

Answer is probably to simplify `alike` use by not having any arguments other
than target and current; then all other arguments are accessible through
`alike_settings`.

Do we give up on `parent.frame()`?  Do we provide it not even as an argument?

The main draw-back is that by not providing that argument we move away from the standard of having default behavior of `alike` be what we think is the most correct in most cases.  One possibility is to use `parent.frame`, but not have it as an argument in the actual R interface function.  This won't save us any time, and we lose the flexibility of actually turning off function matching, but the interface is consistent (therere isn't one random argument that we can adjust.)

Another option might be allow flexible numeric types only for scalars under the view that those are the only ones that are likely to be manually input?

## Error Messages

When looking at something like:
```
## Error in analyze.test.run(x = run.1): Argument `x` should be "dist" at index [[1]] for "names" (is "x") at index [["data"]]
```
Perhaps:
```
Argument `x` should have `names(x$data)[[1]]` be "dist" (is "x")
```
would be better?
```
- Error in analyze(x = laps.3):
-   Argument `x` should be class "POSIXct" (is "numeric") at index
-   [["data"]][["time"]]

+ Error in analyze(x = laps.3):
+   Argument `x` should have `x$data$time` be class "POSIXct" (is "numeric")

+ Error in analyze(x = laps.2):
+   Argument `x` should have a "names" attribute ("names" missing)

+ Error in analyze(x = laps.2):
+   Argument `x` should have "names" ("names" attribute missing)

- Argument `x` should be \"a\" at index [[1]] for \"levels\" (is \"k\") at 
- index [[2]][[\"b\"]]

+ Argument `x` should have `levels(x[[2]]$b)[[1]]` be \"a\" (is \"k\")
+ > str(x[[2]]$b)
+ List of 2
+ ...

+ Argument `x` should have `attr(x[[2]]$b, "levels")[[1]]` be \"a\" (is \"k\")
```

### Names

Special case, particularly because we **could** use them to ascertain missingness, but what about rownames/colnames?  How comprehensive a view of the mismatch do we want to provide?  We can't realistically provide a full diff; under what circumstances do we want to point out exactly what's wrong?  Some scenarios:

* Missing names
* Names completely mismatched
* One name missing
* One name mismatch

One of the annoyances with the existing mechanism is that it tells you about the first mismatch, which may be one of many, and it makes it seem like only one is wrong.  Ideally we would get a comprehensive diff of the mismatches with some logic to make the screen output manageable (i.e. don't necessarily show the diffs if names are completely wrong).

```
- Error in analyze(x = laps.1):
-   Argument `x` should be "car" at index [[1]] for "names" (is "lap")

+ Error in analyze(x = laps.1):
+   Argument `x` should have `names(laps.1[["track"]][["curves"]])[[1]]` be 
+   "car" (is "lap"); here is a snapshot of the object as supplied:
+ > str(laps.1[["track"]][["curves"]])
+ | List of 2
+ |  $ lap : int [1:10] 1 2 3 4 5 6 7 8 9 10
+ |  $ time: num [1:10] 123 241 363 481 600 ...
+ |  - attr(*, "row.names")= int [1:10] 1 2 3 4 5 6 7 8 9 10
+ |  - attr(*, "class")= chr "laps"

+ Error in analyze(x = laps.1):
+   Argument `x` should have `names(laps.1$track$curves]])[[1]]` be 
+   "car" (is "lap"); here is a snapshot of the object as supplied:
+ > str(laps.1$track$curves)
+ | List of 2
+ |  $ lap : int [1:10] 1 2 3 4 5 6 7 8 9 10
+ |  $ time: num [1:10] 123 241 363 481 600 ...
+ |  - attr(*, "row.names")= int [1:10] 1 2 3 4 5 6 7 8 9 10
+ |  - attr(*, "class")= chr "laps"

# OR:

+ Error in analyze(x = laps.1):
+   Argument `x` is missing element "car"

# OR:

+   Argument `x` should have x[["car"]] defined (is missing)

# -----------------------------------------------------------------------------

- Error in analyze(x = laps.2):
-   Argument `x` should be type "character" (is "NULL") for "names"

```
### Language objects

Langauge object diffs actually provide a lot more information, so minor tweaks might be okay.

```
- should be a call to `+` (is "symbol") for `Sepal.Width` in:
- > Sepal.Length ~ `{Sepal.Width}`
- at index [["terms"]]

+ should have `x[["terms"]]` contain a call to `+` (is "symbol") at
+ `Sepal.Width` in:
+ > Sepal.Length ~ `{Sepal.Width}`
```

## Bugs to Report

```
> x <- 25
> x <- integer()
> attr(x, "special") <- list(1, 2, setNames(1:3, letters[1:3]))
> y <- list(NULL, NULL, x)
> z <- y
> z[[1]] <- list(list(1, "a"), "hello")
> z[[2]] <- 25
> w <- x
> attr(w, "special") <- list(1, 2, setNames(1:3, letters[2:4]))
> z[[3]] <- w
> alike(y, z)
Error in alike(y, z) : 
  Logic Error: unexpected index type 1661633088; contact maintainer.
```
