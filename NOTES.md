These are internal developer notes

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
