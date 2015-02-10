## Garbage Collection

### What Should Be PROTECTed

`SEXP` that you create must be protected

### How Does Protection Work

**Note**: a lot of what follows is somewhat untested

Protection is purely a function of the protection stack; the following two are equivalent (assuming GC cant happen between the two statements in the second point):

* `x = PROTECT(ScalarLogical(1));`
* `x = ScalarLogical(1); PROTECT()`

A corollary of the above is that it is often useful to use spurious `PROTECT` calls to maintain stack balance in forked logic:

    if(i > 5) w = PROTECT(ScalarLogical(1)); ...
    else PROTECT(); ...
    ...
    UNPROTECT(1);  // Otherwise would have to unprotect 0 or 1 depending on logic above

### When Is Protection Not Necessary?

Only SEXPs that are not pointed to by protected SEXPs need to be protected, for example in:

    x = PROTECT(list2(ScalarLogical(1), ScalarLogical(2)));
    y = CAR(x);
    z = CADR(x);

We don't need to `PROTECT` `y` or `z` as they are already protected as members of `x`.

Also, assuming `z` is protected already:

    SETCDR(z, list1(scalarLogical(1)));

Does not require any protection even though we are creating two new `SEXP`s because `z` is already protected and `z` will point to its CDR, obviously.

SEXPs that are created within the initialization script `R_init_...` don't need to be protected (had some issues earlier trying to create nested objects, but seems to work now; perhaps the requirement is that all objects that are in nested objects must themselves exist at the top level so the GC counter works properly?).

## Output / Inspection

void Rprintf(const char *, ...);
void REprintf(const char *, ...);
  like `Rprintf`, but prints to stderr!

void PrintValue(SEXP)
  equivalent to `print` in R!

void error(const char *, ...);
  equivalent to `stop` in R, always has for context the function that initially `.Call` the R code.


## Conversions

SEXP mkString(const char *s)
  return: STRSXP, equivalent to ScalarString(mkChar(x))

SEXP ScalarString(SEXP):
  param: should be CHARSXP, i.e. `ScalarString(mkChar(x))`
  return: STRSXP

SEXP PRINTNAME(SEXP)
  param: should be a pairlist tag (e.g `TAG(X)`), presumably a symbol
  return: CHARSXP
  can potentially convert to STRSXP with: `ScalarString(PRINTNAME(TAG(x)))`

SEXP asChar(x):
  param: STRSXP
  return: CHARSXP, first element in x

From TAG to R object:
  ScalarString(PRINTNAME(TAG(x)))

From Hadley:

There are a few helper functions that turn length one R vectors into C scalars:

    asLogical(x): LGLSXP -> int
    asInteger(x): INTSXP -> int
    asReal(x): REALSXP -> double
    CHAR(asChar(x)): STRSXP -> const char*

And helpers to go in the opposite direction:

    ScalarLogical(x): int -> LGLSXP
    ScalarInteger(x): int -> INTSXP
    ScalarReal(x): double -> REALSXP
    mkString(x): const char* -> STRSXP

## NA values

From arith.h:

    #define NA_LOGICAL  R_NaInt
    #define NA_INTEGER  R_NaInt
    /* #define NA_FACTOR  R_NaInt  unused */
    #define NA_REAL   R_NaReal

## Understanding How to Read C Types


## Hash Tables vs Simple Lookup

One question is whether using hash tables makes sense for lookups on things that are likely to be reasonably small.  Here are some tests of various sizes.  We're using the pfhash hash tables:

    x <- do.call(paste, expand.grid(letters, letters, letters))
    y <- do.call(paste, expand.grid(LETTERS, LETTERS, LETTERS))
    names(x) <- x
    names(y) <- x
    x.0 <- as.pairlist(x[1:10])
    y.0 <- as.pairlist(y[1:10])
    x.1 <- as.pairlist(x[1:25])
    y.1 <- as.pairlist(y[1:25])
    x.2 <- as.pairlist(x[1:50])
    y.2 <- as.pairlist(y[1:50])
    x.3 <- as.pairlist(x[1:100])
    y.3 <- as.pairlist(y[1:100])
    x.4 <- as.pairlist(x[1:200])
    y.4 <- as.pairlist(y[1:200])
    microbenchmark(
      alike_test(x.0, y.0, 1),
      alike_test(x.0, y.0, 0),
      alike_test(x.1, y.1, 1),
      alike_test(x.1, y.1, 0),
      alike_test(x.2, y.2, 1),
      alike_test(x.2, y.2, 0),
      alike_test(x.3, y.3, 1),
      alike_test(x.3, y.3, 0),
      alike_test(x.4, y.4, 1),
      alike_test(x.4, y.4, 0)
    )

Produces:

    Unit: microseconds
                        expr     min       lq   median       uq     max neval
     alike_test(x.0, y.0, 1)   3.518   4.1180   4.4765   6.1930  11.338   100
     alike_test(x.0, y.0, 0)   2.167   2.5025   2.6570   2.8010  12.652   100
     alike_test(x.1, y.1, 1)   6.649   7.2300   7.8070  10.5965  31.948   100
     alike_test(x.1, y.1, 0)   6.040   6.3695   6.5710   6.7890   8.065   100
     alike_test(x.2, y.2, 1)  11.477  12.6375  14.4195  17.7555  33.460   100
     alike_test(x.2, y.2, 0)  18.804  19.1270  19.2685  19.4835  28.185   100
     alike_test(x.3, y.3, 1)  21.527  23.6695  25.4665  31.8590  53.237   100
     alike_test(x.3, y.3, 0)  67.789  68.1235  68.3185  68.5460  79.175   100
     alike_test(x.4, y.4, 1)  43.806  46.4580  49.4490  63.4970 113.256   100
     alike_test(x.4, y.4, 0) 259.650 260.0055 260.3515 262.0430 285.271   100

0 indicates no hash table.  This suggests up to about ~25 elements we're better off not using a hash table due to the associated overhead...  Additionally, it looks like `xlength` takes about 1ns per element in list to operate, so maybe the correct way to do this is to check length, and if greater than 25, use hash, else just do a double loop.  Or even better than `xlenght`, just use a loop to count length up to 25, adds a little overhead if list is small, but probably as soon as we get above 50 `for` wins.
