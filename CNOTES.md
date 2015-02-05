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

SEXPs that are created outside of the `.Call` call don't need to be protected (kind of, our attempt to create pre-composed calls in initialization script failed miserably, but single objects work fine).

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

