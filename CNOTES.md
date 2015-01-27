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



## Understanding How to Read C Types

