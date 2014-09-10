# C Benchmarking

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
overhead (tested on simple loop sum).

typeof:

- base version with no adjustments for speed
- alternate with just tolerance (since typeof is basically like setting mode
  greater than zero? at which point tolerance isn't meaningful?)

typealike:

- base version with no adjustments for speed, default mode (0), and tolerance
- alternate with both tolerance and mode