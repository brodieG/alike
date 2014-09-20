# C Benchmarking

## Assessing General Overhead

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

## Alike

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

    setClass("z", contains="x", list(b="integer"))
    setClass("x", list(a="integer"))
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

## Stack Manipulation

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