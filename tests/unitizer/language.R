library(alike)
unitizer_sect("Functions", {
  alike(sd, median)                     # TRUE
  alike(sd, cor)                        # "does not have the expected formals structure"
} )
unitizer_sect("Calls / Formulas", {
  alike(quote(1 + 1), quote(x + y))
  alike(quote(fun(1 + 1)), quote(fun(x + y, 9)))
  alike(quote(fun(x + y, 9)), quote(fun(1 + 1)))

  if(exists("fun")) rm(fun)
  alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(NULL, fun2(a, b), 1)))   # sub-optimal error message, but necessary to handle cases that engage match.call
  alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(b=NULL, fun2(a, b), 1))) # clearer what's going on here
  fun <- function(a, b, c) NULL
  alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(NULL, fun2(a, b), 1)))  # Because fun defined, uses match.call() to re-arrange args
  alike(quote(fun(b=fun2(x, y), 1, 3)), quote(fun(fun2(a, b), NULL, 1)))
  alike(quote(fun(a=1)), quote(fun(b=1)))

  alike(quote(fun(1, 2)), quote(fun(1)))
  alike(quote(fun(1)), quote(fun(1, 2)))

  alike(quote(fun(1, 2)), quote(fun2(1, 2)))
  alike(quote(fun(1, fun2(3))), quote(fun(1, fun(3))))

  alike(x ~ y, z ~ w)
  alike(x ~ y, z ~ w + 1)
  alike(x ~ y + 2, z ~ w + 1)
  alike(x ~ y + z:y, w ~ v + u:v)
  alike(z ~ w + 1, x ~ y)
  alike(y ~ x ^ 2 + x * z + z + w:z, q ~ l ^ 2 + l * j + j + w:j)
  alike(y ~ x ^ 2 + x * z + z + w:z, q ~ l ^ 3 + l * j + j + w:j)

  # Repeating parses to deal with potential parse issues in clean R runs

  exp.1 <- parse(text="x + y; fun2(fun(1, 2, 3), z)", keep.source=TRUE)
  exp.2 <- parse(text="z + 2; fun(fun2(1, 2, 3), q)", keep.source=TRUE)
  exp.3 <- parse(text="z + fun(3); fun(fun2(a, b, c), 3)", keep.source=TRUE)

  alike(exp.1, exp.2)
  alike(exp.2, exp.3)
  alike(exp.3, exp.2)
} )
