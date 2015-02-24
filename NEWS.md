alike 0.3.0
===========

External Changes:

* Added support for additional recursive objects: environments, pairlists, and
  expressions are now recursed through, with `alike` applied recursively to each
  elemment therein; this mirrors the previous treatment for lists (the VECSXP
  type)
* Improved comparison of language objects (calls, symbols, formulas, and
  functions); see docs for explanations of what makes language objects alike
  (#10, #11, #14, #16)
* Attributes are now recursively compared with `alike` as well instead of
  identical
* Added `abstract` functions to assist in the creation of objects (#9)
* `ts` class objects now have specialized comparison

Internal Changes:

* C level optimizations (#7, #13, but #13 not complete)

alike 0.2.5
===========

* Doc updates
* removed C library files to allow installs on platforms with different
  compilers

alike 0.2.4
===========

* NULLs are only wildcard when nested inside other objects

alike 0.2.3
===========

* Clearer error messages focus on highest level differences first (e.g. class)
* `names`/`row.names` may now be shorter in `target` than `current`
* major internal re-structuring; should be transparent to user

**NOTE**: `alike` will likely cease to exist as a stand alone package and become part of `validate` at some point in the future.  If this happens the interface will be unchanged, but the functions will be in a different package.

alike 0.2.2
===========

* Internal changes to support `validate`

alike 0.2.1
===========

* Now unloading dynamic libraries on package unload (Fixes #1), though some
  questions as to whether this introduces other issues when loading/unloading
  same package (happened once, can't reproduce).

alike 0.2
=========

* Primarily moved to C based computations to run faster
* Lost some functionality in the process of doing the above
