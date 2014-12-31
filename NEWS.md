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
