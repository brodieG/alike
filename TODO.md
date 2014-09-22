# S4 classes

* environment comparison issues; these change every time we load a session

# Optimize!!!

* Currently working in C, looks promising.
* Make sure all the safety / infinite loop stuff and stack re-allocation is implemented


---
# Random Notes

ref: 1L   # integer
cur: 1.0  # numeric, integer like

ref: 1.0  # numeric, integer like
cur: 1L   # 

Are these two levels really distinct:
- Integers can be consider numeric no matter what
- Integer like numerics are integers

Basically, is there a situation where you would like integers to be accepted as
numeric, but not the other way around?  In addition to having the option of also
allowing integer like numerics to be treated as numeric?

int.strict = integers can only match integers

* 1L, 1.0 # FALSE
* 1.0, 1L # FALSE

int.loose = integers can be numeric

* 1L, 1.0 # FALSE
* 1.0, 1L # TRUE

int.super.loose = integer like numerics can be integers

* 1L, 1.0 # TRUE
* 1.0, 1L # TRUE

# Things to test

## alike.c

* Make sure our use of `flags` argument to R_compute_identical() is correct
* Test that our decision to use direct comparison or `rownames` attribute isn't
  a disaster
* should we disable the zero length matches any length for the stricter modes?
  does this mean we need another len_mode argument?  would rather avoid more
  args.  A more complex mode argument?
* should names be allowable as a partially specified attribute?  If yes, what
  about row names?  The answer is probably yes, as this would then align with
  `dimnames`, which would make sense.
* add tests for reference classes