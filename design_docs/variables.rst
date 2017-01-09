Basic scheme types are:
=======================

-  Number

   -  fixnums (integers)
   -  flonums (floating points/reals)
   -  complex number (a pair of reals usually)
   -  bignum (a variable size integer)
   -  ratio (a fraction - a pair of integers)

-  strings
-  character (a single character)
-  pair (a cons cell)
-  list (usually chained cons cells)
-  vector
-  hashmap
-  environment (used internally to represent symbol tables)
-  procedure (a function)

Integer types
=============

The default integer form should be whichever is the native integer type
for the platform. Alternate forms should be specified with ``declare``. 
