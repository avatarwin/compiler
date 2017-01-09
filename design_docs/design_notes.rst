Scheme Compiler Notes
=====================

Rationale
---------

Common Lisp problems:

-  overly complex standard library

-  much cruft focused on older programming paradigms (non functional
   loops, etc)

-  fewer high-order programming tools (see above) (that said, cl-arrows
   and such do exist)

-  no focus on TCO recursion

-  limited macros

-  Lisp-2 is VERY annoying

Scheme problems:

-  No 'declare' support for static typing

-  Bulky and inconsistant SRFIs (lists and vectors have a wide range of
   modern hi-order operations thanks to srfi-1 & srfi-133, but the typed
   vectors, eg f32vector, do not).

Common problems:

-  No easy solution for *all* targets wanted (avr, pic, arm/cortex - CL
   has an advantage here)

-  Both lisp and scheme aren't really suited for a offline complication,
   some scheme systems (such as chicken) offer this, but it is not
   standard.

Garbage Collector
-----------------

The box address, box type and GC intrinsics should be allocated/kept
separate from the actual data in memory, such that GC passes can be more
optimal.

Stack vs Heap Allocation
------------------------

Literals used inside a function's scope can be allocated on the stack
with no need for GC if the continuation is not used elsewhere. Most
integer literals can be unboxed completely in this way.

The macro expander
------------------

Code generation
---------------

Library functions
-----------------

Numeric Tower
-------------

Unlike scheme, we should have bignum & ratio as part of the numeric
tower.

Pure Functions
--------------

Function signatures should include a pure bit. If the function is
referentially pure then it is tagged pure, a function can only be
referentially pure if it uses only referntially pure functions.
