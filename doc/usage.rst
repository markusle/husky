============================================================
husky command line calculator
============================================================

:Author: Markus Dittrich

:Version: 0.4 (04/04/2009)


Introduction
------------

husky is a command line calculator with a small memory
footprint. It can be used in a fashion similar to the
interactive shells of python, octave, or ghci.

Functionality
-------------

Husky presently can be used as 

1) calculator 
2) unit converter 

The following sections describe in detail each functionality.


Calculator
==========

Basic Functionality
###################

Currently, the mathematical operations "+", "-", "*", and
"/" are supported with arbitrary nesting of parenthesised
expressions. All calculations are performed in double 
precision. In addition to the standard operations above,
the following functions are currently supported:

- *sqrt* : square root
- *a^n* : exponentiation (n can be arbitrary double) 
- *exp* : exponential function
- *ln, log2, log10* : natural, base2, and base10 logarithm
- *cos, sin, tan, acos, asin, atan*: trigonometric functions and inverse
- *cosh, sinh, tanh, acosh, asinh, atanh*: hyperbolic trigonometric functions and inverse
- *fact n*: factorial function. NOTE: *n* has to be an integer or be convertible to an integer type, i.e., *fact 2* and *fact 2.0* are fine but *fact 2.1* is not.


Variables
#########

Users can define any number of variables via

  *variable name* = value

where variable name can be any combination of alphanumeric
characters but has to begin with a letter. Hence, *foobar1*
is fine, but *1foobar* is not. Defined variables can be
used in expressions and definition of other variables. Users
can list all currently defined variables via \\v (see 
`Command Shortcuts`_)

Since husky uses the GNU readline library all readline
related functionality is available at husky's interactive
prompt (including command history). See [1]_ for more 
detail.


Functions
#########

Users can define their own custom functions via the syntax

  *function <function name> <list of variables> = <expression>*

Here, the list of variables can either be a comma separated list
of variable names enclosed in parentheses or a simple list of
variables separated by whitespace without parentheses like in
Haskell. The following expressions are valid and equivalent
function definitions

::
  
  f(x,y) = x * y
  f x y  = x * x

Several restrictions currently apply to function definitions:

- *<expression>* can only span a single line and will be parsed
  until the end of the line.
- *<expression>* has to be a single expression, i.e., it can **not**
  contain a list of semicolon separated sub-expressions.

Functions which have been defined can then be called according to
the same conventions used for function definitions. Hence, function
f as defined above can be called via

  f(3,2) or f 3 2

Here, the calling method does not depend on the way the function 
was defined, i.e., a function could be defined the Haskell way and
then be called via f(x,y). The function arguments can either be
literals or constants that have been defined previously. Hence,
the following husky session is valid

::

  a = 3
  b = 4
  function f x y = x * y
  f(a,b)

will yield the value "12.0".

Users can list all currently defined function via \\f (see
`Command Shortcuts`_).



Unit Converter
==============

The unit conversion functionality of husky can be used via the 
command

   \\c[onvert] *<unit value>* *<from unit>* *<to unit>* [ :: *<unit type>*]

Here, we convert *<unit value>* in units of *<from unit>* to the 
target unit *<to unit>*. In addition, the user may further specify 
the unit type (e.g. Length, Time, ...) to disambiguate a unit 
conversion request. The space between *<unit value>* and *<from unit>*
is optional. E.g.::

   \c 1m yd
   \c 1 m yd
   \c 1 m yd :: Length

will all convert 1 meter into yards. Please type:: 

  \h[elp] units

for a list of all unit conversions.

   
Command Shortcuts
-----------------

The following commands are available at the command prompt:

- \\q       : quit husky
- \\v       : list all currently defined variables
- \\f       : list all currently defined functions
- \\t       : current time
- \\h[elp]  : available help


COPYRIGHT and LICENSE
---------------------

\(C\) 2008-2009, Markus Dittrich

This program is free software; you can redistribute it 
and/or modify it under the terms of the GNU General Public 
License Version 3 as published by the Free Software Foundation. 
 
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License Version 3 for more details.


BUGS
----

Please report all bugs to <haskelladdict@gmail.com>. Thank you!


References
----------

.. [1] The GNU readline library 
   http://tiswww.case.edu/php/chet/readline/rltop.html 
