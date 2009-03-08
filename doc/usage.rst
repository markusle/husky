============================================================
husky command line calculator
============================================================

:Author: Markus Dittrich

:Version: 0.3 (03/08/2008)


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


Furthermore, users can define any number of variables via

  *variable name* = value

where variable name can be any combination of alphanumeric
characters but has to begin with a letter. Hence, *foobar1*
is fine, but *1foobar* is not. Defined variables can be
used in expressions and definition of other variables.

Since husky uses the GNU readline library all readline
related functionality is available at husky's interactive
prompt (including command history). See [1]_ for more 
detail.

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
