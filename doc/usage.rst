============================================================
husky command line calculator
============================================================

:Author: Markus Dittrich

:Version: 0.1 (02/18/2008)


Introduction
------------

husky is a command line calculator with a small memory
footprint. It can be used in a fashion similar to the
interactive shells of python, octave, or ghci.

Currently, the mathematical operations "+", "-", "*", and
"/" are supported with arbitrary nesting of parenthesised
expressions. All calculations are performed in double 
precision. In addition to the standard operations above,
the following functions are currently supported:

- sqrt          : square root
- a^n           : exponentiation (n can be arbitrary double) 
- exp           : exponential function
- cos, sin, tan : trigonometric functions

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

You can exit a husky session anytime by typing :q at
the command prompt.


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

Please report all bugs to <markusle@gmail.com>. Thank you!


References
----------

.. [1] The GNU readline library 
   http://tiswww.case.edu/php/chet/readline/rltop.html 
