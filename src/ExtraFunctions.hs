{-# LANGUAGE ForeignFunctionInterface #-}
{-----------------------------------------------------------------
 
  (c) 2008-2009 Markus Dittrich 
 
  This program is free software; you can redistribute it 
  and/or modify it under the terms of the GNU General Public 
  License Version 3 as published by the Free Software Foundation. 
 
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License Version 3 for more details.
 
  You should have received a copy of the GNU General Public 
  License along with this program; if not, write to the Free 
  Software Foundation, Inc., 59 Temple Place - Suite 330, 
  Boston, MA 02111-1307, USA.

--------------------------------------------------------------------}

-- | definition of additional math and helper functions
module ExtraFunctions ( fact 
                      , is_equal
                      , is_non_negative_int
                      , real_exp 
                      ) where


-- imports
import Foreign()
import Foreign.C.Types
import Prelude 


-- | use glibc DBL_EPSILON
dbl_epsilon :: Double
dbl_epsilon = 2.2204460492503131e-16


-- | comparison function for doubles via dbl_epsion
is_equal :: Double -> Double -> Bool
is_equal x y = abs(x-y) <= abs(x) * dbl_epsilon


-- | function checking if a Double can be interpreted as a non
-- negative Integer. We need this since all parsing of numbers 
-- is done with Doubles but some functions only work for 
-- non-negative integers such as factorial.
-- To check if we are dealing with Double, we convert to an
-- Integer via floor and the compare if the numbers are identical.
-- If yes, the number seems to be an Integer and we return it,
-- otherwise Nothing
is_non_negative_int :: Double -> Maybe Integer
is_non_negative_int x = 
  case is_equal (fromInteger . floor $ x) x of
    True  -> Just $ floor x
    False -> Nothing


-- | helper function for defining real powers
-- NOTE: We use glibc's pow function since it is more
-- precise than implementing it ourselves via, e.g.,
-- pow a x = exp $ x * log a
foreign import ccall "math.h pow"
        c_pow :: CDouble -> CDouble -> CDouble

real_exp :: Double -> Double -> Double 
real_exp a x = realToFrac $ c_pow (realToFrac a) (realToFrac x)


-- | factorial function
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)
