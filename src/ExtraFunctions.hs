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
module ExtraFunctions ( erf
                      , erfc
                      , fact 
                      , is_equal
                      , is_equal_with
                      , real_exp 
                      , to_int
                      , to_positive_int
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


-- | comparison function for doubles via threshold
is_equal_with :: Double -> Double -> Double -> Bool
is_equal_with x y th = abs(x-y) <= abs(x) * th


-- | function checking if a Double can be interpreted as a non
-- negative Integer. We need this since all parsing of numbers 
-- is done with Doubles but some functions only work for 
-- non-negative integers such as factorial.
-- To check if we are dealing with Double, we convert to an
-- Integer via floor and the compare if the numbers are identical.
-- If yes, the number seems to be an Integer and we return it,
-- otherwise Nothing
to_positive_int :: Double -> Maybe Integer
to_positive_int x = 
  case (is_equal (fromInteger . floor $ x) x) && (x > 0.0) of
    True  -> Just $ floor x
    False -> Nothing


-- | function checking if a Double can be interpreted as an
-- Integer. See is_positive_int for more detail
to_int :: Double -> Maybe Integer
to_int x = 
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


-- | error function erf(x)
-- we use a recursive solution of the Taylor series expansion
erf :: Double -> Double
erf x 
  | x == 0.0     = 0.0            -- our recursive alg. loops forever
                                  -- in this case
  | abs(x) > 2.2 = 1.0 - erfc x   -- use erfc for numerical accuracy
  | otherwise    = 2.0 / sqrt(pi) * (erf_h x x 1.0)

  where
    x_next n = -(x^2) * (2*n-1)/(n * (2*n+1))

    erf_h old x_old n = let x_new = x_old * (x_next n)
                            tot   = old + x_new
                        in
                          if abs(x_new/tot) < dbl_epsilon
                          then tot
                          else erf_h tot x_new (n+1)


-- | complementary error function erfc(x) = 1 - erf(x)
-- we use a recursive solution of the continued fraction
-- expression of erfc(x) for it superior convergence 
-- property. Here, we calculate the ith and (i+1)th convergent, (see
-- http://archives.math.utk.edu/articles/atuyl/confrac/intro.html)
-- and terminate when the relative difference is smaller than a
-- certain threshold. 
erfc :: Double -> Double
erfc x 
  | abs(x) < 2.2  = 1.0 - erf(x)   -- use erf(x) in [-2.2,2.2]
  | signum(x) < 0 = 2.0 - erfc(-x) -- continued fraction expansion
                                   -- only valid for x > 0
  | otherwise     = 1/sqrt(pi) * exp(-x^2) 
                    * (erfc_h nc1 nc2 dc1 dc2 1.0)

  where
    nc1 = 1.0     :: Double -- numerator of 1st convergent
    nc2 = x       :: Double -- numerator of 2nd convergent
    dc1 = x       :: Double -- denominator of 1st convergent
    dc2 = x^2+0.5 :: Double -- denominator of 2nd convergent

    erfc_h n1 n2 d1 d2 i = 
      let num_new   = n1*i + n2*x
          denom_new = d1*i + d2*x
          d_old     = n2/d2
          d_new     = num_new/denom_new
      in
        if abs((d_old - d_new)/d_new) < dbl_epsilon
        then d_new
        else erfc_h n2 num_new d2 denom_new (i+0.5)

