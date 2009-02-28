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

-- | this module provides the functionality needed to do unit
-- conversions
module UnitConverter ( convert_unit ) where
                      

-- imports
import qualified Data.Map as M


-- | function in charge of the main unit conversion
-- There are possible paths:
-- 1) The user did not supply a unit type specifier. In this case
--    we look through all available unit maps for a matching
--    conversion routine. If we find more than one we'll abort
--    with a hopefully useful error message.
-- 2) If a user supplies a unit type specifier we directly look
--    through the corresponding map for a conversion
convert_unit :: String -> String -> Maybe String -> Double 
             -> Either String Double 
convert_unit unit1 unit2 unitType value = 
    
  case unitType of
     -- no unit type specifier: look through all unit maps
    Nothing -> case unit_lookup (unit1 ++ unit2) allConv of
                 []        -> Left unit_conv_error 
                 a@(x:xs)  -> case length a of
                                1 -> Right (converter x $ value)
                                _ -> Left too_many_matches
      
    -- the user supplied a unit type: grab the proper map and look
    Just unit -> case M.lookup unit allConv of
                   Nothing -> Left $ no_unit_error unit
                   Just a  -> case M.lookup (unit1 ++ unit2) a of
                                Nothing -> Left unit_conv_error
                                Just x  -> Right (converter x $ value)
                   
  where
    -- unit conversion errors
    unit_conv_error  = "No unit conversion known for " 
                       ++ unit1 ++ " to " ++ unit2 ++ "!"

    no_unit_error a  = "Don't know unit " ++ a ++ "!"

    too_many_matches = "More than one unit conversion matched.\n"
                       ++ "Consider disambiguating with an explicit "
                       ++ "unit type."


-- | helper function looking through all unit maps for a matching
-- conversion routine
unit_lookup :: String -> M.Map String UnitMap -> [UnitConverter]
unit_lookup key = M.fold append_val [] 
   where
     append_val entry acc = case M.lookup key entry of
                              Nothing -> acc
                              Just a  -> a:acc


-- | UnitConverter holds all information known about 
-- a particular unit conversion 
data UnitConverter = 
    UnitConverter 
    { converter   :: (Double -> Double)  -- actual conversion fctn
    , description :: String              -- short description
    }


-- | unitMap holds all available conversions for a particular
-- unit type
type UnitMap = M.Map String UnitConverter


-- | allConv holds a map of all available unit conversions
-- indexed by the unit type such as Temp, Length, ....
allConv :: M.Map String UnitMap
allConv = M.fromList [ ("Temp", tempConv) ]


-- | temperature conversions

-- | data structure holding temparature conversion units
tempConv :: UnitMap
tempConv = M.fromList [ ("FC", fc_conv) ]


-- | convert Celcius into Fahrenheit
fc_conv = UnitConverter 
          { converter   = \x -> (5/9)*(x-32)
          , description = "Fahrenheit to Celsius"
          }



