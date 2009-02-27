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
convert_unit :: String -> String -> Maybe String -> Double 
             -> Maybe Double 
convert_unit unit1 unit2 unitType value = 
    
    case unitType of
      Nothing -> case M.lookup (unit1 ++ unit2) tempConv of
                     Nothing -> Nothing
                     Just a  -> Just (converter a $ value)
      
      Just unit -> Nothing




-- | data struct holding the neccesary bits for a conversion
-- operation
data UnitConverter = 
    UnitConverter 
    { converter   :: (Double -> Double)  -- actual conversion fctn
    , description :: String              -- short description
    }



-- | temperature conversions

-- | data structure holding temparature conversion units
tempConv :: M.Map String UnitConverter
tempConv = M.fromList [ ("FC", fc_conv) ]


-- | convert Celcius into Fahrenheot
fc_conv = UnitConverter 
          { converter   = \x -> (5/9)*(x-32)
          , description = "Fahrenheit to Celsius"
          }



