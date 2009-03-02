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

-- | parser for unit conversions
module UnitConversionParser ( unit_conversion ) where


-- local imports
import CalculatorState
import TokenParser
import UnitConverter


-- | parser for unit conversions 
-- the user can request a conversion between two compatible
-- unit-full values (temperatures, lengths, ...).
-- The command spec is 
--     conv <value in unit1> <unit1> <unit2> [ :: <unit type> ] 
-- and returns <value in unit2>
unit_conversion :: CharParser CalcState (Double,String)
unit_conversion = (whiteSpace
                  >> conversion_keyword
                  >> whiteSpace
                  >> parse_unit_value
                  >>= \value -> whiteSpace
                  >> unit_value
                  >>= \unit1 -> whiteSpace
                  >> unit_value
                  >>= \unit2 -> whiteSpace
                  >> optionMaybe parse_unit_type 
                  >>= \unitType ->
                    case convert_unit unit1 unit2 unitType value of
                      Left err           -> add_error_message err 
                                             >> return (0,"") 
                      Right (conv, unit) -> return (conv,unit) )
               <?> "unit conversion"
 

-- | parse a unit value
-- We can't use parse_number since we'd like to explictly allow
-- things like 1m or 2yd which parse_number rejects
parse_unit_value :: CharParser CalcState Double
parse_unit_value = naturalOrFloat 
                   >>= \num -> case num of 
                                Left i  -> return $ fromInteger i
                                Right d -> return d          


-- | parse for all acceptable conversion keywords
conversion_keyword :: CharParser CalcState ()
conversion_keyword = reserved "\\c" 
                  <|> reserved "\\convert"
                  <?> "(c)onv keyword"


-- | this function adds an error message to the queue of
-- special (outside of parsing errors) to the error
-- queue
add_error_message :: String -> CharParser CalcState ()
add_error_message = updateState . insert_error


-- | this parser parses an (optional) unit type signature following 
-- a unit conversion statement. It should be of the form 
-- (a la Haskell ;) ) " :: unit_type "
parse_unit_type :: CharParser CalcState String
parse_unit_type = (whiteSpace
                  >> string "::"
                  >> whiteSpace
                  >> unit_type )
               <?> "unit_type"

