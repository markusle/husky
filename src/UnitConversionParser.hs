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
module UnitConversionParser ( unit_conversion 
                            , parse_unit_type
                            ) where


-- local imports
import CalculatorState
import Prelude
import TokenParser
import UnitConverter


-- | an identifier for a unit_value and unit_type 
unit_value :: CharParser CalcState String
unit_value = unit_variable

unit_type :: CharParser CalcState String
unit_type = unit_variable

unit_variable :: CharParser CalcState String
unit_variable = letter 
           >>= \first -> many alphaNum
           >>= \rest  -> return $ [first] ++ rest


-- | parser for unit conversions 
-- the user can request a conversion between two compatible
-- unit-full values (temperatures, lengths, ...).
-- The command spec is 
--     conv <value in unit1> <unit1> <unit2> [ :: <unit type> ] 
-- and returns <value in unit2>
unit_conversion :: CharParser CalcState ParseResult
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
                      Left err           -> return $ ErrResult err
                      Right (conv, unit) -> return $ UnitResult (conv,unit) )
               <?> "unit conversion"
 

-- | parse a unit value
-- We can't use parse_number since we'd like to explictly allow
-- things like 1m or 2yd which parse_number rejects
parse_unit_value :: CharParser CalcState Double
parse_unit_value = converter <$> parse_sign <*> naturalOrFloat 
    where 
      converter sign val = case val of
                             Left i  -> sign * (fromInteger i)
                             Right x -> sign * x


-- | parse the optional sign in front of a unit value
parse_sign :: CharParser CalcState Double
parse_sign = option 1.0 ( whiteSpace *> char '-' *> pure (-1.0) )


-- | parse for all acceptable conversion keywords
conversion_keyword :: CharParser CalcState ()
conversion_keyword = reserved "\\c" 
                  <|> reserved "\\convert"
                  <?> "(c)onv keyword"


-- | this parser parses an (optional) unit type signature following 
-- a unit conversion statement. It should be of the form 
-- (a la Haskell ;) ) " :: unit_type "
parse_unit_type :: CharParser CalcState String
parse_unit_type = whiteSpace *> string "::" *> whiteSpace *> unit_type
               <?> "unit_type"
