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

-- | main archy driver
module HelpParser ( help ) where


-- imports
import Control.Monad

-- local imports
import CalculatorState
import Prelude
import PrettyPrint
import TokenParser
import UnitConverter
import UnitConversionParser


-- | main help parser entry point
help :: CharParser CalcState String
help = ( help_keyword 
       >> optionMaybe parse_help_option 
       >>= \opt -> case opt of
                     Nothing -> return help_info
                     Just r  -> return r )
    <?> "help"



-- | parser for help keyword
-- we accept "\help" as well as the short form "\h"
help_keyword :: CharParser CalcState ()
help_keyword =  reserved "\\help"
            <|> reserved "\\h"
            <?> "\\help or \\h"


-- | parser for the particular help option requested
-- without it we print what kind of help is available
parse_help_option :: CharParser CalcState String
parse_help_option = msum . map snd $ helpOptions


-- | retrieve unit conversion information
unit_info :: CharParser CalcState String
unit_info = ( string "units"
            >> optionMaybe parse_unit_type
            >>= \unitType -> return $ retrieve_unit_string unitType)
         <?> "unit info"


-- | return about info
about_info :: CharParser CalcState String
about_info = string "about"
             >> return about_string
          <?> "about info"
  where
    about_string = "husky (v0.3) (C) 2009 Markus Dittrich\n"
                   ++ "husky is licenced under the GPL V3\n"


-- | return all currently available help options
help_info :: String
help_info = (color_string Yellow $ "Available help options"
             ++ " (request via \\[h]elp <option>):\n") 
             ++ help_string
  where
    help_string = unlines . map fst $ helpOptions


-- | data type holding all currently supported help keywords
-- together with a parser for that particular help option
helpOptions :: [(String, CharParser CalcState String)]
helpOptions = [ ("about                  - about husky", about_info)
              , ("units [:: <unit type>] - list available unit "
                 ++ "conversions", unit_info) 
              ]
