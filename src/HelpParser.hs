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
import Messages
import Prelude
import PrettyPrint
import TokenParser
import UnitConverter
import UnitConversionParser


-- | main help parser entry point
help :: CharParser CalcState String
help = eval_request <$> (help_keyword 
                         *> optionMaybe parse_help_option)
    <?> "help"
 where
   eval_request x = case x of
                      Nothing -> help_info
                      Just r  -> r


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
unit_info = retrieve_unit_string <$> (string "units" 
                                      *> optionMaybe parse_unit_type)
         <?> "unit info"


-- | return about info
about_info :: CharParser CalcState String
about_info = string "about"
             >> return infoString
          <?> "about info"


-- | return info about available commands
command_info :: CharParser CalcState String
command_info = string "commands" *> pure commandString
            <?> "command info"


-- | currently available commands
commandString :: String
commandString = (color_string Yellow $ "Available commands:\n")
                ++ "\\q    - quit\n"
                ++ "\\t    - show current date and time\n"
                ++ "\\v    - list currently defined variables\n"
                ++ "\\f    - list currently defined functions\n"


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
              , ("commands               - list commands", command_info)
              , ("conversion [:: <type>] - list available unit "
                 ++ "conversions", unit_info) 
              ]
