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
module Main where


-- imports
import System.Console.Readline

-- local imports
import Parser
import CalculatorState
import HelpParser
import InfoRoutines
import Messages
import Prelude
import PrettyPrint
import TokenParser


-- | main
main :: IO ()
main = do
  show_greeting
  parse_it defaultCalcState


-- | main parse function
parse_it :: CalcState -> IO ()
parse_it state = do

  -- prompt and get a line from stdin
  input <- readline $ color_string Red "husky> "
  case input of 
    Nothing   -> parse_it state

    Just ""    -> parse_it state            -- continue w/o parsing

    Just "\\q" -> confirm_and_exit 
                  >>= \ans -> case ans of   -- quit after confirmation
                    True -> return ()       -- otherwise continue
                    False -> parse_it state

    Just "\\v" -> list_variables state      -- list all defined 
                  >> parse_it state         -- variables

    Just "\\f" -> list_functions state      -- list all defined
                  >> parse_it state         -- functions

    Just "\\t" -> show_time                 -- show current time
                  >> parse_it state

    Just line -> do                         -- otherwise calculate 

      addHistory line

      {- parse it as a potential help request if it succeeds we 
         parse the next command line, otherwise we channel it 
         into the calculator parser -}
      case runParser help state "" line of
        Right helpMsg  -> putStr helpMsg 
                          >> parse_it state   
        Left _         -> 

          -- parse it as a calculation or unit conversion
          case runParser main_parser state "" line of

            Left er  -> print_error_message (show er) 
                        >> parse_it state

            {- if the parser succeeds we do one of the following:
               1) If the return value is a DblResult or UnitResult we 
                  just print it
               2) If the return value is a ErrResult we print the
                  the associated error string -}
            Right (result, newState) -> 
                case result of
                  DblResult d      -> husky_result $ (show d):[""]
                  UnitResult (v,u) -> husky_result $ (show v):[u]
                  ErrResult err    -> (putStrLn $ "Error: " ++ err)
                  StrResult str    -> husky_result $ str:[""]

                 >> parse_it newState

