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
--import Data.Map
import System.Console.Readline

-- local imports
import CalculatorParser
import CalculatorState
import Messages
import PrettyPrint
import TokenParser
import InfoRoutines


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
    Just "\\q" -> return ()             -- quit
    Just "\\v" -> list_variables state  -- list all defined variables
                  >> parse_it state
    Just line -> do

      addHistory line

      -- parse it
      case runParser calculator state "" line of
        Left er  -> putStrLn $ "Error: " ++ (show er)
        Right (result, newState) -> 
          case result of
            Nothing  -> parse_error line
            Just val -> husky_result >> putStrLn (show val)

          >> parse_it newState
