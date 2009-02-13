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


-- local imports
import CalculatorParser
import CalculatorState
import Messages
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
  husky_prompt
  input <- getLine

  -- parse it
  case runParser calculator state "" input of
    Left er  -> putStrLn $ "Error: " ++ (show er)
    Right (result, newState) -> 
        case result of
          Nothing  -> parse_error input
          Just val -> husky_result >> putStrLn (show val)

        >> parse_it newState

