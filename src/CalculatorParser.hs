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
module CalculatorParser ( calculator ) where


-- imports
import qualified Data.Map as M
import Control.Monad

-- local imports
import CalculatorState
import TokenParser

import Debug.Trace


-- | main parser entry point
calculator :: CharParser CalcState (Maybe Double, CalcState)
calculator = parse_calc 
             >>= \val -> getState
             >>= \state -> return (val, state)


-- | grammar description for parser
-- NOTE: We have to make sure to provide a default
--       catch all parsing rule in case the user
--       enters something we don't understand. Otherwise
--       parsing fails and we loose our state.
parse_calc :: CharParser CalcState (Maybe Double)
parse_calc =  try (add_term >>= \x -> return (Just x))
          <|> define_variable
          <|> return Nothing
          <?> "math expression, variable definition " ++
              "or variable name"


-- | if the line starts off with a string we either
-- have a variable definition or want to show the value
-- stored in a variable
define_variable :: CharParser CalcState (Maybe Double)
define_variable = (spaces
                 >> variable
                 >>= \varName -> variable_def varName 
                             <|> show_variable varName )
              <?> "variable definition"


show_variable :: String -> CharParser CalcState (Maybe Double)
show_variable varName = (spaces 
                         >> getState
                         >>= \state -> 
                             let value = get_variable varName state in
                               return value )
                     <?> "variable"


variable_def :: String -> CharParser CalcState (Maybe Double)
variable_def varName = ( spaces
                >> reservedOp "=" 
                >> spaces 
                >> parse_number 
                >>= \value -> 
                    updateState (insert_variable value varName)
                >> return (Just value) )
            <?> "variable"



parse_variable :: CharParser CalcState Double
parse_variable = (variable 
                  >>= \varName -> getState 
                  >>= \state -> 
                      case get_variable varName state of
                        Just a  -> return a
                        Nothing -> pzero )
              <?> "variable"
                  

-- | parser for expressions chained via "+" or "-"
add_term :: CharParser CalcState Double
add_term = mul_term `chainl1` add_action


-- | parser for expressions chained via "*" or "/"
mul_term :: CharParser CalcState Double
mul_term = exp_term `chainl1` multiply_action


-- | parser for potentiation operations "^"
exp_term :: CharParser CalcState Double
exp_term = factor `chainl1` exp_action


-- | parser for individual factors, i.e, numbers,
-- variables or operations
factor :: CharParser CalcState Double
factor = parens add_term
      <|> parse_operations
      <|> parse_number
      <|> parse_variable
      <?> "token or variable"         


-- | parse all operations we currently know about
parse_operations :: CharParser CalcState Double
parse_operations = msum $ extract_ops operations

    where
      extract_ops = foldr (\(x,y) acc -> 
                           ((reserved x >> execute y):acc)) [] 


-- | execute the requested operator on the term enclosed
-- in parentheses       
execute :: OperatorAction -> CharParser CalcState Double
execute op = parens add_term >>= return . op

          
multiply_action :: CharParser CalcState (Double -> Double -> Double)
multiply_action = (reservedOp "*" >> return (*))
                  <|> (reservedOp "/" >> return (/))

add_action :: CharParser CalcState (Double -> Double -> Double)
add_action = (reservedOp "+" >> return (+))
             <|> (reservedOp "-" >> return (-))

exp_action :: CharParser CalcState (Double -> Double -> Double)
exp_action = reservedOp "^" >> return real_exp

parse_number :: CharParser CalcState Double
parse_number = naturalOrFloat >>= 
               \num -> case num of 
                 Left i  -> return $ fromInteger i
                 Right x -> return x


-- | function retrieving a variable from the database if
-- present 
get_variable :: String -> CalcState -> Maybe Double
get_variable name (CalcState { varMap = theMap }) =
    M.lookup name theMap 
    
