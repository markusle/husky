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
module CalculatorParser ( calculator_parser ) where


-- imports
import qualified Data.Map as M
import Control.Monad

-- local imports
import CalculatorState
import ExtraFunctions
import TokenParser


-- | grammar description for calculator parser
calculator_parser :: CharParser CalcState (Double, String)
calculator_parser = try ( define_variable >>= \x -> return (x,"") )
          <|> (add_term >>= \x -> end_of_line >> return (x,"") )
          <?> "math expression, variable definition, " ++
              "variable name"


-- | if the line starts off with a string we either
-- have a variable definition or want to show the value
-- stored in a variable
define_variable :: CharParser CalcState Double
define_variable = (whiteSpace
                  >> variable
                  >>= \varName -> variable_def varName )
               <?> "variable definition"


-- | check that we are at the end of the line; otherwise
-- parsing failed since we always expect to parse the 
-- full expression
end_of_line :: CharParser CalcState ()
end_of_line = getInput >>= \input ->
                case length input of
                  0 -> return ()
                  _ -> pzero


-- | define a variable
variable_def :: String -> CharParser CalcState Double
variable_def varName = ( whiteSpace
                >> reservedOp "=" 
                >> whiteSpace 
                >> ( variable_def_by_value varName 
                    <|> variable_def_by_var varName)  )
            <?> "variable"


-- | define a variable via a literal double
variable_def_by_value :: String -> CharParser CalcState Double
variable_def_by_value varName = ( add_term
            >>= \value -> updateState (insert_variable value varName)
            >> return value )
          <?> "variable from value"


-- | define a variable via the value of another variable
variable_def_by_var :: String -> CharParser CalcState Double
variable_def_by_var varName = parse_variable 
            >>= \value -> updateState (insert_variable value varName)
            >> return value
             

-- | look for the value of a given variable if any
parse_variable :: CharParser CalcState Double
parse_variable = ( variable 
                  >>= \val -> whiteSpace
                  >> get_variable_value val
                  >>= \result -> case result of
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
exp_term = (whiteSpace >> factor) `chainl1` exp_action


-- | parser for individual factors, i.e, numbers,
-- variables or operations
factor :: CharParser CalcState Double
factor = parens add_term
      <|> parse_keywords
      <|> parse_number
      <|> parse_variable
      <?> "token or variable"         


-- | parse all operations we currently know about
parse_keywords :: CharParser CalcState Double
parse_keywords = msum $ extract_ops builtinFunctions

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
parse_number = naturalOrFloat 
               >>= \num -> notFollowedBy alphaNum
               >> case num of 
                    Left i  -> return $ fromInteger i
                    Right x -> return x


-- | function retrieving a variable from the database if
-- present 
get_variable_value :: String -> CharParser CalcState (Maybe Double)
get_variable_value name = getState 
  >>= \(CalcState { varMap = myMap }) -> return $ M.lookup name myMap
