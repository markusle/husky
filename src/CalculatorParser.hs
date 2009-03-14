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
import Prelude
import TokenParser


-- | grammar description for calculator parser
calculator_parser :: CharParser CalcState (Double, String)
calculator_parser = (,) <$> parse_statements <* eof 
                        <*> pure "" 
                 <?> "math expression, variable definition, " 
                     ++ "variable name"


-- | if the line starts off with a string we either
-- have a variable definition or want to show the value
-- stored in a variable
define_variable :: CharParser CalcState Double
define_variable = variable_def_by_value 
               <?> "variable definition"



-- | define a variable via a literal double
variable_def_by_value :: CharParser CalcState Double
variable_def_by_value = update_var (whiteSpace *> variable) 
   (whiteSpace *> reservedOp "=" *> whiteSpace *> add_term)
                     <?> "variable from value"


-- | update the state of a variable
update_var :: CharParser CalcState String 
           -> CharParser CalcState Double
           -> CharParser CalcState Double
update_var name_p val_p = name_p
       >>= \name -> val_p
       >>= \val  -> updateState (insert_variable val name)
       >> return val


-- | parse individual statements separated by semicolon
-- NOTE: 'sepBy1' as opposed to 'sepBy' is crucial here to
-- guarantee the list is not empty; otherwise head will die
-- on us.
parse_statements :: CharParser CalcState Double
parse_statements = (head . reverse) <$> individual_statement 
                   `sepBy1` semi
                <?> "statement"
             

-- | parse an individual statement, i.e. either a computation
-- or a variable definition
individual_statement :: CharParser CalcState Double
individual_statement = try define_variable <|> add_term 
                    <?> "expression or variable definition"


-- | parser for expressions chained via "+" or "-"
add_term :: CharParser CalcState Double
add_term = mul_term `chainl1` add_action


-- | parser for expressions chained via "*" or "/"
mul_term :: CharParser CalcState Double
mul_term = exp_term `chainl1` multiply_action


-- | parser for potentiation operations "^"
exp_term :: CharParser CalcState Double
exp_term = (whiteSpace *> factor) `chainl1` exp_action


-- | parser for individual factors, i.e, numbers,
-- variables or operations
factor :: CharParser CalcState Double
factor = try signed_parenthesis
      <|> parse_keywords
      <|> parse_number
      <|> parse_variable
      <?> "token or variable"         


-- | parse a potentially signed expression enclosed in parenthesis.
-- In the case of parenzised expressions we parse -() as (-1.0)*()
signed_parenthesis :: CharParser CalcState Double
signed_parenthesis = (*) <$> parse_sign <*> parens add_term


-- | parse all operations we currently know about
parse_keywords :: CharParser CalcState Double
parse_keywords = msum $ extract_ops builtinFunctions

    where
      extract_ops = foldr (\(x,y) acc -> 
                           ((reserved x >> execute y):acc)) [] 


-- | execute the requested operator on the term enclosed
-- in parentheses       
execute :: OperatorAction -> CharParser CalcState Double
execute op = op <$> parens add_term 

          
multiply_action :: CharParser CalcState (Double -> Double -> Double)
multiply_action = (reservedOp "*" *> pure (*))
               <|> (reservedOp "/" *> pure (/))


add_action :: CharParser CalcState (Double -> Double -> Double)
add_action = (reservedOp "+" *> pure (+))
          <|> (reservedOp "-" *> pure (-))


exp_action :: CharParser CalcState (Double -> Double -> Double)
exp_action = reservedOp "^" *> pure real_exp


-- | parse a number; integers are automatically promoted to double
parse_number :: CharParser CalcState Double
parse_number = converter <$> parse_sign <*> 
               (naturalOrFloat <* notFollowedBy alphaNum)
    where 
      converter sign val = case val of
                             Left i  -> sign * (fromInteger i)
                             Right x -> sign * x


-- | parse the sign of a numerical expression
parse_sign :: CharParser CalcState Double
parse_sign = option 1.0 ( whiteSpace *> char '-' *> pure (-1.0) )


-- | look for the value of a given variable if any
parse_variable :: CharParser CalcState Double
parse_variable = get_variable_value variable <* whiteSpace
              <?> "variable"


-- | function retrieving a variable from the database if
-- present 
get_variable_value :: CharParser CalcState String 
                   -> CharParser CalcState Double
get_variable_value name_parser = getState 
  >>= \(CalcState { varMap = myMap }) -> name_parser
  >>= \name -> case M.lookup name myMap of
                 Nothing -> pzero
                 Just a  -> return a
                            


