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
import Text.ParserCombinators.Parsec 
import qualified Text.ParserCombinators.Parsec.Token as PT
import Text.ParserCombinators.Parsec.Language (haskellDef
                                              , reservedOpNames
                                              , reservedNames )


-- | main
main :: IO ()
main = do

  -- get a line from stdin
  input <- getLine

  -- parse it
  case parse parse_calc "" input of
    Left er  -> putStrLn $ "Error: " ++ (show er)
    Right cl -> case cl of
                  Nothing  -> return ()
                  Just val -> putStrLn (show val)

  main


-- | function generating a token parser based on a 
-- lexical parsers combined with a language record definition
lexer :: PT.TokenParser st
lexer  = PT.makeTokenParser 
         ( haskellDef { reservedOpNames = ["*","/","+","-","="]
                      , reservedNames   = ["sqrt"] } )


-- | token parser for parenthesis
parens :: CharParser st a -> CharParser st a
parens = PT.parens lexer


-- | token parser for Integer
integer :: CharParser st Integer
integer = PT.integer lexer


-- | token parser for Char
stringLiteral :: CharParser st String
stringLiteral = PT.stringLiteral lexer


-- | token parser for Double
float :: CharParser st Double
float = PT.float lexer


-- | token parser for Either Integer Double
naturalOrFloat :: CharParser st (Either Integer Double)
naturalOrFloat = PT.naturalOrFloat lexer


-- | token parser for keywords
reservedOp :: String -> CharParser st ()
reservedOp = PT.reservedOp lexer


-- | token parser for keywords
reserved :: String -> CharParser st ()
reserved = PT.reserved lexer


-- | helper function for defining real powers
real_exp :: Double -> Double -> Double
real_exp a x = exp $ x * log a



-- | grammar description for parser
parse_calc :: CharParser () (Maybe Double)
parse_calc =  (add_term >>= \x -> return (Just x))
              <|> (variable_def >> return Nothing)

variable_def :: CharParser () ()
variable_def = (many letter >> spaces >> reservedOp "=" 
                >> spaces >> parse_number >> return ())
               <?> "variable"

add_term :: CharParser () Double
add_term = mul_term `chainl1` add_action

mul_term :: CharParser () Double
mul_term = exp_term `chainl1` multiply_action

exp_term :: CharParser () Double
exp_term = factor `chainl1` exp_action

factor :: CharParser () Double
factor = parens add_term
         <|> parse_sqr
         <|> parse_number

parse_sqr :: CharParser () Double
parse_sqr = reserved "sqrt" >> parens add_term >>= 
            \x -> return $ sqrt x 
          
multiply_action :: CharParser () (Double -> Double -> Double)
multiply_action = (reservedOp "*" >> return (*))
                  <|> (reservedOp "/" >> return (/))

add_action :: CharParser () (Double -> Double -> Double)
add_action = (reservedOp "+" >> return (+))
             <|> (reservedOp "-" >> return (-))

exp_action :: CharParser () (Double -> Double -> Double)
exp_action = reservedOp "^" >> return real_exp

parse_number :: CharParser () Double
parse_number = naturalOrFloat >>= 
               \num -> case num of 
                 Left i  -> return $ fromInteger i
                 Right x -> return x


