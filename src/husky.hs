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


-- global imports
import Data.List

-- local imports
import ApplicativeParsec

-- | main
main :: IO ()
main = do

  -- shell prompt
  putStrLn "husky <> "

  -- get a line from stdin
  input <- getLine

  -- parse it
  case parse parse_cl "" input of
    Left _   -> putStrLn "Error: Unknown action"
    Right cl -> mapM_ (putStr) (intersperse ":" cl)

  main


--data ClString = ClString String | ClStrings [String]

-- | simple command line parser
parse_cl :: CharParser () [String]
parse_cl = spaces *> parse_strings

  where
    parse_strings :: CharParser () [String]
    parse_strings = (many1 letter) `sepBy` spaces  
