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

-- | Messages provides common messages
module Messages ( husky_result
                , parse_error
                , show_greeting
                ) where

-- imports
import System.IO

-- local imports
import PrettyPrint


-- | display output somewhat colorful
husky_result :: IO ()
husky_result = do
  putStr $ color_string Yellow "=> "


-- | greeting                                                         
show_greeting :: IO ()                                                
show_greeting = do                                                    
  putStrLn "Welcome to husky (v0.0)  (C) 2009 Markus Dittrich"
  putStrLn "-------------------------------------------------"


-- | message indicating that something went wrong
-- during parsing
parse_error :: String -> IO ()
parse_error message = do
  putStr   $ color_string Yellow "Error: "
  putStrLn $ color_string Green "Could not parse '" ++ message ++ "'"
