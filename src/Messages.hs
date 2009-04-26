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
                , infoString
                , print_error_message
                , show_greeting
                ) where

-- imports
import System.IO()
import Prelude 


-- local imports
import PrettyPrint
import ErrorParser


-- | current version
version :: String
version = "0.5"

-- | info string
infoString :: String
infoString = "Welcome to husky (v" ++ version ++ ")\n" 
          ++ "(C) 2009 Markus Dittrich, licensed under the GPL-3\n"
          ++ "husky comes WITHOUT ANY WARRANTY\n"


-- | display output somewhat colorful
husky_result :: [String] -> IO ()
husky_result items = do
  putStr $ color_string Yellow "=> "
  putStrLn $ unwords items


-- | greeting                                                         
show_greeting :: IO ()                                                
show_greeting = putStrLn $ banner ++ "\n" ++ infoString ++ banner
  where
    banner = replicate 60 '*'


-- | helpful message after bad parse
print_error_message :: String -> String -> IO ()
print_error_message err line = highlight_error
                               >>= putStrLn 
                               >> putStr (parse_error err)
                         
  where
    parse_error = unlines . tail . lines 

    -- | highlight the position where the error occurs
    -- if we can't determine it we simply echo the input line
    highlight_error = extract_error_position err >>= \p -> 
      case p of
        Nothing  -> return line
        Just pos -> return $ highlight_error_h (fromIntegral pos) line
      
      where
        highlight_error_h p l = 
              (color_string Yellow $ "parse error:\n\n") 
              ++ ">> " ++ l ++ "\n" ++ ptr
          
          where          
            ptr = (replicate (p-1+3) ' ') ++ "^" 
