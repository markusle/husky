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

-- | PrettyPrint provides tools for colored output to the terminal
module PrettyPrint ( putColorStr 
                   , putColorStrLn 
                   , putColorBStr 
                   , putColorBStrLn 
                   , husky_prompt
                   , husky_result
                   ) where

-- imports
import qualified Data.ByteString as B
import System.Console.ANSI
import System.IO


-- | prints a string in Color to the terminal
putColorStr :: Color -> String -> IO ()
putColorStr color text = 
  do
    setSGR[Reset]
    setSGR[SetColor Foreground Vivid color]
    putStr text
    setSGR[Reset]
          

-- | prints a string with backspace in Color to the terminal
putColorStrLn :: Color -> String -> IO ()
putColorStrLn color text = 
  do
    setSGR[Reset]
    setSGR[SetColor Foreground Vivid color]
    putStrLn text
    setSGR[Reset]
 
-- | prints a ByteString in Color to the terminal
putColorBStr :: Color -> B.ByteString -> IO ()
putColorBStr color text = 
  do
    setSGR[Reset]
    setSGR[SetColor Foreground Vivid color]
    B.putStr text
    setSGR[Reset]
          

-- | prints a ByteString with backspace in Color to the terminal
putColorBStrLn :: Color -> B.ByteString -> IO ()
putColorBStrLn color text = 
  do
    setSGR[Reset]
    setSGR[SetColor Foreground Vivid color]
    B.putStrLn text
    setSGR[Reset]


-- | how the prompts look like
husky_prompt :: IO ()
husky_prompt = do
  putColorStr Red $ "husky> "
  hFlush stdout

husky_result :: IO ()
husky_result = do
  putColorStr Yellow $ "=> "
