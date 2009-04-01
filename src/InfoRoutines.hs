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

-- | routines called from the toplevel readline instance without
-- before any parsing is done, aka info routines of any sort and
-- shape
module InfoRoutines ( confirm_and_exit
                    , list_functions
                    , list_variables 
                    , show_time
                    ) where


-- imports
import Data.List
import Data.Map
import Data.Time
import Prelude
import System.IO
import System.Locale


-- local imports
import CalculatorState

 
-- | list all currently defined variables
list_variables :: CalcState -> IO ()
list_variables (CalcState {varMap = theMap}) = 
  mapM_ print_variable (assocs theMap) 

    where
      print_variable (x,y) = putStrLn (x ++ " == " ++ (show y)) 


-- | list all currently defined functions
list_functions :: CalcState -> IO ()
list_functions (CalcState {funcMap = theMap} ) = 
  mapM_ print_function (assocs theMap)

    where
      print_function ( x
                     , (Function { f_vars = vars 
                                 , f_expression = expr } 
                       )
                     ) =
        putStrLn (x ++ "(" ++ intercalate [','] vars ++ ") = " ++ expr)


-- | display the current localtime
show_time :: IO ()
show_time = getCurrentTime
            >>= \utcTime -> getTimeZone utcTime 
            >>= \zone -> 
                let 
                    localTime  = utcToLocalTime zone utcTime 
                    timeString = formatTime defaultTimeLocale 
                                 "%a %b %d %Y  <>  %T %Z " localTime 
                in
                  putStrLn timeString


-- | ask user for confirmation before exiting
confirm_and_exit :: IO Bool
confirm_and_exit = putStr "Really quit (y/n)? "
                   >> hFlush stdout
                   >> getLine
                   >>= \answer -> case answer of 
                                   "y" -> return True
                                   "n" -> return False
                                   _   -> confirm_and_exit
