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
module InfoRoutines ( list_variables 
                    , show_time
                    ) where


-- imports
import Data.Map
import Data.Time
import Prelude
import System.Locale


-- local imports
import CalculatorState

 
-- | list all currently defined variables
list_variables :: CalcState -> IO ()
list_variables (CalcState { varMap = theMap }) = 
  mapM_ print_variable (assocs theMap) 

    where
      print_variable x = putStrLn (fst x ++ " == " ++ (show $ snd x)) 


-- | display the current localtime
show_time :: IO ()
show_time = getCurrentTime
            >>= \utcTime -> getTimeZone utcTime 
            >>= \zone -> 
                let 
                    localTime  = utcToLocalTime zone utcTime 
                    timeString = formatTime defaultTimeLocale 
                                 "%a %b %m %Y  <>  %T %Z " localTime 
                in
                  putStrLn timeString

