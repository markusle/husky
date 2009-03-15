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

-- | this module contains small bits and pieces needed for 
-- the calculator. Eventually, these might all find a home
-- in their separate modules
module CalculatorState ( CalcState(..)
                       , defaultCalcState 
                       , insert_variable
                       ) where


-- imports
import qualified Data.Map as M
import Prelude


-- | this data structure provides some state information
-- to the calculator (variables, etc ...)
-- Currently, we thread the following pieces of information:
-- varMap   : map with all currently defined variable/value pairs
--            (persistent across calls to reset_state below)
-- errState : bool indicating that any special error messages
--            have been queued
-- errValue : [String] holding all special error messages
data CalcState = CalcState 
    { 
      varMap   :: M.Map String Double   
    }


defaultCalcState :: CalcState
defaultCalcState = CalcState 
    { 
      varMap   = M.fromList constantList 
    }


-- | function adding a new variable to the database
insert_variable :: Double -> String -> CalcState -> CalcState
insert_variable num name state@(CalcState { varMap = theMap }) =
    state { varMap = M.insert name num theMap } 


-- | provide a few useful mathematical constants that we
-- load into the default CalculatorState
constantList :: [(String,Double)]
constantList = 
    [ ("pi",3.14159265358979323846264338327950288)
    , ("e",2.71828182845904523536028747135266249)
    , ("phi",1.61803398874989484820458683436563811)]  -- golden ratio 

