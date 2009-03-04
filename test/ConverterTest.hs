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

-- | handcoded test for checking out the conversion function parser
module Main where


-- import
import Control.Monad.Writer
import System.Exit


-- local imports
import Parser
import CalculatorState
import ExtraFunctions
import PrettyPrint
import TokenParser


-- | top level main routine 
-- we use the Writer monad to capture the results for all tests 
-- and then examine the results afterward
main :: IO ()
main = do
  putStrLn "\n\n\nTesting conversion function parser ..."

  putStr $ color_string Cyan "\nSimple tests:\n"
  let simple1 = execWriter $ good_test_driver defaultCalcState 
               simpleTests
  status1 <- examine_output simple1

  putStr $ color_string Cyan "\nFailing tests:\n"
  let simple2 = execWriter $ failing_test_driver defaultCalcState 
               failingTests
  status2 <- examine_output simple2


  let status = status1 && status2
  if status == True then
      exitWith ExitSuccess
    else
      exitWith $ ExitFailure 1
   

-- | helper function for examining the output of a good test run
-- (i.e. one that should succeed), prints out the result for each 
-- test, collects the number of successes/failures and returns 
-- True in case all tests succeeded and False otherwise
examine_output :: [TestResult] -> IO Bool
examine_output = foldM examine_output_h True
                 
  where
    examine_output_h :: Bool -> TestResult -> IO Bool
    examine_output_h acc (TestResult status token target actual) = do
      if status == True then do
          putStr   $ color_string Blue "["
          putStr   $ color_string White "OK"
          putStr   $ color_string Blue  "] "
          putStr   $ color_string Green " Successfully evaluated "
          putStrLn $ color_string Yellow token
          return $ acc && True
        else do
          putStr   $ color_string Blue "["
          putStr   $ color_string Red "TROUBLE"
          putStr   $ color_string Blue "] "
          putStr   $ color_string Green " Failed to evaluate "
          putStrLn $ color_string Yellow token
          putStrLn $ color_string Green "\t\texpected : " 
                       ++ (show target)
          putStrLn $ color_string Green "\t\tgot      : " 
                       ++ (show actual)
          return False
    

-- | main test routine for "good tests"
good_test_driver :: CalcState -> [GoodTestCase] 
                 -> Writer [TestResult] ()
good_test_driver _ []         = return ()
good_test_driver state (x:xs) = do

  let tok      = fst x
  let expected = snd x
  case runParser main_parser state "" tok of
    Left er -> tell [TestResult False tok (show expected) (show er)]
    Right (result, newState) -> examine_result expected result tok
        
      where
        -- NOTE: when we compare target and actual result we
        -- probably need to be more careful and can't use ==
        -- if we are dealing with Doubles!!!
        examine_result :: (Double,String) -> (Double,String) 
                       -> String -> Writer [TestResult] ()
        examine_result (t_value,t_unit) (a_value,a_unit) tok = 
          if ((is_equal t_value a_value) && (t_unit == a_unit)) 
            then do
              tell [TestResult True tok target_string actual_string]
              good_test_driver newState xs
            else do
              tell [TestResult False tok target_string actual_string]
              good_test_driver newState xs

            where
              -- we compare doubles x,y for equality by means
              -- of abs(x-y) <= dbl_epsilon * abs(x)
              is_equal a b = abs(a-b) <= abs(a) * dbl_epsilon

              actual_string = (show a_value) ++ " " ++ a_unit
              target_string = (show t_value) ++ " " ++ t_unit


-- | main test routine for "failing tests"
failing_test_driver :: CalcState -> [FailingTestCase] 
                    -> Writer [TestResult] ()
failing_test_driver _ []         = return ()
failing_test_driver state (x:xs) = do

  case runParser main_parser state "" x of
    Left er           -> tell [TestResult True x "Failure" "Failure"]
                         >> failing_test_driver state xs
    Right (_,status)  -> case have_special_error status of
                           Just err -> tell [TestResult True x 
                                             "Failure" "Failure"]
                                       >> failing_test_driver state xs
                           Nothing  -> tell [TestResult False x 
                                             "Failure" "Success"]
 
-- | our test results consist of a bool indicating success
-- or failure, the test token as well as the expected and
-- received result
data TestResult = TestResult { status :: Bool
                             , token  :: String
                             , target :: String
                             , actual :: String
                             }


defaultResult :: TestResult
defaultResult = TestResult False "" "" ""


-- | a good test case consists of an expression and an
-- expected result (value,unit)
type GoodTestCase  = (String, (Double,String))


-- | a failing test case currently consists only of an
-- expression to be parser and we simply tests if it
-- fails as expected. 
-- FIXME:
-- In principle, we should check for the correct failure 
-- message. However, since I am still playing with the parser 
-- these may change so for now we just check for failure.
type FailingTestCase = String


-- NOTE: For each "run" of test_driver we thread a common 
-- calculator state to be able to test variable assignment
-- and use. Therefore, the order of which tests appear in
-- a [GoodTestCase] may matter if variable definitions are involved.
-- I.e., think twice when changing the order, or keep order
-- dependend and independent sets in different lists 
simpleTests :: [GoodTestCase]
simpleTests = [ simpleTest1, simpleTest2, simpleTest3, simpleTest4
              , simpleTest5, simpleTest6]{-, simpleTest7
              , simpleTest8, simpleTest9, simpleTest10, simpleTest11
              , simpleTest12, simpleTest13, simpleTest14]i-}

-- list of simple tests
simpleTest1 :: GoodTestCase
simpleTest1 = ("\\c 0F C", (-17.77777777777778,"C") )

simpleTest2 :: GoodTestCase
simpleTest2 = ("\\c 0C F", (32,"F"))

simpleTest3 :: GoodTestCase
simpleTest3 = ("\\c -12C K", (261.15,"K"))

simpleTest4 :: GoodTestCase
simpleTest4 = ("\\c -12K C", (-285.15,"C"))

simpleTest5 :: GoodTestCase
simpleTest5 = ("\\c 23F K", (268.15,"K"))

simpleTest6 :: GoodTestCase
simpleTest6 = ("\\c 45K F", (-378.67,"F"))
{-
simpleTest7 :: GoodTestCase
simpleTest7 = ("(((((((3.0+3.0)*(9.0+8.0)))))))", 102.0)

simpleTest8 :: GoodTestCase
simpleTest8 = ("(((((((3.0+3.0)))))*(((((9.0+8.0)))))))", 102.0)

simpleTest9 :: GoodTestCase
simpleTest9 = ("3+3*99.0", 300.0)

simpleTest10 :: GoodTestCase
simpleTest10 = ("3+3*8+4*3*2+1*4*3+5", 68.0)

simpleTest11 :: GoodTestCase
simpleTest11 = ("(3+3)*(8+4)*3*(2+1)*4*(3+5)", 20736.0)

simpleTest12 :: GoodTestCase
simpleTest12 = (" 3  +3*     99.0", 300.0)

simpleTest13 :: GoodTestCase
simpleTest13 = (" 3  + 3*8+4  *3 *2+1*  4*3+5  ", 68.0)

simpleTest14 :: GoodTestCase
simpleTest14 = ("(3+3)   *(8+4)*3 *  (2+1 )*4*( 3+5)", 20736.0)

-}


-- a few tests that are failing 
failingTests :: [FailingTestCase]
failingTests = [ failingTest1, failingTest2, failingTest3]
 {-              , failingTest4, failingTest5, failingTest6
               , failingTest7, failingTest8, failingTest9
               , failingTest10, failingTest11, failingTest12 ]-}

-- list of failing tests
failingTest1 :: FailingTestCase
failingTest1 = ("\\c 1F F")

failingTest2 :: FailingTestCase
failingTest2 = ("\\c 1C D")

failingTest3 :: FailingTestCase
failingTest3 = ("\\c C F")
{-
failingTest4 :: FailingTestCase
failingTest4 = ("(3.0*3.0)+3.0*4.0)")

failingTest5 :: FailingTestCase
failingTest5 = ("(3y3)*(9+8)")

failingTest6 :: FailingTestCase
failingTest6 = ("(3.0+3.0)*(9.0+8.0")

failingTest7 :: FailingTestCase
failingTest7 = ("(((((((3.0+3.0)*(9.0+8.0))))))")

failingTest8 :: FailingTestCase
failingTest8 = ("(((((((3.0+3.0))))*((((((9.0+8.0)))))))")

failingTest9 :: FailingTestCase
failingTest9 = ("a3+3*99.0")

failingTest10 :: FailingTestCase
failingTest10 = ("3+3*8+4*3++2+1*4*3+5")

failingTest11 :: FailingTestCase
failingTest11 = ("(3+3)**(8+4)*3*(2+1)*4*(3+5)")

failingTest12 :: FailingTestCase
failingTest12 = ("b")
-}
