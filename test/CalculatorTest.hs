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

-- | handcoded test for checking out the parser
module Main where


-- import
import Control.Monad.Writer
import System.Exit


-- local imports
import CalculatorParser
import CalculatorState
import ExtraFunctions
import PrettyPrint
import TokenParser


-- | top level main routine 
-- we use the Writer monad to capture the results for all tests 
-- and then examine the results afterward
main :: IO ()
main = do
  putStr $ color_string Cyan "\nSimple tests:\n"
  let simple = execWriter $ good_test_driver defaultCalcState 
               simpleTests
  status1 <- examine_output simple

  putStr $ color_string Cyan "\nVariable tests:\n"
  let vars = execWriter $ good_test_driver defaultCalcState 
             variableTests
  status2 <- examine_output vars

  putStr $ color_string Cyan "\nFailure tests:\n"
  let failing = execWriter $ failing_test_driver defaultCalcState 
                failingTests
  status2 <- examine_output failing


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
  case runParser calculator state "" tok of
    Left er -> tell [TestResult False tok (show expected) (show er)]
    Right (result, newState) -> examine_result expected result tok
        
      where
        -- NOTE: when we compare target and actual result we
        -- probably need to be more careful and can't use ==
        -- if we are dealing with Doubles!!!
        examine_result :: Double -> Double -> String 
                       -> Writer [TestResult] ()
        examine_result target actual tok = 
          if (is_equal target actual) 
            then do
              tell [TestResult True tok (show target) (show actual)]
              good_test_driver newState xs
            else do
              tell [TestResult False tok (show target) (show actual)]
              good_test_driver newState xs

            where
              -- we compare doubles x,y for equality by means
              -- of abs(x-y) <= dbl_epsilon * abs(x)
              is_equal a b = abs(a-b) <= abs(a) * dbl_epsilon


-- | main test routine for "failing tests"
failing_test_driver :: CalcState -> [FailingTestCase] 
                    -> Writer [TestResult] ()
failing_test_driver _ []         = return ()
failing_test_driver state (x:xs) = do

  case runParser calculator state "" x of
    Left er  -> tell [TestResult True x "Failure" "Failure"]
                >> failing_test_driver state xs
    Right _  -> tell [TestResult False x "Failure" "Success"]
                  
 
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
-- expected result
type GoodTestCase  = (String, Double)


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
              , simpleTest5, simpleTest6, simpleTest7
              , simpleTest8, simpleTest9, simpleTest10, simpleTest11
              , simpleTest12, simpleTest13, simpleTest14]

-- list of simple tests
simpleTest1 :: GoodTestCase
simpleTest1 = ("3+4", 7.0)

simpleTest2 :: GoodTestCase
simpleTest2 = ("3*3", 9.0)

simpleTest3 :: GoodTestCase
simpleTest3 = ("(3*3)+(3*4)", 21.0)

simpleTest4 :: GoodTestCase
simpleTest4 = ("(3.0*3.0)+(3.0*4.0)", 21.0)

simpleTest5 :: GoodTestCase
simpleTest5 = ("(3+3)*(9+8)", 102.0)

simpleTest6 :: GoodTestCase
simpleTest6 = ("(3.0+3.0)*(9.0+8.0)", 102.0)

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




-- a few tests involving variables
variableTests :: [GoodTestCase]
variableTests = [ variableTest1, variableTest2, variableTest3
                , variableTest4, variableTest5, variableTest6
                , variableTest7, variableTest8, variableTest9
                , variableTest10, variableTest11, variableTest12 ] 

-- list of failing tests
variableTest1 :: GoodTestCase
variableTest1 = ("b = 4", 4)

variableTest2 :: GoodTestCase
variableTest2 = ("3 * b ", 12)

variableTest3 :: GoodTestCase
variableTest3 = ("(b*b)", 16)

variableTest4 :: GoodTestCase
variableTest4 = ("a = 12", 12)

variableTest5 :: GoodTestCase
variableTest5 = ("a * b", 48)

variableTest6 :: GoodTestCase
variableTest6 = ("a - b * b", (-4))

variableTest7 :: GoodTestCase
variableTest7 = ("3 * b - a", 0)

variableTest8 :: GoodTestCase
variableTest8 = ("kjhdskfsd123hjksdf = a * b", 48)

variableTest9 :: GoodTestCase
variableTest9 = ("(a*b) - kjhdskfsd123hjksdf", 0)

variableTest10 :: GoodTestCase
variableTest10 = ("c = 2", 2) 

variableTest11 :: GoodTestCase
variableTest11 = ("a-b-c + ( a + b + c ) + (a*a)", 168)

variableTest12 :: GoodTestCase
variableTest12 = ("b^a - c", 16777214)



-- a few tests that are failing 
failingTests :: [FailingTestCase]
failingTests = [ failingTest1, failingTest2, failingTest3
               , failingTest4, failingTest5, failingTest6
               , failingTest7, failingTest8, failingTest9
               , failingTest10, failingTest11, failingTest12 ]

-- list of failing tests
failingTest1 :: FailingTestCase
failingTest1 = ("3+4b")

failingTest2 :: FailingTestCase
failingTest2 = ("3*a3")

failingTest3 :: FailingTestCase
failingTest3 = ("(3*3)B+(3*4)")

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

