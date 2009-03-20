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

-- | handcoded test for checking out the calculator parser
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
  putStrLn "\n\n\nTesting calculator Parser ..."

  putStr $ color_string Cyan "\nSimple tests:\n"
  let simple = execWriter $ good_test_driver defaultCalcState 
               simpleTests
  status1 <- examine_output simple

  putStr $ color_string Cyan "\nFunction parsing tests:\n"
  let vars = execWriter $ good_test_driver defaultCalcState 
             functionTests
  status2 <- examine_output vars
 
  putStr $ color_string Cyan "\nVariable tests:\n"
  let vars = execWriter $ good_test_driver defaultCalcState 
             variableTests
  status3 <- examine_output vars

  putStr $ color_string Cyan "\nFailure tests:\n"
  let failing = execWriter $ failing_test_driver defaultCalcState 
                failingTests
  status4 <- examine_output failing


  let status = status1 && status2 && status3 && status4
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
    Right (DblResult result, newState) -> examine_result expected result tok
        
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


-- | main test routine for "failing tests"
failing_test_driver :: CalcState -> [FailingTestCase] 
                    -> Writer [TestResult] ()
failing_test_driver _ []         = return ()
failing_test_driver state (x:xs) = do

  case runParser main_parser state "" x of
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
              , simpleTest12, simpleTest13, simpleTest14
              , simpleTest15, simpleTest16, simpleTest17
              , simpleTest18, simpleTest19, simpleTest20
              , simpleTest21, simpleTest22, simpleTest23
              , simpleTest24, simpleTest25, simpleTest26
              , simpleTest27, simpleTest28, simpleTest29
              , simpleTest30, simpleTest31, simpleTest32
              , simpleTest33, simpleTest34]

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

simpleTest15 :: GoodTestCase
simpleTest15 = ("3*-4", -12.0)

simpleTest16 :: GoodTestCase
simpleTest16 = ("3* -4", -12.0)

simpleTest17 :: GoodTestCase
simpleTest17 = ("-3*4", -12.0)

simpleTest18 :: GoodTestCase
simpleTest18 = ("-3*-4", 12.0)

simpleTest19 :: GoodTestCase
simpleTest19 = ("3*(-4)", -12.0)

simpleTest20 :: GoodTestCase
simpleTest20 = ("(-3)*(-4)", 12.0)

simpleTest21 :: GoodTestCase
simpleTest21 = ("3/-4", -0.75)

simpleTest22 :: GoodTestCase
simpleTest22 = ("3^-4", 1/81)

simpleTest23 :: GoodTestCase
simpleTest23 = ("-3*-4^-4", -3/256)

simpleTest24 :: GoodTestCase
simpleTest24 = ("-3+-4", -7)

simpleTest25 :: GoodTestCase
simpleTest25 = ("-1/-1/-1/-1", 1.0)

simpleTest26 :: GoodTestCase
simpleTest26 = ("-(-(-1))", -1)

simpleTest27 :: GoodTestCase
simpleTest27 = ("3/-4; -1/-1/-1/-1; -3*-4^-4", -3/256)

simpleTest28 :: GoodTestCase
simpleTest28 = ("3*3; 4+5; 34 * 34   ; 3^-4", 1/81)

simpleTest29 :: GoodTestCase
simpleTest29 = ("3*3;4*4;-3*-4^-4", -3/256)

simpleTest30 :: GoodTestCase
simpleTest30 = ("  3; 3+4; 4*2   ; -3+-4", -7)

simpleTest31 :: GoodTestCase
simpleTest31 = ("3*1;3;3;3;3  ;-1/-1/-1/-1", 1.0)

simpleTest32 :: GoodTestCase
simpleTest32 = ("4^4;-(-(-1))", -1)

simpleTest33 :: GoodTestCase
simpleTest33 = ("-3", -3)

simpleTest34 :: GoodTestCase
simpleTest34 = (" -    9  ", -9)


-- a few tests involving variables
variableTests :: [GoodTestCase]
variableTests = [ variableTest1, variableTest2, variableTest3
                , variableTest4, variableTest5, variableTest6
                , variableTest7, variableTest8, variableTest9
                , variableTest10, variableTest11, variableTest12 
                , variableTest13, variableTest14, variableTest15
                , variableTest16, variableTest17, variableTest18
                , variableTest19, variableTest20 ] 

-- list of variable tests
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

variableTest13 :: GoodTestCase
variableTest13 = ("a=3; b=4; c=a/b; c*b", 3)

variableTest14 :: GoodTestCase
variableTest14 = ("x=   10; y = log(x); exp(y)", 10)

variableTest15 :: GoodTestCase
variableTest15 = ("x=5; x=6; x=7; 3*x; y = x*3", 21)

variableTest16 :: GoodTestCase
variableTest16 = ("x = 2; y = 10^x; log10(y)", 2)

variableTest17 :: GoodTestCase
variableTest17 = ("c = 2; d = c; d", 2) 

variableTest18 :: GoodTestCase
variableTest18 = (" x = pi; y = cos(x); acos(y)", pi)

variableTest19 :: GoodTestCase
variableTest19 = ("a = 5; -a", -5.0) 

variableTest20 :: GoodTestCase
variableTest20 = ("b= 15; 3*( - b)", -45)



-- a few tests involving builtin functions, mostly to check
-- for proper parsing rather than proper math
functionTests :: [GoodTestCase]
functionTests = [ functionTest1, functionTest2, functionTest3
                , functionTest4, functionTest5, functionTest6
                , functionTest7, functionTest8, functionTest9
                , functionTest10, functionTest11]

-- list of variable tests
functionTest1 :: GoodTestCase
functionTest1 = ("sqrt 2", 1.4142135623730951)

functionTest2 :: GoodTestCase
functionTest2 = ("sqrt 2 * 2", 2.8284271247461903)

functionTest3 :: GoodTestCase
functionTest3 = ("sqrt 2*2", 2.8284271247461903)

functionTest4 :: GoodTestCase
functionTest4 = ("sqrt(2*2)", 2)

functionTest5 :: GoodTestCase
functionTest5 = ("cos 0.5", 0.8775825618903728)

functionTest6 :: GoodTestCase
functionTest6 = ("cos 0.5 +0.5", 1.3775825618903728)

functionTest7 :: GoodTestCase
functionTest7 = ("cos 0.5 - 0.5", 0.37758256189037276)

functionTest8 :: GoodTestCase
functionTest8 = ("cos(0.5 -0.5)", 1.0)

functionTest9 :: GoodTestCase
functionTest9 = ("cos -0.5", 0.8775825618903728)

functionTest10 :: GoodTestCase
functionTest10 = ("cos(-0.5)", 0.8775825618903728) 

functionTest11 :: GoodTestCase
functionTest11 = ("cos 0.5 - cos -0.5", 0)


-- a few tests that are failing 
failingTests :: [FailingTestCase]
failingTests = [ failingTest1, failingTest2, failingTest3
               , failingTest4, failingTest5, failingTest6
               , failingTest7, failingTest8, failingTest9
               , failingTest10, failingTest11, failingTest12
               , failingTest13, failingTest14, failingTest15 ]

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

failingTest13 :: FailingTestCase
failingTest13 = ("3+3;;3+4")

failingTest14 :: FailingTestCase
failingTest14 = ("(3+3;4+4)")

failingTest15 :: FailingTestCase
failingTest15 = ("3+3, 3+3")

