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
import Test.QuickCheck

import Debug.Trace

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

  -- run a bunch of tests to test if our conversion can be
  -- be properly inverted
  check_invertibility


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
              , simpleTest5, simpleTest6, simpleTest7, simpleTest8
              , simpleTest9, simpleTest10, simpleTest11, simpleTest12
              , simpleTest13, simpleTest14, simpleTest13, simpleTest14
              , simpleTest15, simpleTest16, simpleTest17, simpleTest18
              , simpleTest19, simpleTest20]

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
 
simpleTest7 :: GoodTestCase
simpleTest7 = ("\\c 1ft m", (0.3048,"m"))

simpleTest8 :: GoodTestCase
simpleTest8 = ("\\c 4m ft", (13.123359580052492,"ft"))

simpleTest9 :: GoodTestCase
simpleTest9 = ("\\c 1km mi", (0.621371192237334,"mi"))

simpleTest10 :: GoodTestCase
simpleTest10 = ("\\c 5km nmi", (2.6997840172786174, "nmi"))

simpleTest11 :: GoodTestCase
simpleTest11 = ("\\c 23.1m ft", (75.78740157480314, "ft"))

simpleTest12 :: GoodTestCase
simpleTest12 = ("\\c 0.45mi km", (0.7242048, "km"))

simpleTest13 :: GoodTestCase
simpleTest13 = ("\\c 43.2mi m", (69523.66080000001, "m"))

simpleTest14 :: GoodTestCase
simpleTest14 = ("\\c 4.2m in", (165.35433070866142, "in"))

simpleTest15 :: GoodTestCase
simpleTest15 = ("\\c 34.2m mi", (2.125089477451682e-2, "mi"))

simpleTest16 :: GoodTestCase
simpleTest16 = ("\\c 123.3m nmi", (6.657667386609072e-2, "nmi"))

simpleTest17 :: GoodTestCase
simpleTest17 = ("\\c 1.23m yd", (1.3451443569553807, "yd"))

simpleTest18 :: GoodTestCase
simpleTest18 = ("\\c 0.23nmi km", (0.42596, "km"))

simpleTest19 :: GoodTestCase
simpleTest19 = ("\\c 1.2nmi m", (2222.4, "m"))

simpleTest20 :: GoodTestCase
simpleTest20 = ("\\c 1.23yd m", (1.124712, "m"))


-- a few tests that are failing 
failingTests :: [FailingTestCase]
failingTests = [ failingTest1, failingTest2, failingTest3
               , failingTest4, failingTest5, failingTest6
               , failingTest7, failingTest8, failingTest9
               , failingTest10, failingTest11, failingTest12 ]

-- list of failing tests
failingTest1 :: FailingTestCase
failingTest1 = ("\\c 1F F")

failingTest2 :: FailingTestCase
failingTest2 = ("\\c 1C D")

failingTest3 :: FailingTestCase
failingTest3 = ("\\c C F")

failingTest4 :: FailingTestCase
failingTest4 = ("\\c 1F mi")

failingTest5 :: FailingTestCase
failingTest5 = ("\\c 1mi mi")

failingTest6 :: FailingTestCase
failingTest6 = ("\\c 1mi 1K")

failingTest7 :: FailingTestCase
failingTest7 = ("\\c 1nmi yd")

failingTest8 :: FailingTestCase
failingTest8 = ("\\c 1yd yd")

failingTest9 :: FailingTestCase
failingTest9 = ("\\c 1K K")

failingTest10 :: FailingTestCase
failingTest10 = ("\\c 1F F")

failingTest11 :: FailingTestCase
failingTest11 = ("c 1C yd")

failingTest12 :: FailingTestCase
failingTest12 = ("\\c c c 1")


-- | list of unit conversion pairs for inversion test
unitPairs :: [(String,String)]
unitPairs = [ unitPair1, unitPair2, unitPair3, unitPair4, unitPair5
            , unitPair6, unitPair7]

unitPair1 :: (String,String)
unitPair1 = ("m","yd")

unitPair2 :: (String,String)
unitPair2 = ("m","in")

unitPair3 :: (String,String)
unitPair3 = ("m","mi")

unitPair4 :: (String,String)
unitPair4 = ("ft","m")

unitPair5 :: (String,String)
unitPair5 = ("km","mi")

unitPair6 :: (String,String)
unitPair6 = ("km","nmi")

unitPair7 :: (String,String)
unitPair7 = ("m","nmi")


-- | function using quickcheck to test if our unit conversions
-- are properly invertible, i.e. if we convert m to yd and then
-- back we should end up with our initial result
-- NOTE: It would be great if we could check the "return status"
-- of quickcheck somehow so we can propage the result to the
-- shell
check_invertibility :: IO ()
check_invertibility =
    (putStr $ color_string Cyan "\n\nCheck Invertibility ..\n") 
    >> mapM_ (\x -> quickCheck $ prop_invert (fst x) (snd x)) 
       unitPairs


-- | properties for Quickcheck
-- in a nutshell if our conversion functions are invertible,
-- e.g. \c (\c 5yd m)m yd == 5
-- NOTE: We could use Double here instead of Integer but then
-- we had to make dbl_epsilon slightly less strict 
prop_invert :: String -> String -> Integer -> Bool
prop_invert u1 u2 i =
  case runParser main_parser defaultCalcState "" $ test_to i of
    Left _  -> False
    Right ((x,_),_) -> 
      case runParser main_parser defaultCalcState "" $ test_from x of
        Left _ -> False
        Right ((y,_),_) -> is_equal (fromInteger i) y

  where
    test_to z   = "\\c " ++ (show z) ++ u1 ++ " " ++ u2
    test_from z = "\\c " ++ (show z) ++ u2 ++ " " ++ u1
