{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception.ContextTH

import Control.Exception hiding (try)
import Control.Monad.Catch
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------

f :: IO ()
f = $chainedError "some error"

f' :: IO ()
f' = $chainedError'

data TestException = TestException deriving Show
instance Exception TestException

fe :: IO ()
fe = throwM TestException

f1 :: IO ()
f1 = $addContext "some context" f

f1' :: IO ()
f1' = $addContext' f

fe' :: IO ()
fe' = $addContext' fe

fe'e :: IO ()
fe'e = $addContext "some context 2" fe'

fmb :: IO ()
fmb = $maybeAddContext "some context 3" Nothing

fmb' :: IO ()
fmb' = $maybeAddContext' Nothing

fei' :: IO ()
fei' = $eitherAddContext' $ Left "some error description 4"

--------------------------------------------

checkExceptions :: IO () -> String -> IO ()
checkExceptions m s = try m >>= \ case
    Right _                    -> assertFailure "it should throw an error"
    Left (e :: SomeException)  -> show e @?= s

main :: IO ()
main = defaultMain $ testGroup "exceptions" [ testCase "f"    $ checkExceptions f    "some error (tests/Test.hs:17)"
                                            , testCase "f'"   $ checkExceptions f'   "(tests/Test.hs:20)"
                                            , testCase "fe"   $ checkExceptions fe   "TestException"
                                            , testCase "f1"   $ checkExceptions f1   "some context (tests/Test.hs:29) // some error (tests/Test.hs:17)"
                                            , testCase "f1'"  $ checkExceptions f1'  "(tests/Test.hs:32) // some error (tests/Test.hs:17)"
                                            , testCase "fe'"  $ checkExceptions fe'  "(tests/Test.hs:35) // TestException"
                                            , testCase "fe'e" $ checkExceptions fe'e "some context 2 (tests/Test.hs:38) // (tests/Test.hs:35) // TestException"
                                            , testCase "fmb"  $ checkExceptions fmb  "some context 3 (tests/Test.hs:41)"
                                            , testCase "fmb'" $ checkExceptions fmb' "(tests/Test.hs:44)"
                                            , testCase "fei'" $ checkExceptions fei' "some error description 4 (tests/Test.hs:47)"
                                            ]
