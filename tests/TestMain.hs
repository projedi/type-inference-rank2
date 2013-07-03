module Main where

import Test.Framework

import qualified TestTerm
import qualified TestTheta
import qualified TestASUP

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ TestTerm.tests
        , TestTheta.tests
        , TestASUP.tests
        ]
