module Main where

import KWInference
import Term

{-asup = evalInEnvironment [] . computeASUPInstance []-}

{-infer str = either (error . show) (asup . evalInEnvironment [] . go) $ parse str-}
   {-where go exp = makeUniqueVars exp >>= thetaReduction . markTerm-}

toTerm :: String -> IO Term
toTerm = either (error . show) return . parse 

main :: IO ()
main = do
   --let t = parse "\\x. x x"
   let t = parse "\\x. x"
   either print (print . inferType []) t
