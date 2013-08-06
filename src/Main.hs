module Main(getType, main) where

import System.Environment
import System.Exit

import KWInference
import Term
import Type

{-asup = evalInEnvironment [] . computeASUPInstance []-}

{-infer str = either (error . show) (asup . evalInEnvironment [] . go) $ parse str-}
   {-where go exp = makeUniqueVars exp >>= thetaReduction . markTerm-}

--toTerm :: String -> IO Term
--toTerm = either (error . show) return . parse 

--- GHCi interface ---

getType :: String -> [String] -> IO Type
getType term userTypes = do
   (term',userTypes') <- parseArgs (term:userTypes)
   case inferType userTypes' term' of
    Left err -> error $ show err
    Right t -> return t

--- Command line interface ---

printHelp :: IO ()
printHelp = do
   putStrLn "USAGE: program-name term [user-annotation] [--help|-h]"
   putStrLn "user-annotation is in the form 'var:type'"
   putStrLn "Exit code 1 is error in arguments"
   putStrLn "Exit code 2 is error in type inference"

parseArgs :: [String] -> IO (Term, [(String, Type)])
parseArgs args
 | "--help" `elem` args || "-h" `elem` args = printHelp >> exitSuccess
 | length args < 1 = putStrLn "Error: not enough arguments" >> printHelp >> exitWith (ExitFailure 1)
 | otherwise = do
    term <- either (printError "parsing term") return $ Term.parse $ head args
    userTypes <- mapM parseUserType $ tail args
    return (term, userTypes)
 where printError what err = putStr ("Error " ++ what ++ ": ") >> print err >> exitWith (ExitFailure 1)
       parseUserType (':':xs) = do
          t <- either (printError "parsing type") return $ Type.parse xs
          return ("",t)
       parseUserType (x:xs) = do
          (y,t) <- parseUserType xs
          return (x:y, t)
       parseUserType [] = do
          putStrLn "Error parsing user annotation"
          exitWith (ExitFailure 1)

main :: IO ()
main = do
   args <- getArgs
   (term, userTypes) <- parseArgs args
   case inferType userTypes term of
    Left err -> do
       putStr "Error infering type: "
       print err
       exitWith (ExitFailure 2)
    Right t -> print t
