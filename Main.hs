module Main where

import System.Environment (getArgs, getEnvironment)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import System.Posix.Process (executeFile)
import System.Posix.Temp (mkdtemp)
import Data.List (isPrefixOf)

main :: IO ()
main = getArgs >>= parse

parse [] = usage >> die
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse ["clear"] = do
    envs <- getEnvironment
    putStrLn $ unlines $ envUnsetFunctions envs
    exit
parse ["env", fname, stmt] = do
    putStrLn $ "export " ++ envFunctionPrefix ++ fname ++ "='" ++ stmt ++ "'"
    exit
parse [stmt] = do
    envs <- getEnvironment
    tempDir <- mkdtemp "/tmp/"
    let tempFile = tempDir ++ "/hwk.hs"
    writeFile tempFile $ genModule (envFunctions envs) stmt
    executeFile "runhaskell" True [tempFile] Nothing

extensions = "{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}"
imports = unlines [ "import Data.List.Split"
                  , "import Data.List"
                  ]
genMain fname = "main = getContents >>= \\contents -> " ++
                "mapM_ putStrLn $ toList $ " ++ fname ++ " $ lines contents"

toList = unlines [ "class ToList a where toList :: a -> [String]"
                 , "instance ToList Integer where toList x = [show x]"
                 , "instance ToList String where toList x = [x]"
                 , "instance ToList [Integer] where toList lst = map (\\x -> show x) lst"
                 , "instance ToList [String] where toList = id"
                 , "instance ToList [[String]] where toList = map (intercalate \"\\t\")"
                 ]

toInt = unlines [ "int str = read str :: Integer"
                , "ints = map int"
                ]

libraryFunctions = [toInt, toList]
genFunction fname stmt = fname ++ " = " ++ stmt
genModule functions stmt = unlines $ [extensions, imports, genFunction "hwk" stmt, genMain "hwk"] ++
                           functions ++  libraryFunctions

exit    = exitSuccess
die     = exitWith (ExitFailure 1)
envFunctions envs = [envGenFunction e | e <- envs, envFunctionPrefix `isPrefixOf` fst e]
envGenFunction (fname, stmt) = genFunction (drop (length envFunctionPrefix) fname) stmt
envUnsetFunctions envs = ["unset " ++ fst e | e <- envs, envFunctionPrefix `isPrefixOf` fst e]
envFunctionPrefix = "HWK_FUNCTION_"
version = putStrLn "hwk - Haskell based AWK replacement v0.1"
usage   = putStr $ unlines [ "Usage:"
                           , "  hwk env <fname> <stmt>"
                           , "  hwk clear"
                           , "  hwk <stmt>"
                           , ""
                           , "Options:"
                           , "  -h   Show help screen."
                           , "  -v   Show version."
                           ]
