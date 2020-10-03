{-# LANGUAGE CPP #-}

import Control.Monad.Extra
import SimpleCmd
#if MIN_VERSION_simple_cmd(0,2,1)
  hiding (ifM)
#endif
import SimpleCmdArgs
import System.Directory
import System.FilePath
import System.IO.Extra
import System.Process

import Paths_hwk (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "A Haskell awk/sed like tool"
    "Simple shell text processing with Haskell" $
  runExpr <$> switchWith 'd' "debug" "Output Haskell" <*> optional (strOptionWith 'f' "file" "FILE" "User function include file") <*> strArg "FUNCTION"

runExpr :: Bool -> Maybe FilePath -> String -> IO ()
runExpr dbg mfile stmt = do
  functions <- do
    case mfile of
      Nothing -> do
        cfg <- getAppUserDataDirectory "hwk.hs"
        ifM (doesFileExist cfg) (lines <$> readFile cfg) (return [])
      Just f -> lines <$> readFile f
  withTempDir $ \tempDir -> do
    let tempFile = tempDir </> "hwk.hs"
    writeFile tempFile $ genScript functions stmt
    if dbg then do
      ok <- cmdBool "runghc" [tempFile]
      unless ok $ readFile tempFile >>= error'
      else void $ rawSystem "runghc" [tempFile]

genScript :: [String] -> [Char] -> String
genScript functions stmt =
  unlines $ [extensions, imports] ++ functions ++ genFunction "hwk" stmt : genMain "hwk" : [toInt, toList]
  where
    extensions, imports :: String
    extensions = "{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}"

    imports = unlines [ "import Data.List"
                      , "import Data.List.Split"
                      , "import Data.Char"
                      ]

    genMain :: String -> String
    genMain fname =
      "main = getContents >>= mapM_ putStrLn . toList . " ++ fname ++ " . lines"

    toList, toInt :: String
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

genFunction :: [Char] -> [Char] -> [Char]
genFunction fname stmt = fname ++ " = " ++ stmt
