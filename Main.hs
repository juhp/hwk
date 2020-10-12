{-# LANGUAGE CPP #-}

#if !MIN_VERSION_simple_cmd_args(0,1,3)
import Control.Applicative ((<|>))
#endif
import Control.Monad.Extra
import qualified Data.List.Extra as L
import Data.Version (showVersion)
import Language.Haskell.Interpreter
import SimpleCmdArgs
import System.Directory
import System.FilePath
import System.IO (hPutStrLn, stderr)

import Paths_hwk (getDataDir, version)

data HwkMode = DefaultMode | WholeMode | LineMode | TypeMode | EvalMode
  deriving Eq

main :: IO ()
main =
  simpleCmdArgs (Just version) "A Haskell awk/sed like tool"
    "Simple shell text processing with Haskell" $
  runExpr <$> modeOpt <*> strArg "FUNCTION" {-<*> many (strArg "FILE...")-}
  where
    modeOpt :: Parser HwkMode
    modeOpt =
      flagWith' TypeMode 't' "type-check" "Print out the type of the given function" <|>
      flagWith' EvalMode 'e' "eval" "Evaluate a Haskell expression" <|>
      flagWith' WholeMode 'a' "all" "Apply function once to the whole input" <|>
      flagWith DefaultMode LineMode 'l' "line" "Apply function to each line"

runExpr :: HwkMode -> String -> {-[FilePath] ->-} IO ()
runExpr mode stmt {-files-} = do
  --mapM_ checkFileExists files
  input <- getContents
  userdir <- getXdgDirectory XdgConfig "hwk"
  let usercfg = userdir </> "Hwk.hs"
  datadir <- getDataDir
  let versionCfg = usercfg ++ "-" ++ showVersion version
  unlessM (doesFileExist versionCfg) $
    copyFile (datadir </> "Hwk.hs") versionCfg
  unlessM (doesFileExist usercfg) $ do
    copyFile (datadir </> "Hwk.hs") usercfg
    warn $ usercfg ++ " created"
  r <- runInterpreter (runHint userdir input)
  case r of
    Left err -> putStrLn $ errorString err
    Right () -> return ()
  where
    runHint :: FilePath -> String -> Interpreter ()
    runHint cfgdir input = do
      set [searchPath := [cfgdir], languageExtensions := [TypeApplications]]
      loadModules ["Hwk"]
      let setHwkImports ms = setImports (L.nub (ms ++ ["Hwk"]))
      setHwkImports ["Prelude"]
      imports <- do
        haveModules <- typeChecks "userModules"
        if haveModules
          then interpret "userModules" infer
          else return ["Prelude", "Data.List"]
      setHwkImports imports
      case mode of
        DefaultMode -> mapInputList stmt (lines input)
        LineMode -> mapEachLine stmt (lines input)
        WholeMode -> applyToInput stmt (removeTrailingNewline input)
        TypeMode -> typeOfExpr stmt
        EvalMode -> evalExpr stmt
      where
        removeTrailingNewline :: String -> String
        removeTrailingNewline "" = ""
        removeTrailingNewline s =
          if last s == '\n'
          then init s
          else s

    -- checkFileExists :: FilePath -> IO ()
    -- checkFileExists file = do
    --   unlessM (doesFileExist file) $
    --     error' $ "file not found: " ++ file

    errorString :: InterpreterError -> String
    errorString (WontCompile es) =
      unlines $ "ERROR: Won't compile:" : map errMsg es
    errorString e = show e

cleanupType :: String -> String
cleanupType = L.replace "[Char]" "String"

-- fn $ lines input
mapInputList :: String -> [String] -> InterpreterT IO ()
mapInputList stmt inputs = do
  typ <- resultTypeOfApplied stmt "[String]"
  case cleanupType typ of
    "String" -> do
      fn <- interpret stmt (as :: [String] -> String)
      liftIO $ putStrLn (fn inputs)
    "[String]" -> do
      fn <- interpret stmt (as :: [String] -> [String])
      liftIO $ mapM_ putStrLn (fn inputs)
    "[[String]]" -> do
      fn <- interpret stmt (as :: [String] -> [[String]])
      liftIO $ mapM_ (putStrLn . unwords) (fn inputs)
    "Int" -> do
      fn <- interpret stmt (as :: [String] -> Int)
      liftIO $ (putStrLn . show) (fn inputs)
    "[Int]" -> do
      fn <- interpret stmt (as :: [String] -> [Int])
      liftIO $ mapM_ (putStrLn . show) (fn inputs)
    "[[Int]]" -> do
      fn <- interpret stmt (as :: [String] -> [[Int]])
      liftIO $ mapM_ (putStrLn . unwords . map show) (fn inputs)
    _ -> do
      liftIO $ warn typ
      fn <- interpret stmt (as :: [String] -> [String])
      liftIO $ mapM_ putStrLn (fn inputs)

-- map fn $ lines input
mapEachLine :: String -> [String] -> InterpreterT IO ()
mapEachLine stmt inputs = do
  typ <- resultTypeOfApplied stmt "String"
  case cleanupType typ of
    "String" -> do
      fn <- interpret stmt (as :: String -> String)
      liftIO $ mapM_ (putStrLn . fn) inputs
    "[String]" -> do
      fn <- interpret stmt (as :: String -> [String])
      liftIO $ mapM_ (putStrLn . unwords . fn) inputs
    "[[String]]" -> do
      fn <- interpret stmt (as :: String -> [[String]])
      liftIO $ mapM_ (putStrLn . L.intercalate "\t" . map unwords . fn) inputs
    "Int" -> do
      fn <- interpret stmt (as :: String -> Int)
      liftIO $ mapM_ (putStrLn . show . fn) inputs
    "[Int]" -> do
      fn <- interpret stmt (as :: String -> [Int])
      liftIO $ mapM_ (putStrLn . unwords . map show . fn) inputs
    "[[Int]]" -> do
      fn <- interpret stmt (as :: String -> [[Int]])
      liftIO $ mapM_ (putStrLn . L.intercalate "\t" . map (unwords . map show) .fn) inputs
    _ -> do
      liftIO $ warn typ
      fn <- interpret stmt (as :: String -> String)
      liftIO $ mapM_ putStrLn (map fn inputs)

-- fn input
applyToInput :: String -> String -> InterpreterT IO ()
applyToInput stmt input = do
  typ <- resultTypeOfApplied stmt "String"
  case cleanupType typ of
    "String" -> do
      fn <- interpret stmt (as :: String -> String)
      liftIO $ putStrLn (fn input)
    "[String]" -> do
      fn <- interpret stmt (as :: String -> [String])
      liftIO $ mapM_ putStrLn (fn input)
    "[[String]]" -> do
      fn <- interpret stmt (as :: String -> [[String]])
      liftIO $ mapM_ (putStrLn . unwords) (fn input)
    "Int" -> do
      fn <- interpret stmt (as :: String -> Int)
      liftIO $ (putStrLn . show) (fn input)
    "[Int]" -> do
      fn <- interpret stmt (as :: String -> [Int])
      liftIO $ mapM_ (putStrLn . show) (fn input)
    "[[Int]]" -> do
      fn <- interpret stmt (as :: String -> [[Int]])
      liftIO $ mapM_ (putStrLn . unwords . map show) (fn input)
    _ -> do
      liftIO $ warn typ
      fn <- interpret stmt (as :: String -> [String])
      liftIO $ mapM_ putStrLn (fn input)

typeOfExpr :: String -> InterpreterT IO ()
typeOfExpr stmt = do
#if MIN_VERSION_hint(0,8,0)
  etypchk <- typeChecksWithDetails stmt
  case etypchk of
    Left err -> liftIO $ mapM_ (putStrLn . errMsg) err
    Right typ -> liftIO $ putStrLn (cleanupType typ)
#else
  typeOf stmt >>= liftIO . putStrLn . cleanupType
#endif

evalExpr :: String -> InterpreterT IO ()
evalExpr stmt = do
  typ <- typeOf stmt
  case cleanupType typ of
    "String" -> do
      interpret stmt "a String" >>= liftIO . putStrLn
    "[String]" -> do
      interpret stmt ["a String"] >>= liftIO . mapM_ putStrLn
    "[[String]]" -> do
      interpret stmt [["a String"]] >>= liftIO . mapM_ (putStrLn . unwords)
    "[Int]" -> do
      interpret stmt [1 :: Int] >>= liftIO . mapM_ (putStrLn . show)
    "[[Int]]" -> do
      interpret stmt [[1 :: Int]] >>= liftIO . mapM_ (putStrLn . unwords . map show)
    _ -> do
      if " -> " `L.isInfixOf` typ
        then liftIO $ putStrLn typ
        else do
        res <- eval stmt
        -- FIXME option to display type
        liftIO $ putStrLn $ res ++ " :: " ++ typ

warn :: String -> IO ()
warn = hPutStrLn stderr

resultTypeOfApplied :: MonadInterpreter m => String -> String -> m String
resultTypeOfApplied expr typ =
  L.dropPrefix (typ ++ " -> ") <$> typeOf (expr ++ " . (id  @" ++ typ ++ ")")
