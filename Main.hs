{-# LANGUAGE CPP #-}

#if !MIN_VERSION_simple_cmd_args(0,1,3)
import Control.Applicative ((<|>))
#endif
import qualified Data.List.Extra as L
import Language.Haskell.Interpreter
import SimpleCmdArgs
import System.Directory

import Paths_hwk (getDataDir, version)

data HwkMode = DefaultMode | WholeMode | LineMode | TypeMode
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
      flagWith' WholeMode 'a' "all" "Apply function once to the whole input" <|>
      flagWith DefaultMode LineMode 'l' "line" "Apply function to each line"

runExpr :: HwkMode -> String -> {-[FilePath] ->-} IO ()
runExpr mode stmt {-files-} = do
  --mapM_ checkFileExists files
  input <- getContents
  usercfg <- getXdgDirectory XdgConfig "hwk"
  datadir <- getDataDir
--    copyFile (datadir </> "hwk.hs") usercfg
  r <- runInterpreter (runHint [usercfg, datadir] input)
  case r of
    Left err -> putStrLn $ errorString err
    Right () -> return ()
  where
    runHint :: [FilePath] -> String -> Interpreter ()
    runHint cfgdirs input = do
      set [searchPath := cfgdirs]
      loadModules ["Hwk"]
      setHwkImports ["Prelude"]
      imports <- do
        haveModules <- typeChecks "userModules"
        if haveModules
          then interpret "userModules" infer
          else return ["Prelude", "Data.List"]
      setHwkImports imports
      if mode == TypeMode then do
#if MIN_VERSION_hint(0,8,0)
        etypchk <- typeChecksWithDetails stmt
        case etypchk of
          Left err -> liftIO $ mapM_ (putStrLn . errMsg) err
          Right typ -> liftIO $ putStrLn (cleanupType typ)
#else
        typeOf stmt >>= liftIO . putStrLn . cleanupType
#endif
        else do
        let polyList = "toList . "
            polyString = "toString . "
        case mode of
          DefaultMode -> do
            fn <- interpret (polyList ++ stmt) (as :: [String] -> [String])
            liftIO $ mapM_ putStrLn (fn (lines input))
          LineMode -> do
            fn <- interpret (polyString ++ stmt) (as :: String -> String)
            liftIO $ mapM_ (putStrLn . fn) (lines input)
          WholeMode -> do
            fn <- interpret (polyList ++ stmt) (as :: String -> [String])
            liftIO $ mapM_ putStrLn (fn input)
          TypeMode -> error "already handled earlier"
      where
        cleanupType :: String -> String
        cleanupType = L.replace "[Char]" "String"

    -- FIXME use Set
    setHwkImports ms = setImports (L.nub (ms ++ ["Hwk"]))

    -- checkFileExists :: FilePath -> IO ()
    -- checkFileExists file = do
    --   unlessM (doesFileExist file) $
    --     error' $ "file not found: " ++ file

    errorString :: InterpreterError -> String
    errorString (WontCompile es) =
      unlines $ "ERROR: Won't compile:" : map errMsg es
    errorString e = show e
