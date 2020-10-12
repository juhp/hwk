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
      set [searchPath := [cfgdir]]
      loadModules ["Hwk"]
      setHwkImports ["Prelude"]
      imports <- do
        haveModules <- typeChecks "userModules"
        if haveModules
          then interpret "userModules" infer
          else return ["Prelude", "Data.List"]
      setHwkImports imports
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
          liftIO $ mapM_ putStrLn (fn (removeTrailingNewline input))
        TypeMode -> do
#if MIN_VERSION_hint(0,8,0)
          etypchk <- typeChecksWithDetails stmt
          case etypchk of
            Left err -> liftIO $ mapM_ (putStrLn . errMsg) err
            Right typ -> liftIO $ putStrLn (cleanupType typ)
#else
          typeOf stmt >>= liftIO . putStrLn . cleanupType
#endif
        EvalMode -> do
          typ <- cleanupType <$> typeOf stmt
          case typ of
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
              res <- eval stmt
              -- FIXME option to display type
              liftIO $ putStrLn $ res ++ " :: " ++ typ
      where
        cleanupType :: String -> String
        cleanupType = L.replace "[Char]" "String"

        removeTrailingNewline :: String -> String
        removeTrailingNewline "" = ""
        removeTrailingNewline s =
          if last s == '\n'
          then init s
          else s

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

warn :: String -> IO ()
warn = hPutStrLn stderr
