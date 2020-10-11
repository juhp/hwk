import qualified Data.List.Extra as L
import Language.Haskell.Interpreter
import SimpleCmdArgs
import System.Directory

import Paths_hwk (getDataDir, version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "A Haskell awk/sed like tool"
    "Simple shell text processing with Haskell" $
  runExpr <$> switchWith 'a' "all" "All input as a single string" <*> switchWith 't' "type-check" "Print out the type of the given function" <*> strArg "FUNCTION" {-<*> many (strArg "FILE...")-}

runExpr :: Bool -> Bool -> String -> {-[FilePath] ->-} IO ()
runExpr allinput checktype stmt {-files-} = do
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
      if checktype then do
        etypchk <- typeChecksWithDetails stmt
        case etypchk of
          Left err -> liftIO $ mapM_ (putStrLn . errMsg) err
          Right typ -> liftIO $ putStrLn (cleanupType typ)
        else do
        poly <- do
          havePoly <- typeChecks "polymorph"
          if havePoly then do
            s <- interpret "polymorph" infer
            return $ if null s then "" else s ++ " . "
            else return ""
        if allinput
          then do
          fn <- interpret (poly ++ stmt) (as :: String -> [String])
          liftIO $ mapM_ putStrLn (fn input)
          else do
          fn <- interpret (poly ++ stmt) (as :: [String] -> [String])
          liftIO $ mapM_ putStrLn (fn (lines input))
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
    errorString (WontCompile es) = L.intercalate "\n" (header : map unbox es)
      where
        header = "ERROR: Won't compile:"
        unbox (GhcError e) = e
    errorString e = show e
