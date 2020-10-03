import Control.Monad.Extra
import qualified Data.List as L
import Data.Maybe
import Language.Haskell.Interpreter
import SimpleCmdArgs
import System.Directory
import System.FilePath

import Paths_hwk (getDataDir, version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "A Haskell awk/sed like tool"
    "Simple shell text processing with Haskell" $
  runExpr <$> switchWith 'a' "" "Output Haskell" <*> optional (strOptionWith 'f' "file" "FILE" "User function include file") <*> strArg "FUNCTION" <*> many (strArg "FILE...")

runExpr :: Bool -> Maybe FilePath -> String -> [FilePath] -> IO ()
runExpr _all mfile stmt files = do
  mapM_ checkFileExists files
  input <- lines <$> getContents
  usercfg <- getAppUserDataDirectory "hwk.hs"
  unlessM (doesFileExist usercfg) $ do
    datadir <- getDataDir
    copyFile (datadir </> "hwk.hs") usercfg
  -- FIXME check file exists
  let mods = usercfg : maybeToList mfile
  r <- runInterpreter (runHint mods input)
  case r of
    Left err -> putStrLn $ errorString err
    Right () -> return ()
  where
    runHint :: [FilePath] -> [String] -> Interpreter ()
    runHint mods input = do
      loadModules mods
      setImports ["Prelude", "Data.List", "Data.Char", "Hwk"]
      -- -- Not sure which is better: typechecking manually or polymorph string
      -- etypchk <- typeChecksWithDetails stmt
      -- case etypchk of
      --   Left err -> error' err
      --   Right typ ->
      --     case typ of
      poly <- do
        havePoly <- typeChecks "polymorph"
        if havePoly then do
          s <- interpret "polymorph" infer
          return $ if null s then "" else s ++ " . "
          else return ""
      fn <- interpret (poly ++ stmt) (as :: [String] -> [String])
      liftIO $ mapM_ putStrLn (fn input)

    checkFileExists :: FilePath -> IO ()
    checkFileExists file = do
      unlessM (doesFileExist file) $
        error' $ "file not found: " ++ file

    errorString :: InterpreterError -> String
    errorString (WontCompile es) = L.intercalate "\n" (header : map unbox es)
      where
        header = "ERROR: Won't compile:"
        unbox (GhcError e) = e
    errorString e = show e
