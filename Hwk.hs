module Hwk where

-- | modules to be imported in hint interpreter
-- These modules must be from installed packages
userModules :: [String]
userModules = [ "Prelude", "Data.List", "Data.Char", "Data.Bool"
              , "Data.List.Extra", "Data.Tuple.Extra"
              , "System.FilePath"
              ]

int :: String -> Int
int str = read str :: Int

ints :: [String] -> [Int]
ints = map int
