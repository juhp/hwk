{-# LANGUAGE FlexibleInstances #-}

module Hwk where

import Data.List (intercalate)

-- | modules to be imported into hint
-- these modules must be from installed packages
userModules :: [String]
userModules = [ "Prelude", "Data.List", "Data.Char", "Data.Bool"
              -- , "Data.List.Extra", "Data.Tuple.Extra"
              , "System.FilePath"
              ]

-- you can put user hwk functions here:


int :: String -> Int
int str = read str :: Int

ints :: [String] -> [Int]
ints = map int

------ below here all required internal machinery ------

-- | toString and toList are required by hwk
-- They determine the types of functions hwk can interpret,
-- allowing some simple polymorphism.

-- ToString allows handling functions of type: ToString a => String -> a
class ToString a where
  toString :: a -> String
instance ToString String where
  toString x = x
instance ToString [String] where
  toString = unwords
instance ToString [[String]] where
  toString = intercalate "\t" . map unwords
instance ToString Int where
  toString x = show x
instance ToString [Int] where
  toString lst = unwords $ map show lst

-- ToList allows handling functions of type: ToList a => [String] -> a
class ToList a where
  toList :: a -> [String]
instance ToList String where
  toList x = [x]
instance ToList [String] where
  toList = id
instance ToList [[String]] where
  toList = map (intercalate "\t")
instance ToList Int where
  toList x = [show x]
instance ToList [Int] where
  toList lst = map show lst
