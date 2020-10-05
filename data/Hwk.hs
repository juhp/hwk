{-# LANGUAGE FlexibleInstances #-}

module Hwk where

import Data.List (intercalate)

-- | modules to be imported into hint
-- these modules must be globally installed
userModules :: [String]
userModules = ["Prelude", "Data.List", "Data.Char"]

-- this string is used by hwk
-- | Determines the types of functions hwk can interpret
-- "toList" allows some simple polymorphism
-- use "id" or "" to allow only functions of type: [String] -> [String]
polymorph :: String
polymorph = "toList"

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

-- below here all user defined ------

int :: String -> Int
int str = read str :: Int

ints :: [String] -> [Int]
ints = map int
