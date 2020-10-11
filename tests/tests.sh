#!/bin/bash

seq 1 10 | hwk 'map ("number " ++)'
echo System.FilePath
seq 1 10 | hwk -l '(<.> "txt")'
echo
seq -100 100 | hwk 'sum . filter (<0) . ints'
echo
seq 0 2 | hwk --line '(++ ".txt") . show . (+1) . int'
echo
seq 10 12 | hwk --line 'let {fact 0 = 1; fact n = n * fact (n - 1)} in fact . int'
echo Data.List.Extra
cat /etc/passwd | hwk --line 'reverse . filter (/= "x") . take 3 . splitOn ":"' | head -3
echo
hwk -t 'intercalate "\t"'
echo Data.Bool
cat /etc/passwd | hwk --all 'bool "no" "yes" . isInfixOf "1000"'
