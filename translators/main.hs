module Main where

import Tests
import Text.Read
import Data.IntMap.Strict as Map
import PrintAgda
import PrintIdris
import PrintLean
import PrintRocq

_maxsize :: Int
_maxsize = 1000000

chooseTest :: String -> IO Int
chooseTest str = case readMaybe str :: Maybe Int of
    Just i ->
        if i > 0 || i <= length tests then return i else 
            do
                putStrLn "test choice out of bounds"
                getLine >>= chooseTest
    Nothing ->
        do
            putStrLn $ "not a valid test choice. enter an integer from 1 to " ++ show (length tests)
            getLine >>= chooseTest

chooseSize :: String -> IO Int
chooseSize str = case readMaybe str :: Maybe Int of
    Just i ->
        if i >= 0 || i <= _maxsize then return i else 
            do
                putStrLn "test size out of bounds"
                getLine >>= chooseSize
    Nothing ->
        do
            putStrLn $ "not a valid test size. enter an integer from 0 to " ++ show _maxsize
            getLine >>= chooseSize

main :: IO()
main = do
    putStrLn "choose test: " -- todo: show detailed options and index
    i <- getLine >>= chooseTest
    putStrLn "choose size: "
    n <- getLine >>= chooseSize
    let test = (tests ! i) n
    runAgda test
    runIdris test
    runLean test
    runRocq test