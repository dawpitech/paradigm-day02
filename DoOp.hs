{-
-- EPITECH PROJECT, 2025
-- PARADIGM-DAY02
-- File description:
-- DoOp.hs
-}

module Main where

import Data.Char (ord)
import Data.Maybe (isNothing)
import Distribution.ModuleName (main)
import System.Directory.Internal.Prelude (getArgs, exitFailure)
import Distribution.Compat.Prelude (exitSuccess, ExitCode (ExitSuccess, ExitFailure), exitWith)
import Data.Binary.Builder (putInt16host)

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem n (e:es) | n == e = True
                | otherwise = myElem n es

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (div a b)

safeNth :: [a] -> Int -> Maybe a
safeNth [] i     = Nothing
safeNth (a:as) 0 = Just a
safeNth (a:as) i | i < 0 = Nothing
                 | otherwise = safeNth as (i - 1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing  = Nothing
safeSucc (Just e) = Just $ (+) e 1

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ []          = Nothing
myLookup r ((i, v):es) | i == r = Just v
                       | otherwise = myLookup r es

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo _ Nothing _         = Nothing
maybeDo _ _ Nothing         = Nothing
maybeDo f (Just a) (Just b) = Just $ f a b

readInt :: [Char] -> Maybe Int
readInt []       = Nothing
readInt [c]      | ord c < 48 || ord c > 57 = Nothing
                 | otherwise = Just $ ord c - 48
readInt ('-':cs) = readInt cs >>= (\r -> Just $ r * (-1))
readInt (c:cs)   | ord c < 48 || ord c > 57 = Nothing
                 | otherwise = rest >>= (\r -> Just $ r + value)
                where
                  value = (ord c - 48) * (10 ^ length cs)
                  rest = readInt cs

getLineLength :: IO Int
getLineLength = getLine >>= (\l -> return $ length l)

printAndGetLength :: String -> IO Int
printAndGetLength str = do
    _ <- putStrLn str
    return $ length str

printBoxLine :: Int -> IO ()
printBoxLine 0 = return ()
printBoxLine 1 = putStrLn "+"
printBoxLine n = putChar '-' >> printBoxLine (n - 1)

printBoxWall :: Int -> IO ()
printBoxWall 0 = return ()
printBoxWall 1 = putStrLn "|"
printBoxWall n = putChar ' ' >> printBoxWall (n - 1)

printBoxInternal :: Int -> Int -> IO ()
printBoxInternal n s | n <= 0 = return ()
                     | otherwise = putChar '|' >>
                                   printBoxWall (s * 2 - 1) >>
                                   printBoxInternal (n - 1) s

printBox :: Int -> IO ()
printBox 1 = putStrLn "++"
printBox n | n <= 0 = return ()
           | otherwise = putChar '+' >>
                         printBoxLine (n * 2 - 1) >>
                         printBoxInternal (n - 2) n >>
                         putChar '+' >>
                         printBoxLine (n * 2 - 1) >>
                         return ()

concatLines :: Int -> IO String
concatLines n | n <= 0 = return ""
              | otherwise = do
                            line <- getLine
                            others <- concatLines $ n - 1
                            return $ line ++ others

getInt :: IO (Maybe Int)
getInt = readInt <$> getLine

maybeSafeDiv :: Maybe Int -> Maybe Int -> Maybe Int
maybeSafeDiv _ (Just 0)        = Nothing
maybeSafeDiv (Just a) (Just b) = Just (div a b)
maybeSafeDiv _ _               = Nothing

maybeSafeMod :: Maybe Int -> Maybe Int -> Maybe Int
maybeSafeMod _ (Just 0)        = Nothing
maybeSafeMod (Just a) (Just b) = Just (mod a b)
maybeSafeMod _ _               = Nothing

doop :: [String] -> Maybe Int
doop [ a, "+", b ] = maybeDo (+) (readInt a) (readInt b)
doop [ a, "-", b ] = maybeDo (+) (readInt a) (readInt b)
doop [ a, "*", b ] = maybeDo (*) (readInt a) (readInt b)
doop [ a, "/", b ] = maybeSafeDiv (readInt a) (readInt b)
doop [ a, "%", b ] = maybeSafeMod (readInt a) (readInt b)
doop [ a, _ , b ]  = Nothing

computeExit :: Maybe Int -> IO ()
computeExit Nothing  = exitWith (ExitFailure 84)
computeExit (Just i) = print i

main :: IO ()
main = do
    args <- getArgs
    computeExit (doop args)
