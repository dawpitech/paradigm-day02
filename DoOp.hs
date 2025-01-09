{-
-- EPITECH PROJECT, 2025
-- PARADIGM-DAY02
-- File description:
-- DoOp.hs
-}

import Data.Char (ord)
import Data.Maybe (isNothing)

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
    putStrLn str
    return $ length str
