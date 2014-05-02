module Main where

--import qualified Data.ByteString
--import Data.Array.Unboxed
--import Text.ParserCombinators.Parsec
--import Control.Monad.State
--import Control.Exception
import           Control.Monad
import           System.IO

-- | The main entry point.
main :: IO ()
main = do
    contents <- getTildeContents
    print $ countWords contents

getTildeContents :: IO String
getTildeContents = liftM (takeTill '~') getContents

takeTill :: Eq a => a -> [a] -> [a]
takeTill n (x:xs) = if x == n then []
                        else x:(takeTill n xs)


copy :: String -> String
copy = id

count :: String -> Int
count = length

countLines :: String -> Int
countLines input = foldr count1 0 input
    where
        count1 '\n' n = n + 1
        count1 _ n = n

countWords :: String -> Int
countWords input = length $ splitMany "\n \t\r" input

splitMany :: Eq a => [a] -> [a] -> [[a]]
splitMany [] list = [list]
splitMany (x:xs) list = clearEmpty . join $ (liftM $ splitMany xs) step
  where step = split x list

split :: Eq a => a -> [a] -> [[a]]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

clearEmpty :: [[a]] -> [[a]]
clearEmpty [] = []
clearEmpty ([]:xs) = clearEmpty xs
clearEmpty (x:xs) = x:(clearEmpty xs)
