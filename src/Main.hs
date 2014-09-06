
module Main where

--import qualified Data.ByteString
--import Data.Array.Unboxed
--import Text.ParserCombinators.Parsec
--import Control.Monad.State
--import Control.Exception

import           Control.Arrow
import           Control.Monad
import           System.IO

-- | The main entry point.
main :: IO ()
main = do
    --contents <- getTildeContents
    contents <- readFile "inputtext.txt"
    print $ wc contents

getTildeContents :: IO String
getTildeContents = liftM (takeTill '~') getContents

takeTill :: Eq a => a -> [a] -> [a]
takeTill n (x:xs) = if x == n then []
                        else x:(takeTill n xs)

wc s = count &&& countWords &&& countLines $ s

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
countWords input = length $ splitBy isSpace input

isSpace :: Char -> Bool
isSpace a = elem a "\n \t\r"

splitBy f s = case rest of
                []     -> [chunk]
                _:rest -> case chunk of
                            [] -> splitBy f rest
                            _ -> chunk : splitBy f rest
  where (chunk, rest) = break f s
