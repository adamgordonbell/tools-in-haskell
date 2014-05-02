module Main where

--import qualified Data.ByteString
--import Data.Array.Unboxed
--import Text.ParserCombinators.Parsec
--import Control.Monad.State
--import Control.Exception
import System.IO
import Control.Monad

-- | The main entry point.
main :: IO ()
main = do
    contents <- getTildeContents
    print $ countLines  contents

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