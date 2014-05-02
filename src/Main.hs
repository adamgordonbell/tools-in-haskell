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
    c <- count
    print c

getc :: IO (Maybe Char)
getc = do
    c <- getChar
    if c == '~'
        then return Nothing
    else
        return (Just c)

putc :: Char -> IO ()
putc = putChar               

copy :: IO ()
copy = do
       ch <- getc
       case ch of
               Nothing -> return ()
               Just c -> do
                         putc c
                         copy

count :: IO Int
count = do
    ch <- getc
    case ch of 
        Nothing -> return 0
        Just c ->  do
            c1 <- count
            return $ 1 + c1