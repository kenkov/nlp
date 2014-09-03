-- module Ngram
-- ( ngram2
-- ) where

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Text as T
import System.IO
import System.Environment (getArgs)
import qualified Codec.Binary.UTF8.String as C


ngram2 :: [String] -> [(String, String)]
ngram2 xs = ngram2' (["<s>"] ++ xs ++ ["</s>"])

ngram2' :: [String] -> [(String, String)]
ngram2' xs | length xs >= 2 =
    zip xs (tail xs)

showNgram :: [(String, String)] -> String
showNgram xs = intercalate " " $ map (\(x, y) -> "(" ++ C.encodeString x ++ " " ++ y ++ ")") xs

main :: IO ()
main = do
    ls <- getContents
    -- example: $ cat -n10 filename | ./ngram
    mapM_ putStrLn [showNgram $ ngram2 [T.unpack y | y <- T.splitOn (T.pack " ") (T.pack x)] | x <- lines ls]
