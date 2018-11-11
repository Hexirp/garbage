module Main where
 import Prelude
 import System.IO

 main :: IO ()
 main = putStrLn "" >> game 0

 game :: Int -> IO ()
 game n = do
  hSetBuffering stdout NoBuffering
  putStrLn $ show n ++ " にて起動されました"
  putStrLn "好きな文字列を入力してください..."
  a <- getChar
  case a of
   ' ' -> return ()
   _ -> game (n + 1)
