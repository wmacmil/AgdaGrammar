module Main where

import PGF
import Answer (transfer,transfer2,transfer3)

main :: IO ()
main = do
  gr <- readPGF "Query.pgf"
  loop (translate transfer gr) (translate transfer2 gr) (translate transfer3 gr)

--TODO interact function

loop :: (String -> String) -> (String -> String) -> (String -> String) -> IO ()
loop trans trans2 trans3 = do
  s <- getLine
  if s == "quit" then putStrLn "bye" else do
    putStrLn $ ("  Simple:      " ++ trans s)
    putStrLn $ ("  Verbose:     " ++ trans2 s)
    putStrLn $ ("  Compressed:  " ++ trans3 s)
    loop trans trans2 trans3

translate :: (Tree -> Tree) -> PGF -> String -> String
translate tr gr s =
  case parseAllLang gr (startCat gr) s of
    (lg,t:_):_ -> linearize gr lg (tr t)
    -- (lg,t:_):_ -> unlines [ linearize gr lg tree | tree <- transfers t]
    _ -> "NO PARSE"
    -- where
    --   transfers t = [tr | m <- [MNone, MAns]]
