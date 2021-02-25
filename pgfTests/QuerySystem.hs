module Main where

import PGF
import Answer (transfer,transfer2,Mode)

main :: IO ()
main = do
  gr <- readPGF "Query.pgf"
  loop (translate transfer gr) (translate transfer2 gr)

loop :: (String -> String) -> (String -> String) -> IO ()
loop trans trans2 = do
  s <- getLine
  if s == "quit" then putStrLn "bye" else do
    putStrLn $ ("  " ++ trans s)
    putStrLn $ ("  " ++ trans2 s)
    loop trans trans2

translate :: (Tree -> Tree) -> PGF -> String -> String
translate tr gr s =
  case parseAllLang gr (startCat gr) s of
    (lg,t:_):_ -> linearize gr lg (tr t)
    -- (lg,t:_):_ -> unlines [ linearize gr lg tree | tree <- transfers t]
    _ -> "NO PARSE"
    -- where
    --   transfers t = [tr | m <- [MNone, MAns]]


-- (lg,t:_):_ -> unlines [ linearize gr lg tree | tree <- tr t]
-- tr t = [transfer m t | m <- [MNone, MAns]]

-- (lg,t:_):_ -> linearize gr lg (tr  t)

-- loop :: (String -> String) -> IO ()
-- loop trans = do
--   s <- getLine
--   if s == "quit" then putStrLn "bye" else do
--     putStrLn $ trans s
--     loop trans

-- translate :: (Tree -> Tree) -> PGF -> String -> String
-- translate tr gr s =
--   case parseAllLang gr (startCat gr) s of
--     (lg,t:_):_ -> linearize gr lg (tr t)
--     _ -> "NO PARSE"

