module Main where

import PGF
import Answer (transfer,transfer2,transfer3)


-- >>> main 

-- how to ignore comments and whitespace?
-- algorithm : ignore whitespace, filter out comments but reinsert them after, 
-- idea : each test should isolate unique functionality of
-- (i) evaluation
-- (ii) translation in one direction
  -- maybe come up with a naming convention?


-- main :: IO ()
main = do
  gr <- readPGF "Query.pgf"
  s <- getContents
  let ss = zipWith (,) (lines s) ([1..(length s)])
  mapM
    (\(s,n) -> do
       putStrLn $ ("Question " ++ show n)
       putStrLn $ s
       putStrLn $ ("  Simple : " ++ ((translate transfer gr) s))
       putStrLn $ ("  Verbose : " ++ ((translate transfer2 gr) s))
       putStrLn $ ""
    )
    ss

translate :: (Tree -> Tree) -> PGF -> String -> String
translate tr gr s =
  case parseAllLang gr (startCat gr) s of
    (lg,t:_):_ -> linearize gr lg (tr t)
    _ -> "NO PARSE"
