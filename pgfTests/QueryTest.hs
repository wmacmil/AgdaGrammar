module Main where

import PGF
import Answer (transferAll)

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
       putStrLns answerDescriptions gr s
       putStrLn $ ""
    )
    ss
  where
    putStrLns :: [String] -> PGF -> String -> IO [()]
    putStrLns ss pgf s = mapM putStrLn (zipWith (++) ss (translateAll transferAll pgf s))
    answerDescriptions :: [String]
    answerDescriptions = map (\x -> "  " ++ x ++ " : ") ["Simple","Verbose","Compressed"]

translateAll :: [Tree -> Tree] -> PGF -> String -> [String]
translateAll trs gr s = map (\x -> translate x gr s) trs

translate :: (Tree -> Tree) -> PGF -> String -> String
translate tr gr s =
  case parseAllLang gr (startCat gr) s of
    (lg,t:_):_ -> linearize gr lg (tr t)
    _ -> "NO PARSE"
