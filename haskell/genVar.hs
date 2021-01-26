


import Data.List
import Data.Char

commaLets = intersperse ',' ['A'..'Z']
spaceLets = intersperse ' ' commaLets

gfVarLin x = x : " = \"" ++ [(toLower x)] ++ "\" ;" 

try = fmap (gfVarLin) ['A'..'Z']
try2 = mapM putStrLn try


whiteSpaceChar c = " " ++ c : " "

-- wsS str = fmap whiteSpaceChar str 

wsParen c =  if (c == '(') || (c == ')') then (whiteSpaceChar c) else (c : [])

funext = "funExt (A : U) (B : A -> U) (f g : (x : A) -> B x) (p : (x : A) -> Path (B x) (f x) (g x)) : Path ((y : A) -> B y) f g = undefined"

wsS str = fmap wsParen str 

wsFunext = fmap toLower (concat (wsS funext))

-- lgfVarLin x 
--   X = "x" ;





