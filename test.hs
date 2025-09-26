-- ghci> 
-- ghci> parse (totParser intexp) "" "42"
-- Right (Const 42)
-- ghci> parse (totParser factor) "" "42"


import Text.Parsec
parse (totParser comm) "" "a = 10;if a > 10 {    t = 50} else {b++};repeat {    a++} until a > 11+3"
parse (totParser comm) "" ""