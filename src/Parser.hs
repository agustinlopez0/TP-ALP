module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
-- ghci> import Text.Parsec
-- ghci> parse (totParser intexp) "" "42"
-- Right (Const 42)
-- ghci> parse (totParser factor) "" "42"

factor :: Parser (Exp Int)
factor =
      parens lis intexp -- Si hay parentesis parsea el intexp de adentro
  <|> (Const . fromInteger <$> natural lis) -- Parsea un natural y lo pasa a Int y construye con Cons
  <|> try (do v <- identifier lis
              reservedOp lis "++"
              return (VarInc v)) -- Revisa var++
  <|> (Var <$> identifier lis) -- Sino solo variable
  <|> (reservedOp lis "-" >> UMinus <$> factor)

term :: Parser (Exp Int)
term = do
  f <- factor
  rest f
 where
  rest f =
        (do reservedOp lis "*"
            t <- term
            return (Times f t))
    <|> (do reservedOp lis "/"
            t <- term
            return (Div f t))
    <|> return f


intexp :: Parser (Exp Int)
intexp = do
  t <- term
  rest t
 where
  rest t =
        (do reservedOp lis "+"
            e <- intexp
            return (Plus t e))
    <|> (do reservedOp lis "-"
            e <- intexp
            return (Minus t e))
    <|> return t

------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolfactor :: Parser (Exp Bool)
boolfactor =
      parens lis boolexp -- Si hay parentesis parsea el intexp de adentro
  <|> (reserved lis "true" >> return BTrue)
  <|> (reserved lis "false" >> return BFalse)
  <|> (reservedOp lis "!" >> Not <$> boolexp)


boolcomp :: Parser (Exp Bool)
boolcomp = do
    t <- intexp
    rest t
  where
    rest t =
          (do reservedOp lis "=="
              e <- intexp
              return (Eq t e))
      <|> (do reservedOp lis "!="
              e <- intexp
              return (NEq t e))
      <|> (do reservedOp lis "<"
              e <- intexp
              return (Lt t e))
      <|> (do reservedOp lis ">"
              e <- intexp
              return (Gt t e))


boolexp :: Parser (Exp Bool)
boolexp = do
    b <- (try boolcomp) <|> boolfactor
    rest b
  where
  rest b =
        (do reservedOp lis "&&"
            e <- boolexp
            return (And b e))
    <|> (do reservedOp lis "||"
            e <- boolexp
            return (Or b e))
    <|> return b

-----------------------------------
--- Parser de comandos
-----------------------------------

skip :: Parser Comm
skip = do 
    reserved lis "skip"
    return Skip  

assig :: Parser Comm
assig = do 
    v <- identifier lis
    reservedOp lis "="
    i <- intexp
    return (Let v i)

seqOp :: Parser (Comm -> Comm -> Comm)
seqOp = do
  reservedOp lis ";"
  return Seq

ifthenelse :: Parser Comm
ifthenelse = do
    reserved lis "if"
    b <- boolexp
    c <- braces lis comm
    reserved lis "else"
    c' <- braces lis comm
    return (IfThenElse b c c')

repeatcomm :: Parser Comm
repeatcomm = do
    reserved lis "repeat"
    c <- braces lis comm
    reserved lis "until"
    b <- boolexp
    return (RepeatUntil c b)

commBasic :: Parser Comm
commBasic =  skip
         <|> assig
         <|> ifthenelse
         <|> repeatcomm

comm :: Parser Comm
comm = chainl1 commBasic seqOp

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
