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

boolexp :: Parser (Exp Bool)
boolexp = undefined

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = undefined


------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
