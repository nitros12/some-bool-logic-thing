{-# LANGUAGE TemplateHaskell #-}
-- | datatype decl

module AST where

import           Data.Data                      ( Data
                                                , Typeable
                                                )
import           Data.Generics.Uniplate.Data
import           Text.Show.Deriving
import           Data.Functor
import           Control.Applicative.Combinators
                                               as C
import           Control.Monad.Combinators.Expr
import           Control.Applicative            ( (<|>) )
import           Data.Void
import           Text.Megaparsec               as M
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import qualified Data.Text                     as T

data Logic
  = And Logic Logic
  | Or Logic Logic
  | Not Logic
  | Ident Char
  | Literal Bool
  deriving (Eq, Data, Typeable)

instance Show Logic where
  showsPrec d (And a b) = showParen (d > and_prec) $
                          showsPrec (and_prec + 1) a .
                          showsPrec (and_prec + 1) b
    where and_prec = 7

  showsPrec d (Or a b) = showParen (d > or_prec) $
                         showsPrec (or_prec + 1) a .
                         showString " + " .
                         showsPrec (or_prec + 1) b
    where or_prec = 6

  showsPrec d (Not a) = showParen (d > not_prec) $
                        showsPrec (not_prec + 1) a .
                        showChar '\''
    where not_prec = 8

  showsPrec d (Ident c) = showChar c
  showsPrec d (Literal True) = showChar '1'
  showsPrec d (Literal False) = showChar '0'

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

literalTrue, literalFalse :: Parser Logic
literalTrue = alternateLit ["t", "True", "true", "1"] $> Literal True
literalFalse = alternateLit ["f", "False", "false", "0"] $> Literal False

alternateLit :: [String] -> Parser ()
alternateLit = foldl1 (<|>) . map symbolOrRword
 where
  symbolOrRword [x] = symbol [x] $> ()
  symbolOrRword xs  = rword xs

identifier :: Parser Logic
identifier = (lexeme . M.try) (Ident <$> letterChar)

expr :: Parser Logic
expr = makeExprParser terms operators

operators :: [[Operator Parser Logic]]
operators =
  [ [ Prefix (Not <$ alternateLit ["not", "~"])
    , Postfix (Not <$ alternateLit ["'", "¬ "])
    ]
  , [ InfixL (And <$ alternateLit ["and", ".", "*"])
    , InfixL (And <$ pure ())
    ] -- This lets us parse abc into (a and b) and c
  , [InfixL (Or <$ alternateLit ["or", "+"])]
  ]

terms :: Parser Logic
terms = parens expr <|> literalTrue <|> literalFalse <|> identifier
