module Preserves.Text.Parse
  ( parser,
  )
where

import Data.Fix (Fix (..))
import Data.Text (Text)
import Data.Void (Void)
import Preserves
import Text.Megaparsec
import Text.Megaparsec.Char
import Zhp hiding (many)

parser :: Parsec Void Text (Anno (Fix Value))
parser = pDoc

type Parser = Parsec Void Text

type ErrBundle = ParseErrorBundle Text Void

pToken :: Parser a -> Parser a
pToken p = try p <* whitespace

whitespace :: Parser ()
whitespace = void $ takeWhileP Nothing (`elem` (" ,\t\r\n" :: String))

pDoc :: Parser (Anno (Fix Value))
pDoc = whitespace *> pAnno

pAnno :: Parser (Anno (Fix Value))
pAnno = Anno [] <$> pValue

pValue :: Parser (Value (Fix Value))
pValue = Atom <$> pAtom

pAtom :: Parser Atom
pAtom =
  choice
    [ Bool <$> pBool
    ]

pTrue, pFalse, pBool :: Parser Bool
pTrue = pToken (string "#t") *> pure True
pFalse = pToken (string "#f") *> pure False
pBool = pTrue <|> pFalse
