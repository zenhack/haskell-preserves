module Preserves.Text.Parse
  ( parser,
  )
where

import Data.Fix (Fix (..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text.Lazy (Text)
import Data.Void (Void)
import Preserves
import Text.Megaparsec
import Text.Megaparsec.Char
import Zhp hiding (many)

parser :: Parsec Void Text (Anno (Fix Value))
parser = pDoc

type Parser = Parsec Void Text

type ErrBundle = ParseErrorBundle Text Void

pKwd :: Text -> Parser ()
pKwd s = void $ pToken (string s)

pToken :: Parser a -> Parser a
pToken p = try p <* whitespace

whitespace :: Parser ()
whitespace = void $ takeWhileP Nothing (`elem` (" ,\t\r\n" :: String))

pDoc :: Parser (Anno (Fix Value))
pDoc = whitespace *> pAnno

pAnno :: Parser (Anno (Fix Value))
pAnno = Anno <$> many pAnno1 <*> pValue

pAnno1 :: Parser (Value (Fix Value))
pAnno1 = choice
    [ Atom . String <$> pComment
    , pKwd "@" *> pValue
    ]

pComment :: Parser Text
pComment = char ';' *> takeWhileP Nothing (/= '\n')

pValue :: Parser (Value (Fix Value))
pValue =
  choice
    [ Atom <$> pAtom,
      Compound <$> pCompound,
      Embedded <$> pEmbedded
    ]

pAtom :: Parser Atom
pAtom =
  choice
    [ Bool <$> pBool
    -- TODO
    ]

pTrue, pFalse, pBool :: Parser Bool
pTrue = pKwd "#t" *> pure True
pFalse = pKwd "#f" *> pure False
pBool = pTrue <|> pFalse

pCompound :: Parser (Compound (Fix Value))
pCompound =
  choice
    [ between (pKwd "<") (pKwd ">") $
        Record <$> pAnno <*> many pAnno,
      between (pKwd "[") (pKwd "]") $
        Sequence <$> many pAnno,
      between (pKwd "#{") (pKwd "}") $
        Set . S.fromList <$> many pAnno,
      between (pKwd "{") (pKwd "}") $
        Dictionary . M.fromList <$> many ((,) <$> pAnno <*> pAnno)
    ]

pEmbedded :: Parser (Fix Value)
pEmbedded = pKwd "#!" *> (Fix <$> pValue)
