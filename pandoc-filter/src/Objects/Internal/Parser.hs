module Objects.Internal.Parser
  ( module Objects.Internal.Parser
  , module Control.Monad
  ) where

import Control.Monad
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer
import Data.Foldable (asum)


sc :: Parser ()
sc = space space1 (skipLineComment "--") $ skipBlockComment "{-" "-}"

type Parser = Parsec Void Text

sym :: Text -> Parser ()
sym = void . symbol sc


str :: Parser String
str = lexeme sc $ char '"' >> manyTill charLiteral (char '"')

surrounded :: Parser a -> Parser b -> Parser x -> Parser x
surrounded start end x = start *> x <* end

optionalSurround :: Parser a -> Parser b -> Parser x -> Parser x
optionalSurround start end x = asum
  [ surrounded start end x
  , x
  ]

