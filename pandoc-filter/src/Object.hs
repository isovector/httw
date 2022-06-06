{-# OPTIONS_GHC -Wall #-}

module Object where

import           Data.Text (Text)
import qualified Data.Text as T
import           Objects.Figure
import           Objects.Internal.Types
import           Objects.Tree
import           Text.Pandoc.Definition

------------------------------------------------------------------------------

-- NOTE: When adding a new block object, make sure to also update
-- 'parseSomeBlockObjectType'
data BlockObjectType a where
  STree :: BlockObjectType (Figure (LRose String))


parseSomeBlockObjectType :: Text -> Maybe SomeBlockObjectType
parseSomeBlockObjectType "Obj/Tree" = Just (SomeBlockObjectType STree)
parseSomeBlockObjectType _ = Nothing


------------------------------------------------------------------------------

deriving instance Show (BlockObjectType a)

data SomeBlockObjectType where
  SomeBlockObjectType
    :: IsBlockObject a => BlockObjectType a -> SomeBlockObjectType


------------------------------------------------------------------------------

data SomeBlockObject where
  SomeBlockObject
    :: IsBlockObject a => BlockObjectType a -> a -> SomeBlockObject

instance Show SomeBlockObject where
  show (SomeBlockObject ty a) = unwords
    [ "(SomeBlockObject"
    , show ty
    , showsPrec 10 a ")"
    ]


------------------------------------------------------------------------------

parseSomeBlockObject :: Block -> Maybe SomeBlockObject
parseSomeBlockObject (DefinitionList [(Strs objname, bs)]) =
  case parseSomeBlockObjectType objname of
    Nothing -> Nothing
    Just (SomeBlockObjectType (ty :: BlockObjectType a)) -> case fromBlocks @a bs of
      Left err ->
        error $ "Couldn't parse " <> T.unpack objname <> "\n\n" <> err
      Right bot ->
        Just $ SomeBlockObject ty bot
parseSomeBlockObject _ = Nothing


------------------------------------------------------------------------------

walkIt :: Block -> IO Block
walkIt b = case parseSomeBlockObject b of
  Nothing -> pure b
  Just (SomeBlockObject _ a) -> toBlockObject a

