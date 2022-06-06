{-# LANGUAGE UndecidableInstances #-}

module Objects.Tree where

import Text.Pandoc.Definition
import GHC.Generics
import Cache (caching, hashFile)
import Data.Text (Text)
import Control.Arrow ((&&&))
import qualified Data.Text as T
import Objects.Internal.Dot
import Data.Foldable (for_)
import System.Process (callProcess)
import Data.Hashable (Hashable)

class Show a => IsBlockObject a where
  fromBlocks :: [[Block]] -> Either String a
  toBlockObject :: a -> IO Block

class Read a => IsInlineObject a where
  toInlineObject :: String -> a -> IO [Inline]

data LRose a = LPure a | LRose a [LRose a]
  deriving stock (Eq, Read, Ord, Show, Functor, Generic, Generic1, Foldable, Traversable)

data Figure a = Figure
  { f_id    :: Text
  , f_label :: Text
  , f_thing :: a
  }
  deriving stock (Eq, Read, Ord, Show, Functor, Generic, Generic1, Foldable, Traversable)

instance Hashable a => Hashable (LRose a) where

instance (Hashable a, ToDot a, Show a, Read a) => IsBlockObject (Figure (LRose a)) where
  fromBlocks [[Para (Strs fid)], [Para (Strs label)], [Para (Strs obj)]]
    = Right $ Figure fid label $ read $ T.unpack obj
  fromBlocks z = Left $ "Bad format:\n" <> show z
  toBlockObject (Figure fid lbl t) = do
    fp <- doDot t
    pure $
      -- Magic incantation to make a latex figure
      Div ("", [], [])
        [ Para [Image (fid, [], []) [Str lbl] (T.pack fp , "fig:")]
        ]



doDot
    :: (ToDot a, Hashable a)
    => a
    -> IO FilePath
doDot a = do
  let fp = hashFile a <> ".png"
  writeFile "/tmp/output.dot" $ runDotM $ toDot a

  callProcess "dot"
    [ "-Tpng"
    , "-Gdpi=300"
    , "-o/tmp/out.png"
    , "/tmp/output.dot"
    ]
  callProcess "convert"
    [ "/tmp/out.png"
    , "-density"
    , "300"
    , "-units"
    , "pixelsperinch"
    , "-resize"
    , "40%"
    , fp
    ]

  pure fp


walkIt :: Block -> IO Block
walkIt b = case parseSomeBlockObject b of
  Nothing -> pure b
  Just (SomeBlockObject _ a) -> toBlockObject a


data BlockObjectType a where
  STree :: BlockObjectType (Figure (LRose String))

deriving instance Show (BlockObjectType a)

data InlineObjectType a where

data SomeBlockObjectType where
  SomeBlockObjectType :: IsBlockObject a => BlockObjectType a -> SomeBlockObjectType

instance Show SomeBlockObject where
  show (SomeBlockObject ty a) = unwords
    [ "(SomeBlockObject"
    , show ty
    , showsPrec 10 a ")"
    ]


data SomeBlockObject where
  SomeBlockObject :: IsBlockObject a => BlockObjectType a -> a -> SomeBlockObject

parseSomeBlockObjectType :: Text -> Maybe SomeBlockObjectType
parseSomeBlockObjectType "Obj/Tree" = Just (SomeBlockObjectType STree)
parseSomeBlockObjectType _ = Nothing

pattern Strs :: Text -> [Inline]
pattern Strs ts <-
  ((id &&& id)
    ->
      ( all isStr -> True
      , foldMap fromStr -> ts
      )
  )

isStr :: Inline -> Bool
isStr (Str _) = True
isStr Space = True
isStr (Quoted _ x) = all isStr x
isStr _ = False

fromStr :: Inline -> Text
fromStr (Str s) = s
fromStr (Quoted SingleQuote s) = "'" <> foldMap fromStr s <> "'"
fromStr (Quoted DoubleQuote s) = "\"" <> foldMap fromStr s <> "\""
fromStr Space = " "
fromStr _ = error "fromStr called on not a str"

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



instance ToDot a => ToDot (LRose a) where
  toDot (LPure a) = toDot a
  toDot (LRose a ros) = do
    ns <- traverse toDot ros
    me <- toDot a
    for_ ns $ addEdge me
    pure me







cache :: (a -> IO Block) -> String -> a -> IO Block
cache f s = caching s . f


test :: Block
test =
  DefinitionList
      [ ( [ Str "Obj/Tree" ]
        , [ [ Para [Str "fig:fid"] ]
          , [ Para
                [ Str "I"
                , Space
                , Str "am"
                , Space
                , Str "a"
                , Space
                , Str "label"
                ]
            ]
          , [ Para
                [ Str "LRose"
                , Space
                , Quoted DoubleQuote [ Str "Alan" ]
                , Space
                , Str "[LPure"
                , Space
                , Quoted DoubleQuote [ Str "Sandy" ]
                , Str "]"
                ]
            ]
          ]
        )
      ]

