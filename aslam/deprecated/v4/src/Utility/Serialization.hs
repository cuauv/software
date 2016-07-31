module Utility.Serialization where

import qualified Codec.Compression.Lzma           as LZMA
import qualified Codec.Compression.Zlib           as ZLIB
import qualified Data.Aeson                       as A
import qualified Data.Aeson.TH                    as A
import qualified Data.Aeson.Types                 as A
import qualified Data.Attoparsec.ByteString       as AT
import qualified Data.Attoparsec.ByteString.Char8 as AT
import qualified Data.Attoparsec.Lazy             as ATL
import qualified Data.Binary                      as B
import qualified Data.Binary.Get                  as B
import qualified Data.Binary.Put                  as B
import qualified Data.ByteString.Base64           as B64
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Char
import qualified Data.Hashable                    as H
import qualified Data.HashMap.Strict              as M
import qualified Data.Set                         as S
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.UUID                        as U
import qualified Data.Vector                      as V
import           GHC.Generics
import qualified Language.Haskell.TH              as TH

import           Utility.Control

{- General -}

data Encoding = Encoding {
  encodingFormat      :: Format,
  encodingCompression :: Compression
} deriving (Show, Eq, Generic)

data Format = JSON | Binary deriving (Show, Eq, Generic)

data Compression = None | LZMA | ZLIB deriving (Show, Eq, Generic)

class Serializable a where
  serialize :: Encoding -> a -> BL.ByteString
  deserialize :: Encoding -> BL.ByteString -> Maybe a

class GSerializable' a where
  serialize' :: Encoding -> a x -> BL.ByteString
  deserialize' :: Encoding -> BL.ByteString -> Maybe (a x)

{- Helper -}

autocompress None = id
autocompress LZMA = LZMA.compress
autocompress ZLIB = ZLIB.compress

autodecompress None = id
autodecompress LZMA = LZMA.decompress
autodecompress ZLIB = ZLIB.decompress

{- Instances -}

instance (A.GToJSON a, A.GFromJSON a, B.GBinary a) => GSerializable' a where
  serialize' (Encoding JSON c) = autocompress c . A.encode . A.gToJSON aesonOptions
  serialize' (Encoding Binary c) = autocompress c . B.runPut . B.gput
  deserialize' (Encoding JSON c) = decodeWith jsonEOF (A.parse $ A.gParseJSON aesonOptions) . autodecompress c
  deserialize' (Encoding Binary c) = binaryTrans . B.runGetOrFail B.gget . autodecompress c

serializeGeneric :: (Generic a, GSerializable' (Rep a)) => Encoding -> a -> BL.ByteString
serializeGeneric e x = serialize' e $ from x

deserializeGeneric :: (Generic a, GSerializable' (Rep a)) => Encoding -> BL.ByteString -> Maybe a
deserializeGeneric e x = to |<< deserialize' e x

serializeDef (Encoding JSON c) = autocompress c . A.encode
serializeDef (Encoding Binary c) = autocompress c . B.encode
deserializeDef (Encoding JSON c) = A.decode . autodecompress c
deserializeDef (Encoding Binary c) = binaryTrans . B.runGetOrFail B.get . autodecompress c

instance Serializable Int where serialize = serializeDef; deserialize = deserializeDef
instance Serializable Bool where serialize = serializeDef; deserialize = deserializeDef
instance Serializable Double where serialize = serializeDef; deserialize = deserializeDef
instance Serializable T.Text where serialize = serializeDef; deserialize = deserializeDef
instance Serializable U.UUID where serialize = serializeDef; deserialize = deserializeDef
instance Serializable B.ByteString where serialize = serializeDef; deserialize = deserializeDef
instance (B.Binary a, A.ToJSON a, A.FromJSON a, Ord a) => Serializable (S.Set a) where serialize = serializeDef; deserialize = deserializeDef
instance (B.Binary b, A.ToJSON b, A.FromJSON b) => Serializable (M.HashMap T.Text b) where serialize = serializeDef; deserialize = deserializeDef

instance (B.Binary a, B.Binary b, Eq a, H.Hashable a) => B.Binary (M.HashMap a b) where
  put = B.put . M.toList
  get = M.fromList |<< B.get

instance (B.Binary a) => B.Binary (V.Vector a) where
  put = B.put . V.toList
  get = V.fromList |<< B.get

instance A.ToJSON U.UUID where
  toJSON = A.toJSON . U.toText

instance A.FromJSON U.UUID where
  parseJSON (A.String x) =
    case U.fromText x of
      Just uuid -> pure uuid
      Nothing   -> mempty
  parseJSON _ = mempty

instance A.ToJSON B.ByteString where toJSON = A.toJSON . T.decodeUtf8 . B64.encode
instance A.FromJSON B.ByteString where parseJSON x = A.parseJSON x >>= either fail return . B64.decode . T.encodeUtf8

{- Aux -}

jsonEOF = A.json <* AT.skipSpace <* AT.endOfInput

decodeWith p to s =
  case ATL.parse p s of
    ATL.Done _ v ->
      case to v of
        A.Success a -> Just a
        _           -> Nothing
    _               -> Nothing

decodeStrictWith p to s =
  case either A.Error to (AT.parseOnly p s) of
    A.Success a -> Just a
    _           -> Nothing

binaryTrans (Right (_, _, v)) = return v
binaryTrans _ = Nothing

aesonOptions = A.Options {
  A.fieldLabelModifier = map toLower . dropWhile isLower,
  A.constructorTagModifier = map toLower,
  A.allNullaryToStringTag = True,
  A.omitNothingFields = True,
  A.sumEncoding = A.ObjectWithSingleField,
  A.unwrapUnaryRecords = False
}

{- TH -}

deriveSerialization n = do
  let typeCon      = TH.ConT n
      jsonEncoding = TH.FunD 'A.toEncoding [TH.Clause [] (TH.NormalB (TH.AppE (TH.VarE 'A.genericToEncoding) (TH.VarE 'aesonOptions))) []]
      toJSONf      = TH.FunD 'A.toJSON [TH.Clause [] (TH.NormalB (TH.AppE (TH.VarE 'A.genericToJSON) (TH.VarE 'aesonOptions))) []]
      toJSON       = TH.InstanceD [] (TH.AppT (TH.ConT ''A.ToJSON) typeCon) [jsonEncoding, toJSONf]
      parseJSONf   = TH.FunD 'A.parseJSON [TH.Clause [] (TH.NormalB (TH.AppE (TH.VarE 'A.genericParseJSON) (TH.VarE 'aesonOptions))) []]
      fromJSON     = TH.InstanceD [] (TH.AppT (TH.ConT ''A.FromJSON) typeCon) [parseJSONf]
      binary       = TH.InstanceD [] (TH.AppT (TH.ConT ''B.Binary) typeCon) []
      serializef   = TH.FunD 'serialize [TH.Clause [] (TH.NormalB $ TH.VarE 'serializeGeneric) []]
      deserializef = TH.FunD 'deserialize [TH.Clause [] (TH.NormalB $ TH.VarE 'deserializeGeneric) []]
      serializable = TH.InstanceD [] (TH.AppT (TH.ConT ''Serializable) typeCon) [serializef, deserializef]
  return [toJSON, fromJSON, binary, serializable]
