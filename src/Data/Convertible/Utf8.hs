{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Convertible.Utf8 where

import qualified Codec.Binary.UTF8.String  as Codec.UTF8
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Builder   as BSB
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL.UTF8
import qualified Data.ByteString.Short     as BSS
import qualified Data.ByteString.UTF8      as BS.UTF8
import           Data.Maybe                (fromJust)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Builder    as TB
import qualified Data.Text.Lazy.Encoding   as TLE
import qualified Data.Text.Short           as TS

-- | A typeclass that represents something that can be converted.
--  A @Convertible a b@ instance represents an @a@ that can be converted to a @b@.
class Convertible a b where
  -- | Convert from @a@ to @b@.
  --    Byte string is assumed to be in utf-8 encoding.
  convert :: a -> b

type Text = T.Text
type LazyText = TL.Text
type TextBuilder = TB.Builder
type ShortText = TS.ShortText
type ByteString = BS.ByteString
type LazyByteString = BSL.ByteString
type ByteStringBuilder = BSB.Builder
type ShortByteString = BSS.ShortByteString

-- Convert from String

instance Convertible String String where
  {-# INLINE convert #-}
  convert = id

instance Convertible String Text where
  {-# INLINE convert #-}
  convert = T.pack

instance Convertible String LazyText where
  {-# INLINE convert #-}
  convert = TL.pack

instance Convertible String TextBuilder where
  {-# INLINE convert #-}
  convert = TB.fromString

instance Convertible String ShortText where
  {-# INLINE convert #-}
  convert = TS.fromString

instance Convertible String ByteString where
  {-# INLINE convert #-}
  convert = BS.UTF8.fromString

instance Convertible String LazyByteString where
  {-# INLINE convert #-}
  convert = BSL.UTF8.fromString

instance Convertible String ByteStringBuilder where
  {-# INLINE convert #-}
  convert = BSB.stringUtf8

instance Convertible String ShortByteString where
  {-# INLINE convert #-}
  convert = BSS.pack . Codec.UTF8.encode

-- Convert from Text

instance Convertible Text String where
  {-# INLINE convert #-}
  convert = T.unpack

instance Convertible Text Text where
  {-# INLINE convert #-}
  convert = id

instance Convertible Text LazyText where
  {-# INLINE convert #-}
  convert = TL.fromStrict

instance Convertible Text TextBuilder where
  {-# INLINE convert #-}
  convert = TB.fromText

instance Convertible Text ShortText where
  {-# INLINE convert #-}
  convert = TS.fromText

instance Convertible Text ByteString where
  {-# INLINE convert #-}
  convert = TE.encodeUtf8

instance Convertible Text LazyByteString where
  {-# INLINE convert #-}
  convert = BSL.fromStrict . TE.encodeUtf8

instance Convertible Text ByteStringBuilder where
  {-# INLINE convert #-}
  convert = TE.encodeUtf8Builder

instance Convertible Text ShortByteString where
  {-# INLINE convert #-}
  convert = BSS.toShort . TE.encodeUtf8

-- Convert from LazyText

instance Convertible LazyText String where
  {-# INLINE convert #-}
  convert = TL.unpack

instance Convertible LazyText Text where
  {-# INLINE convert #-}
  convert = TL.toStrict

instance Convertible LazyText LazyText where
  {-# INLINE convert #-}
  convert = id

instance Convertible LazyText ShortText where
  {-# INLINE convert #-}
  convert = TS.fromText . TL.toStrict

instance Convertible LazyText TextBuilder where
  {-# INLINE convert #-}
  convert = TB.fromLazyText

instance Convertible LazyText ByteString where
  {-# INLINE convert #-}
  convert = BSL.toStrict . TLE.encodeUtf8

instance Convertible LazyText LazyByteString where
  {-# INLINE convert #-}
  convert = TLE.encodeUtf8

instance Convertible LazyText ByteStringBuilder where
  {-# INLINE convert #-}
  convert = TLE.encodeUtf8Builder

instance Convertible LazyText ShortByteString where
  {-# INLINE convert #-}
  convert = BSS.toShort . BSL.toStrict . TLE.encodeUtf8

-- Convert from ShortText

instance Convertible ShortText String where
  {-# INLINE convert #-}
  convert = TS.toString

instance Convertible ShortText Text where
  {-# INLINE convert #-}
  convert = TS.toText

instance Convertible ShortText LazyText where
  {-# INLINE convert #-}
  convert = TL.fromStrict . TS.toText

instance Convertible ShortText ShortText where
  {-# INLINE convert #-}
  convert = id

instance Convertible ShortText TextBuilder where
  {-# INLINE convert #-}
  convert = TB.fromText . TS.toText

instance Convertible ShortText ByteString where
  {-# INLINE convert #-}
  convert = TS.toByteString

instance Convertible ShortText LazyByteString where
  {-# INLINE convert #-}
  convert = BSB.toLazyByteString . TS.toBuilder

instance Convertible ShortText ByteStringBuilder where
  {-# INLINE convert #-}
  convert = TS.toBuilder

instance Convertible ShortText ShortByteString where
  {-# INLINE convert #-}
  convert = TS.toShortByteString

-- Convert from TextBuilder

instance Convertible TextBuilder String where
  {-# INLINE convert #-}
  convert = TL.unpack . TB.toLazyText

instance Convertible TextBuilder Text where
  {-# INLINE convert #-}
  convert = TL.toStrict . TB.toLazyText

instance Convertible TextBuilder LazyText where
  {-# INLINE convert #-}
  convert = TB.toLazyText

instance Convertible TextBuilder ShortText where
  {-# INLINE convert #-}
  convert = TS.fromText . TL.toStrict . TB.toLazyText

instance Convertible TextBuilder TextBuilder where
  {-# INLINE convert #-}
  convert = id

instance Convertible TextBuilder ByteString where
  {-# INLINE convert #-}
  convert = BSL.toStrict . TLE.encodeUtf8 . TB.toLazyText

instance Convertible TextBuilder LazyByteString where
  {-# INLINE convert #-}
  convert = TLE.encodeUtf8 . TB.toLazyText

instance Convertible TextBuilder ByteStringBuilder where
  {-# INLINE convert #-}
  convert = TLE.encodeUtf8Builder . TB.toLazyText

instance Convertible TextBuilder ShortByteString where
  {-# INLINE convert #-}
  convert = BSS.toShort . BSL.toStrict . TLE.encodeUtf8 . TB.toLazyText

-- Convert from ByteString

instance Convertible ByteString String where
  {-# INLINE convert #-}
  convert = BS.UTF8.toString

instance Convertible ByteString Text where
  {-# INLINE convert #-}
  convert = TE.decodeUtf8

instance Convertible ByteString LazyText where
  {-# INLINE convert #-}
  convert = TLE.decodeUtf8 . BSL.fromStrict

instance Convertible ByteString ShortText where
  {-# INLINE convert #-}
  convert = fromJust . TS.fromByteString

instance Convertible ByteString TextBuilder where
  {-# INLINE convert #-}
  convert = TB.fromLazyText . TL.fromStrict . TE.decodeUtf8

instance Convertible ByteString ByteString where
  {-# INLINE convert #-}
  convert = id

instance Convertible ByteString LazyByteString where
  {-# INLINE convert #-}
  convert = BSL.fromStrict

instance Convertible ByteString ByteStringBuilder where
  {-# INLINE convert #-}
  convert = BSB.byteString

instance Convertible ByteString ShortByteString where
  {-# INLINE convert #-}
  convert = BSS.toShort

-- Convert from LazyByteString

instance Convertible LazyByteString String where
  {-# INLINE convert #-}
  convert = BSL.UTF8.toString

instance Convertible LazyByteString Text where
  {-# INLINE convert #-}
  convert = TE.decodeUtf8 . BSL.toStrict

instance Convertible LazyByteString LazyText where
  {-# INLINE convert #-}
  convert = TLE.decodeUtf8

instance Convertible LazyByteString ShortText where
  {-# INLINE convert #-}
  convert = fromJust . TS.fromByteString . BSL.toStrict

instance Convertible LazyByteString TextBuilder where
  {-# INLINE convert #-}
  convert = TB.fromLazyText . TLE.decodeUtf8

instance Convertible LazyByteString ByteString where
  {-# INLINE convert #-}
  convert = BSL.toStrict

instance Convertible LazyByteString LazyByteString where
  {-# INLINE convert #-}
  convert = id

instance Convertible LazyByteString ByteStringBuilder where
  {-# INLINE convert #-}
  convert = BSB.lazyByteString

instance Convertible LazyByteString ShortByteString where
  {-# INLINE convert #-}
  convert = BSS.toShort . BSL.toStrict

-- Convert from ByteStringBuilder

instance Convertible ByteStringBuilder String where
  {-# INLINE convert #-}
  convert = BSL.UTF8.toString . BSB.toLazyByteString

instance Convertible ByteStringBuilder Text where
  {-# INLINE convert #-}
  convert = TE.decodeUtf8 . BSL.toStrict . BSB.toLazyByteString

instance Convertible ByteStringBuilder LazyText where
  {-# INLINE convert #-}
  convert = TLE.decodeUtf8 . BSB.toLazyByteString

instance Convertible ByteStringBuilder ShortText where
  {-# INLINE convert #-}
  convert = fromJust . TS.fromShortByteString . BSS.toShort . BSL.toStrict . BSB.toLazyByteString

instance Convertible ByteStringBuilder TextBuilder where
  {-# INLINE convert #-}
  convert = TB.fromLazyText . TLE.decodeUtf8 . BSB.toLazyByteString

instance Convertible ByteStringBuilder ByteString where
  {-# INLINE convert #-}
  convert = BSL.toStrict . BSB.toLazyByteString

instance Convertible ByteStringBuilder LazyByteString where
  {-# INLINE convert #-}
  convert = BSB.toLazyByteString

instance Convertible ByteStringBuilder ByteStringBuilder where
  {-# INLINE convert #-}
  convert = id

instance Convertible ByteStringBuilder ShortByteString where
  {-# INLINE convert #-}
  convert = BSS.toShort . BSL.toStrict . BSB.toLazyByteString

-- Convert from ShortByteString

instance Convertible ShortByteString String where
  {-# INLINE convert #-}
  convert = Codec.UTF8.decode . BSS.unpack

instance Convertible ShortByteString Text where
  {-# INLINE convert #-}
  convert = TE.decodeUtf8 . BSS.fromShort

instance Convertible ShortByteString LazyText where
  {-# INLINE convert #-}
  convert = TLE.decodeUtf8 . BSL.fromStrict . BSS.fromShort

instance Convertible ShortByteString ShortText where
  {-# INLINE convert #-}
  convert = fromJust . TS.fromShortByteString

instance Convertible ShortByteString TextBuilder where
  {-# INLINE convert #-}
  convert = TB.fromText . TE.decodeUtf8 . BSS.fromShort

instance Convertible ShortByteString ByteString where
  {-# INLINE convert #-}
  convert = BSS.fromShort

instance Convertible ShortByteString LazyByteString where
  {-# INLINE convert #-}
  convert = BSL.fromStrict . BSS.fromShort

instance Convertible ShortByteString ByteStringBuilder where
  {-# INLINE convert #-}
  convert = BSB.byteString . BSS.fromShort

instance Convertible ShortByteString ShortByteString where
  {-# INLINE convert #-}
  convert = id
