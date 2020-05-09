{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Convertible.Utf8 where

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

-- Convert from String

instance Convertible String String where
  {-# INLINE convert #-}
  convert = id

instance Convertible String T.Text where
  {-# INLINE convert #-}
  convert = T.pack

instance Convertible String TL.Text where
  {-# INLINE convert #-}
  convert = TL.pack

instance Convertible String TB.Builder where
  {-# INLINE convert #-}
  convert = TB.fromString

instance Convertible String TS.ShortText where
  {-# INLINE convert #-}
  convert = TS.fromString

instance Convertible String BS.ByteString where
  {-# INLINE convert #-}
  convert = BS.UTF8.fromString

instance Convertible String BSL.ByteString where
  {-# INLINE convert #-}
  convert = BSL.UTF8.fromString

instance Convertible String BSB.Builder where
  {-# INLINE convert #-}
  convert = BSB.stringUtf8

instance Convertible String BSS.ShortByteString where
  {-# INLINE convert #-}
  convert = BSS.toShort . BS.UTF8.fromString

-- Convert from Text

instance Convertible T.Text String where
  {-# INLINE convert #-}
  convert = T.unpack

instance Convertible T.Text T.Text where
  {-# INLINE convert #-}
  convert = id

instance Convertible T.Text TL.Text where
  {-# INLINE convert #-}
  convert = TL.fromStrict

instance Convertible T.Text TB.Builder where
  {-# INLINE convert #-}
  convert = TB.fromText

instance Convertible T.Text TS.ShortText where
  {-# INLINE convert #-}
  convert = TS.fromText

instance Convertible T.Text BS.ByteString where
  {-# INLINE convert #-}
  convert = TE.encodeUtf8

instance Convertible T.Text BSL.ByteString where
  {-# INLINE convert #-}
  convert = BSL.fromStrict . TE.encodeUtf8

instance Convertible T.Text BSB.Builder where
  {-# INLINE convert #-}
  convert = TE.encodeUtf8Builder

instance Convertible T.Text BSS.ShortByteString where
  {-# INLINE convert #-}
  convert = BSS.toShort . TE.encodeUtf8

-- Convert from LazyText

instance Convertible TL.Text String where
  {-# INLINE convert #-}
  convert = TL.unpack

instance Convertible TL.Text T.Text where
  {-# INLINE convert #-}
  convert = TL.toStrict

instance Convertible TL.Text TL.Text where
  {-# INLINE convert #-}
  convert = id

instance Convertible TL.Text TS.ShortText where
  {-# INLINE convert #-}
  convert = TS.fromText . TL.toStrict

instance Convertible TL.Text TB.Builder where
  {-# INLINE convert #-}
  convert = TB.fromLazyText

instance Convertible TL.Text BS.ByteString where
  {-# INLINE convert #-}
  convert = BSL.toStrict . TLE.encodeUtf8

instance Convertible TL.Text BSL.ByteString where
  {-# INLINE convert #-}
  convert = TLE.encodeUtf8

instance Convertible TL.Text BSB.Builder where
  {-# INLINE convert #-}
  convert = TLE.encodeUtf8Builder

instance Convertible TL.Text BSS.ShortByteString where
  {-# INLINE convert #-}
  convert = BSS.toShort . BSL.toStrict . TLE.encodeUtf8

-- Convert from ShortText

instance Convertible TS.ShortText String where
  {-# INLINE convert #-}
  convert = TS.toString

instance Convertible TS.ShortText T.Text where
  {-# INLINE convert #-}
  convert = TS.toText

instance Convertible TS.ShortText TL.Text where
  {-# INLINE convert #-}
  convert = TL.fromStrict . TS.toText

instance Convertible TS.ShortText TS.ShortText where
  {-# INLINE convert #-}
  convert = id

instance Convertible TS.ShortText TB.Builder where
  {-# INLINE convert #-}
  convert = TB.fromText . TS.toText

instance Convertible TS.ShortText BS.ByteString where
  {-# INLINE convert #-}
  convert = TS.toByteString

instance Convertible TS.ShortText BSL.ByteString where
  {-# INLINE convert #-}
  convert = BSB.toLazyByteString . TS.toBuilder

instance Convertible TS.ShortText BSB.Builder where
  {-# INLINE convert #-}
  convert = TS.toBuilder

instance Convertible TS.ShortText BSS.ShortByteString where
  {-# INLINE convert #-}
  convert = TS.toShortByteString

-- Convert from TextBuilder

instance Convertible TB.Builder String where
  {-# INLINE convert #-}
  convert = TL.unpack . TB.toLazyText

instance Convertible TB.Builder T.Text where
  {-# INLINE convert #-}
  convert = TL.toStrict . TB.toLazyText

instance Convertible TB.Builder TL.Text where
  {-# INLINE convert #-}
  convert = TB.toLazyText

instance Convertible TB.Builder TS.ShortText where
  {-# INLINE convert #-}
  convert = TS.fromText . TL.toStrict . TB.toLazyText

instance Convertible TB.Builder TB.Builder where
  {-# INLINE convert #-}
  convert = id

instance Convertible TB.Builder BS.ByteString where
  {-# INLINE convert #-}
  convert = BSL.toStrict . TLE.encodeUtf8 . TB.toLazyText

instance Convertible TB.Builder BSL.ByteString where
  {-# INLINE convert #-}
  convert = TLE.encodeUtf8 . TB.toLazyText

instance Convertible TB.Builder BSB.Builder where
  {-# INLINE convert #-}
  convert = TLE.encodeUtf8Builder . TB.toLazyText

instance Convertible TB.Builder BSS.ShortByteString where
  {-# INLINE convert #-}
  convert = BSS.toShort . BSL.toStrict . TLE.encodeUtf8 . TB.toLazyText

-- Convert from ByteString

instance Convertible BS.ByteString String where
  {-# INLINE convert #-}
  convert = BS.UTF8.toString

instance Convertible BS.ByteString T.Text where
  {-# INLINE convert #-}
  convert = TE.decodeUtf8

instance Convertible BS.ByteString TL.Text where
  {-# INLINE convert #-}
  convert = TLE.decodeUtf8 . BSL.fromStrict

instance Convertible BS.ByteString TS.ShortText where
  {-# INLINE convert #-}
  convert = fromJust . TS.fromByteString

instance Convertible BS.ByteString TB.Builder where
  {-# INLINE convert #-}
  convert = TB.fromLazyText . TL.fromStrict . TE.decodeUtf8

instance Convertible BS.ByteString BS.ByteString where
  {-# INLINE convert #-}
  convert = id

instance Convertible BS.ByteString BSL.ByteString where
  {-# INLINE convert #-}
  convert = BSL.fromStrict

instance Convertible BS.ByteString BSB.Builder where
  {-# INLINE convert #-}
  convert = BSB.byteString

instance Convertible BS.ByteString BSS.ShortByteString where
  {-# INLINE convert #-}
  convert = BSS.toShort

-- Convert from LazyByteString

instance Convertible BSL.ByteString String where
  {-# INLINE convert #-}
  convert = BSL.UTF8.toString

instance Convertible BSL.ByteString T.Text where
  {-# INLINE convert #-}
  convert = TE.decodeUtf8 . BSL.toStrict

instance Convertible BSL.ByteString TL.Text where
  {-# INLINE convert #-}
  convert = TLE.decodeUtf8

instance Convertible BSL.ByteString TS.ShortText where
  {-# INLINE convert #-}
  convert = fromJust . TS.fromByteString . BSL.toStrict

instance Convertible BSL.ByteString TB.Builder where
  {-# INLINE convert #-}
  convert = TB.fromLazyText . TLE.decodeUtf8

instance Convertible BSL.ByteString BS.ByteString where
  {-# INLINE convert #-}
  convert = BSL.toStrict

instance Convertible BSL.ByteString BSL.ByteString where
  {-# INLINE convert #-}
  convert = id

instance Convertible BSL.ByteString BSB.Builder where
  {-# INLINE convert #-}
  convert = BSB.lazyByteString

instance Convertible BSL.ByteString BSS.ShortByteString where
  {-# INLINE convert #-}
  convert = BSS.toShort . BSL.toStrict

-- Convert from ByteStringBuilder

instance Convertible BSB.Builder String where
  {-# INLINE convert #-}
  convert = BSL.UTF8.toString . BSB.toLazyByteString

instance Convertible BSB.Builder T.Text where
  {-# INLINE convert #-}
  convert = TE.decodeUtf8 . BSL.toStrict . BSB.toLazyByteString

instance Convertible BSB.Builder TL.Text where
  {-# INLINE convert #-}
  convert = TLE.decodeUtf8 . BSB.toLazyByteString

instance Convertible BSB.Builder TS.ShortText where
  {-# INLINE convert #-}
  convert = fromJust . TS.fromShortByteString . BSS.toShort . BSL.toStrict . BSB.toLazyByteString

instance Convertible BSB.Builder TB.Builder where
  {-# INLINE convert #-}
  convert = TB.fromLazyText . TLE.decodeUtf8 . BSB.toLazyByteString

instance Convertible BSB.Builder BS.ByteString where
  {-# INLINE convert #-}
  convert = BSL.toStrict . BSB.toLazyByteString

instance Convertible BSB.Builder BSL.ByteString where
  {-# INLINE convert #-}
  convert = BSB.toLazyByteString

instance Convertible BSB.Builder BSB.Builder where
  {-# INLINE convert #-}
  convert = id

instance Convertible BSB.Builder BSS.ShortByteString where
  {-# INLINE convert #-}
  convert = BSS.toShort . BSL.toStrict . BSB.toLazyByteString

-- Convert from ShortByteString

instance Convertible BSS.ShortByteString String where
  {-# INLINE convert #-}
  convert = BS.UTF8.toString . BSS.fromShort

instance Convertible BSS.ShortByteString T.Text where
  {-# INLINE convert #-}
  convert = TE.decodeUtf8 . BSS.fromShort

instance Convertible BSS.ShortByteString TL.Text where
  {-# INLINE convert #-}
  convert = TLE.decodeUtf8 . BSL.fromStrict . BSS.fromShort

instance Convertible BSS.ShortByteString TS.ShortText where
  {-# INLINE convert #-}
  convert = fromJust . TS.fromShortByteString

instance Convertible BSS.ShortByteString TB.Builder where
  {-# INLINE convert #-}
  convert = TB.fromText . TE.decodeUtf8 . BSS.fromShort

instance Convertible BSS.ShortByteString BS.ByteString where
  {-# INLINE convert #-}
  convert = BSS.fromShort

instance Convertible BSS.ShortByteString BSL.ByteString where
  {-# INLINE convert #-}
  convert = BSL.fromStrict . BSS.fromShort

instance Convertible BSS.ShortByteString BSB.Builder where
  {-# INLINE convert #-}
  convert = BSB.byteString . BSS.fromShort

instance Convertible BSS.ShortByteString BSS.ShortByteString where
  {-# INLINE convert #-}
  convert = id
