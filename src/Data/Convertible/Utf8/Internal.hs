module Data.Convertible.Utf8.Internal
  ( Text,
    LazyText,
    TextBuilder,
    ShortText,
    ByteString,
    LazyByteString,
    ByteStringBuilder,
    ShortByteString,
  )
where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Short   as BSS
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Builder  as TB
import qualified Data.Text.Short         as TS

type Text = T.Text

type LazyText = TL.Text

type TextBuilder = TB.Builder

type ShortText = TS.ShortText

type ByteString = BS.ByteString

type LazyByteString = BSL.ByteString

type ByteStringBuilder = BSB.Builder

type ShortByteString = BSS.ShortByteString
