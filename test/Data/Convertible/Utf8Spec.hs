module Data.Convertible.Utf8Spec where

import           Codec.Text.Detect     (detectEncodingName)
import           Data.Convertible.Utf8
import           Test.Hspec

spec :: Spec
spec =
  describe "convert to LBS" $
    context "from String" $ do
      let string = convert "これはマルチバイト文字列です" :: String
      let lbs = convert string
      it "is UTF-8" $
        detectEncodingName lbs `shouldBe` utf8
      it "is not truncated" $
        "これはマルチバイト文字列です" `shouldBe` convert string

      context "via Text" $ do
        let text = convert "これはマルチバイト文字列です" :: Text
        let lbs = convert text
        it "is UTF-8" $
          detectEncodingName lbs `shouldBe` utf8
        it "is not truncated" $
          "これはマルチバイト文字列です" `shouldBe` convert text

      context "via LazyText" $ do
        let lazyText = convert "これはマルチバイト文字列です" :: LazyText
        let lbs = convert lazyText
        it "is UTF-8" $
          detectEncodingName lbs `shouldBe` utf8
        it "is not truncated" $
          "これはマルチバイト文字列です" `shouldBe` convert lazyText

      context "via TextBuilder" $ do
        let textBuilder = convert "これはマルチバイト文字列です" :: TextBuilder
        let lbs = convert textBuilder
        it "is UTF-8" $
          detectEncodingName lbs `shouldBe` utf8
        it "is not truncated" $
          "これはマルチバイト文字列です" `shouldBe` convert textBuilder

      context "via ShortText" $ do
        let shortText = convert "これはマルチバイト文字列です" :: ShortText
        let lbs = convert shortText
        it "is UTF-8" $
          detectEncodingName lbs `shouldBe` utf8
        it "is not truncated" $
          "これはマルチバイト文字列です" `shouldBe` convert shortText

      context "via StrictByteString" $ do
        let strictByteString = convert "これはマルチバイト文字列です" :: ByteString
        let lbs = convert strictByteString
        it "is UTF-8" $
          detectEncodingName lbs `shouldBe` utf8
        it "is not truncated" $
          "これはマルチバイト文字列です" `shouldBe` convert strictByteString

      context "via ByteStringBuilder" $ do
        let byteStringBuilder = convert "これはマルチバイト文字列です" :: ByteStringBuilder
        let lbs = convert byteStringBuilder
        it "is UTF-8" $
          detectEncodingName lbs `shouldBe` utf8
        it "is not truncated" $
          "これはマルチバイト文字列です" `shouldBe` convert byteStringBuilder

      context "via ShortByteString" $ do
        let shortByteString = convert "これはマルチバイト文字列です" :: ShortByteString
        let lbs = convert shortByteString
        it "is UTF-8" $
          detectEncodingName lbs `shouldBe` utf8
        it "is not truncated" $
          "これはマルチバイト文字列です" `shouldBe` convert shortByteString

utf8 :: Maybe String
utf8 = Just "UTF-8"
