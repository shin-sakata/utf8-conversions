# `utf8-conversions` A string conversion library that assumes utf8

[![Build Status](https://travis-ci.org/chemirea/utf8-conversions.svg?branch=master)](https://travis-ci.org/chemirea/utf8-conversions)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

This package provides a　Data.Convertible.Utf8 library for easy conversion of many string types in Haskell

## Support

- String
- ByteString
- ByteStringBuilder
- LazyByteString
- ByteStringShort
- Text
- LazyText
- TextBuilder
- TextShort

Supports type conversion between the above types.
Byte string is assumed to be in utf-8 encoding.

## Why?

String conversion in haskell is more difficult than in other languages and needs to be easier.

In particular, the OverloadedStrings pragma does not work properly when creating a ByteString.

So why not other conversion libraries?
There are many other conversion libraries that use the Maybe type for safety or are not explicitly stated as utf8.

Libraries that use maybe types are very labor intensive.
Libraries that do not explicitly state UTF8 are insecure

Therefore, this library clearly states that it assumes UTF8 and performs the conversion without using the MAYBE type, which is both safe and easy.


## Get involved!

If there's a bug or a better way to convert, please report it!