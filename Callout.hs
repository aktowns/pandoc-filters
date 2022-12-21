#!/usr/bin/env cabal
{- cabal:
build-depends: base, pandoc, pandoc-types, text
-}
{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module Main where

import Text.Pandoc.JSON

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.List as List

{-
  $ chmod +x Callout.hs  

  USAGE: pandoc -s input.md --filter ./Callout.hs -o output.md
-}

-- | Bold text up until a softbreak is encountered
-- | This is to bold the title text in a callout, ie 
-- |
-- | > [!question] This is the callout text
-- | > This is the calllout body
-- |
-- | Becomes 
-- |
-- | > **This is the callout text**
-- | > This is the callout body
boldText :: [Inline] -> [Block]
boldText input = 
  case List.elemIndex SoftBreak input of
    Nothing -> [Para input]
    Just i -> do
      let (before, after) = List.splitAt i input
      [Para [Strong $ dropLeadingSpace before, SoftBreak], Para after]
  where
    dropLeadingSpace :: [Inline] -> [Inline]
    dropLeadingSpace (Space:xs) = dropLeadingSpace xs
    dropLeadingSpace xs = xs

-- | are we [!something]
isCallout :: Inline -> Bool
isCallout (Str str) = T.isPrefixOf "[!" str && T.isSuffixOf "]" str
isCallout _ = False

-- | and what is that something in [!something]
calloutName :: Inline -> Maybe T.Text
calloutName inline@(Str str) | isCallout inline = Just name
  where name = T.dropEnd 1 $ T.drop 2 str
calloutName _ = Nothing

-- | Given 
-- | > [!question] Is this the real life?
-- | > Is this just fantasy?
-- | 
-- | We return 
-- | > **Is this the real life?**
-- | > Is this just fantasy?
doCallout :: Block -> Block
doCallout bq@(BlockQuote blocks) = BlockQuote $ blocks >>= handleCallout
  where 
    handleCallout :: Block -> [Block]
    handleCallout (Para (h:xs)) | isCallout h = boldText xs
    handleCallout x = [x]
doCallout x = x

main :: IO ()
main = toJSONFilter doCallout
