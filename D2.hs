#!/usr/bin/env cabal
{- cabal:
build-depends: base, pandoc, pandoc-types, text, bytestring, process, base64, cryptohash-sha256, directory
-}
{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module Main where

import Text.Pandoc.JSON

import System.Process
import System.IO

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Text.Encoding.Base64 as T (encodeBase64) 
import qualified Data.Text.Encoding as T (encodeUtf8) 

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Crypto.Hash.SHA256 as B (hash)

import qualified Data.List as List

import System.Directory

import Numeric (showHex)
import Text.Printf

import Control.Monad(unless)
import Data.Functor ((<&>))

import System.Environment (lookupEnv)

{-
  $ chmod +x D2.hs  

  USAGE: pandoc -s input.md --filter ./D2.hs -o output.md
  USAGE: D2_INLINE=true pandoc -s input.md --filter ./D2.hs -o output.md # inline the diagrams
-}

runD2 :: Text -> IO Text
runD2 input = do
  (Just hin, Just hout, _, _) <- createProcess (proc "d2" ["--theme", "105", "-", "-"]) { std_in = CreatePipe, std_out = CreatePipe }
  _ <- hPutStrLn hin $ T.unpack input
  _ <- hClose hin
  str <- hGetContents hout
  pure $ T.pack str

svgFilename :: Text -> Text
svgFilename input = "d2-" <> hasher input <> ".svg"
  where 
    hasher :: Text -> Text 
    hasher txt = toHexString $ B.hash $ T.encodeUtf8 txt

    toHexString :: ByteString -> Text
    toHexString = B.foldr (\b -> (<>) (T.pack $ printf "%02x" b)) ""

-- | Replace code blocks with d2 with the corrosponding diagram
-- | First argument determines to inline the image or not
-- | 
-- | ```d2 
-- | a -> b
-- | ``` 
-- | 
-- | Gets replaced with ![](./d2-<hash>.svg) or ![](data:image/svg+xml;base64,<svg>) 
-- | depending on inline or not
doD2 :: Bool -> Block -> IO Block
doD2 False cb@(CodeBlock (id, ["d2"], namevals) contents) = do
  let filename = svgFilename contents
  exists <- doesFileExist $ T.unpack filename
  unless exists $ runD2 contents >>= \svg -> TIO.writeFile (T.unpack filename) svg
  pure $ Para [Image (id, [], namevals) [] ("./" <> filename, "")]
doD2 True cb@(CodeBlock (id, ["d2"], namevals) contents) =
  runD2 contents <&> \svg -> Para [Image (id, [], namevals) [] ("data:image/svg+xml;base64," <> T.encodeBase64 svg, "")]
doD2 _ x = pure x

main :: IO ()
main = do 
    inline <- lookupEnv "D2_INLINE" <&> (Just "true" ==) 
    toJSONFilter $ doD2 inline

