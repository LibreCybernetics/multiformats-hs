{-# LANGUAGE OverloadedStrings #-}

module Data.Multiformats.MultibaseTest where

import Data.Multiformats.Multibase

import Data.ByteString as BS
import Data.List       as L
import Data.Maybe      as M
import Data.Text       as T
import Data.Text.IO    as IO

import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty.Hspec

-- Unit Tests

spec_emptyShouldError =
  it "Attempting to decode empty text should error" $ do
    decode "" `shouldBe` Left Empty

spec_unspecifiedCodecShouldError =
  it "Attempting to decode an unknown codec should error" $ do
    decode "x" `shouldBe` Left UnknownCodec

spec_example1 = exampleTestHelper "Decentralize everything!!" "Multibase1.csv"

-- Unit Test Helpers

exampleTestHelper :: ByteString -> FilePath -> Spec
exampleTestHelper bs path =
  beforeAll (exampleTestHelperLoad path) $ do
    it ("Example: " <> show bs) $ \values -> do
      encode Identity bs `shouldBe` (values !! 0) !! 1

exampleTestHelperLoad :: FilePath -> IO [[Text]]
exampleTestHelperLoad path = do
  content <- IO.readFile $ "test/Data/Multiformats/" <> path
  return  (T.splitOn ", " <$> T.lines content)

-- Properties

prop_decodeIsLeftInverseOfEncode :: ByteString -> Bool
prop_decodeIsLeftInverseOfEncode input = decoded == input
  where
    encoded = encode Identity input
    Right (Identity, decoded) = decode encoded
