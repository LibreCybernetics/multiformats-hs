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

spec_errors ::  Spec
spec_errors =
  describe "Basic Error Handling" $ do
    it "Attempting to decode empty text should error" $
      decode "" `shouldBe` Left EmptyInput
    it "Attempting to decode an unknown codec should error" $
      decode "x" `shouldBe` Left UnknownCodec

spec_examples :: Spec
spec_examples =
  describe "Example Data Should Match" $ do
    exampleTestHelper "Decentralize everything!!" "Multibase1.csv"

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
prop_decodeIsLeftInverseOfEncode input = (input == decId)
  where
    encId = encode Identity input
    Right (Identity, decId) = decode encId
