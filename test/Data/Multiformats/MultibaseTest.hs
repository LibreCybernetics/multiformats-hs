{-# LANGUAGE OverloadedStrings #-}

module Data.Multiformats.MultibaseTest where

import Data.Multiformats.Multibase

import Data.ByteString       as BS
import Data.List             as L
import Data.Text             as T
import Data.Text.Encoding    as TEnc
import Data.Text.IO          as IO

import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty.Hspec

-- Unit Tests

spec_errors ::  Spec
spec_errors =
  describe "Basic Error Handling" $ do
    it "Attempting to decode empty text should error" $
      decode ""  `shouldBe` Left EmptyInput
    it "Attempting to decode an unknown codec should error" $
      decode "x" `shouldBe` Left UnknownCodec

spec_examples :: Spec
spec_examples = before (exampleTestLoad "Multibase.csv") exampleTestRun

-- Unit Test Helpers

exampleTestRun :: SpecWith [[Text]]
exampleTestRun = it "Example Data Should Match" $ \values -> mapM_ doCases values
  where
    doCases value = do
      doCase Identity 1
      doCase Base2    2
      doCase Base8    3
      doCase Base10   4
        where
          doCase codec idx = encode codec (encodeUtf8 $ L.head value) `shouldBe` (value !! idx)

exampleTestLoad :: FilePath -> IO [[Text]]
exampleTestLoad path = do
  content <- IO.readFile $ "test/Data/Multiformats/" <> path
  return  (T.splitOn ", " <$> T.lines content)

-- Properties

prop_decodeIsLeftInverseOfEncode :: ByteString -> Bool
prop_decodeIsLeftInverseOfEncode input =
  (input == decId) &&
  (input == decBase2) &&
  (input == decBase8)
  where
    encId    = encode Identity input
    encBase2 = encode Base2    input
    encBase8 = encode Base8    input
    Right (Identity, decId) = decode encId
    Right (Base2, decBase2) = decode encBase2
    Right (Base8, decBase8) = decode encBase8
