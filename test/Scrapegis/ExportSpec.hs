{-# LANGUAGE OverloadedStrings #-}

module Scrapegis.ExportSpec (spec) where

import Test.Hspec

-- import Test.QuickChec
import Control.Exception (evaluate)

import Scrapegis.Export
import Scrapegis.Types

import System.IO ( hPutStrLn
                 , hClose
                 , openFile
                 , IOMode(ReadMode)
                 )

import qualified Data.ByteString.Lazy.Char8 as D8
import qualified Data.ByteString.Lazy as B

import Data.Aeson

textToQueryResult :: B.ByteString -> Maybe IDQueryResult
textToQueryResult text = decode text :: Maybe IDQueryResult

textToFeatureLookup :: B.ByteString -> Maybe FeatureLookup
textToFeatureLookup text = decode text :: Maybe FeatureLookup

textToAttributes :: B.ByteString -> Maybe FeatureAttributes
textToAttributes text = decode text :: Maybe FeatureAttributes

spec :: Spec
spec = do
  describe "Scrapegis.Types" $ do

    it "can parse ID Query Result JSON" $ do
      h <- openFile "test_data/object_ids.json" ReadMode
      instr <- B.hGetContents h
      let (Just json) = textToQueryResult instr
      let first = getIDList json !! 0
      first `shouldBe` 461446
      hClose h

    it "can parse Feature Lookup Attribute JSON" $ do
      h <- openFile "test_data/test_attributes.json" ReadMode
      instr <- B.hGetContents h
      let (Just json) = textToAttributes instr
      putStrLn $ getZIP_CD json
      (getZIP_CD json) `shouldBe` "55401"
      hClose h


    it "can parse Feature Lookup JSON" $ do
      h <- openFile "test_data/two_json.json" ReadMode
      instr <- B.hGetContents h
      let (Just json) = textToFeatureLookup instr
      let feats = getFeatures json
      let attrs = featureAttributes $ feats !! 0
      putStrLn $ getZIP_CD attrs
      (getZIP_CD attrs) `shouldBe` "55401"
      hClose h

  describe "Scrapegis.Export" $ do

    it "can turn Feature Lookup JSON to CSV" $ do

      h <- openFile "test_data/test_chunk.json" ReadMode
      instr <- B.hGetContents h
      let (Just json) = textToFeatureLookup instr
      let feats = take 2 $ getFeatures json
      let csv = featuresToCSV feats

      hClose h

