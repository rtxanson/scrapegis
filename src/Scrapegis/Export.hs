{-# LANGUAGE OverloadedStrings #-}

module Scrapegis.Export (
   featuresToCSV
 , queryToCSV
) where

import Data.Csv ( toRecord
                , encode
                )

import Data.List as L
import qualified Data.ByteString.Lazy as B

import Scrapegis.Types

queryToCSV :: Maybe FeatureLookup -> B.ByteString
queryToCSV (Just recs) = encode $ L.map toRecord (getFeatures recs)
queryToCSV Nothing = "" :: B.ByteString

featuresToCSV :: [Feature] -> B.ByteString
featuresToCSV recs = encode $ L.map toRecord recs
