{-# LANGUAGE OverloadedStrings, DeriveGeneric  #-}

module Scrapegis.Types
    ( IDQueryResult
    , Feature(..)
    , RunParams(..)
    , OutputData(..)
    , RequestBatch(..)
    , FeatureLookup(..)
    , FeatureAttributes(..)
    , getIDList
    , feature_header_bs
    ) where

import Data.List as L
import Data.Text as T

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as D8

import Control.Applicative
import Data.Csv (toRecord, toField, ToRecord, record)

import GHC.Generics

import Control.Monad (mzero)

-- | Parameters for the main run.

data RunParams = RunParams {
    outputFile :: FilePath
  , queryString :: String
  } deriving (Show)

data OutputData = OutputData {
    csvHeader  :: D8.ByteString
  , csvRecords :: [Feature]
}

data RequestBatch = RequestBatch {
    batchNumber :: Int
  , batchTotal :: Int
  , batchValues :: [Integer]
}

-- | Each JSON result contains a list of `Feature` objects, which contains both
-- | attributes and geographical information. FeatureAttributes handles only
-- | the attribute section.

data FeatureAttributes = FeatureAttributes {
      getPID :: String
    , getHOUSE_NO :: Integer
    , getSTREET_NM :: String
    , getZIP_CD :: String
    , getOWNER_NM :: String
    , getTAXPAYER_NM :: String
    , getTAXPAYER_NM_1 :: String
    , getTAXPAYER_NM_2 :: String
    , getTAXPAYER_NM_3 :: String
    , getABBREV_ADDN_NM :: String
    , getBLOCK :: String
    , getLOT :: String
    , getHMSTD_CD1 :: String
    , getHMSTD_CD1_NAME :: String

    , getADDITION_NO :: String
    , getMKT_VAL_TOT :: Integer
    , getTAX_TOT :: Integer
    , getFORFEIT_LAND_IND :: String
    , getBUILD_YR :: String
    , getMUNIC_CD :: String
    , getMUNIC_NM :: String

    , getABSTR_TORRENS_CD    :: String
    , getTORRENS_TYP         :: String
    , getCONDO_NO            :: String
    , getCONTIG_IND1         :: String
    , getCO_OP_IND           :: String
    , getNET_TAX_CAPACITY    :: Integer
    , getEST_BLDG_MKT_VAL1   :: Integer
    , getEST_BLDG_MKT_VAL2   :: Integer
    , getEST_BLDG_MKT_VAL3   :: Integer
    , getEST_BLDG_MKT_VAL4   :: Integer
    , getEST_LAND_MKT_VAL1   :: Integer
    , getEST_LAND_MKT_VAL2   :: Integer
    , getEST_LAND_MKT_VAL3   :: Integer
    , getEST_LAND_MKT_VAL4   :: Integer
    , getFEATURECODE         :: Integer
    , getFRAC_HOUSE_NO       :: String
    , getMAILING_MUNIC_CD    :: String
    , getMAILING_MUNIC_NM    :: String
    , getMETES_BNDS1         :: String
    , getMETES_BNDS2         :: String
    , getMETES_BNDS3         :: String
    , getMETES_BNDS4         :: String
    , getMORE_METES_BNDS_IND :: String
    , getMULTI_ADDR_IND      :: String
    , getOBJECTID            :: Integer

    -- TODO: rename from csv header values
    -- , getPidText :: String
    -- , getPropertyStatusCd :: String
    -- , getPropertyTypeCd1 :: String
    -- , getPropertyTypeCd1Name :: String
    -- , getPropertyTypeCd2 :: String
    -- , getPropertyTypeCd3 :: String
    -- , getPropertyTypeCd4 :: String
    -- , getSaleCode :: String
    -- , getSaleCodeName :: String
    -- , getSaleDate :: String
    -- , getSalePrice :: Integer
    -- , getSchoolDistNo :: String
    -- , getSewerDistNo :: String
    -- , getStateCd :: Integer
    -- , getTifProjectNumber :: String
    -- , getWatershedNumber :: String
    } deriving (Generic, Show)

-- | Control parsing the JSON into a Haskell data type.

instance ToJSON FeatureAttributes where
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = (L.drop 3) }
  toJSON     = genericToJSON defaultOptions { fieldLabelModifier = (L.drop 3) }

instance FromJSON FeatureAttributes where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = (L.drop 3) }

-- | This handles the JSON returned by the first object ID query. It returns no
-- | data apart from the object IDs, which are later chunked and processed
-- | FeatureLookups.

data IDQueryResult = IDQueryResult { getIDList :: [Integer]
                                   } deriving (Show)

-- | JSON parsing for IDQueryResult

instance FromJSON IDQueryResult where
  parseJSON (Object o) = IDQueryResult <$> (o .: "objectIds")
  parseJSON  _ = mzero

-- | This is returned in FeatureLookups queries.

data Feature = Feature { featureAttributes :: FeatureAttributes
                       } deriving (Show)

-- | JSON parsing for Feature objects. Simple for now.

instance FromJSON Feature where
  parseJSON (Object o) = Feature <$> o .: "attributes"
  parseJSON  _ = mzero

-- | This is the result of querying a set of IDs.

data FeatureLookup = FeatureLookup { getFeatures :: [Feature]
                                   , displayFieldName :: String
                                   -- , getFieldAliases :: M.Map String String
                                   -- , getSpatialReference :: M.Map String Integer
                                   -- , getGeometryType :: String
                                   } deriving (Show)

-- | FeatureLookup JSON parsing.

instance FromJSON FeatureLookup where
  parseJSON (Object o) = FeatureLookup <$> o .: "features"
                                       <*> o .: "displayFieldName"
                                       -- <*> o .: "fieldAliases"
                                       -- <*> o .: "spatialReference"
                                       -- <*> o .: "geometryType"
  parseJSON  _ = mzero


-- | FeatureLookup CSV serialization.

instance ToRecord FeatureLookup where
  toRecord feat = record [toField fieldname, toField fieldname]
    where
      fieldname = displayFieldName feat

feature_header_bs :: D8.ByteString
feature_header_bs = heddr
  where
    heddr = D8.pack $ (commad ++ "\n")
    commad = L.intercalate ("," :: String) feature_header_cols

feature_header_cols :: [String]
feature_header_cols = [ "PID"
                      , "HOUSE_NO"
                      , "STREET_NM"
                      , "ZIP_CD"
                      , "OWNER_NM"
                      , "TAXPAYER_NM"
                      , "TAXPAYER_NM_1"
                      , "TAXPAYER_NM_2"
                      , "TAXPAYER_NM_3"
                      , "ABBREV_ADDN_NM"
                      , "BLOCK"
                      , "LOT"
                      , "HMSTD_CD1"
                      , "HMSTD_CD1_NAME"
                      , "ADDITION_NO"
                      , "MKT_VAL_TOT"
                      , "TAX_TOT"
                      , "FORFEIT_LAND_IND"
                      , "BUILD_YR"
                      , "MUNIC_CD"
                      , "MUNIC_NM"
                      , "ABSTR_TORRENS_CD"
                      , "TORRENS_TYP"
                      , "CONDO_NO"
                      , "CONTIG_IND1"
                      , "CO_OP_IND"
                      , "NET_TAX_CAPACITY"
                      , "EST_BLDG_MKT_VAL1"
                      , "EST_BLDG_MKT_VAL2"
                      , "EST_BLDG_MKT_VAL3"
                      , "EST_BLDG_MKT_VAL4"
                      , "EST_LAND_MKT_VAL1"
                      , "EST_LAND_MKT_VAL2"
                      , "EST_LAND_MKT_VAL3"
                      , "EST_LAND_MKT_VAL4"
                      , "FEATURECODE"
                      , "FRAC_HOUSE_NO"
                      , "MAILING_MUNIC_CD"
                      , "MAILING_MUNIC_NM"
                      , "METES_BNDS1"
                      , "METES_BNDS2"
                      , "METES_BNDS3"
                      , "METES_BNDS4"
                      , "MORE_METES_BNDS_IND"
                      , "MULTI_ADDR_IND"
                      , "OBJECTID"

                      -- TODO: more data
                      -- -- <*> clean "PARCEL_AREA"
                      -- , "PID_TEXT"
                      -- , "PROPERTY_STATUS_CD"
                      -- , "PROPERTY_TYPE_CD1"
                      -- , "PROPERTY_TYPE_CD1_NAME"
                      -- , "PROPERTY_TYPE_CD2"
                      -- , "PROPERTY_TYPE_CD3"
                      -- , "PROPERTY_TYPE_CD4"
                      -- , "SALE_CODE"
                      -- , "SALE_CODE_NAME"
                      -- , "SALE_DATE"
                      -- , "SALE_PRICE"
                      -- , "SCHOOL_DIST_NO"
                      -- , "SEWER_DIST_NO"
                      -- , "STATE_CD"
                      -- -- <*> (o .: "Shape.area")
                      -- -- <*> (o .: "Shape.len")
                      -- , "TIF_PROJECT_NO"
                      -- , "WATERSHED_NO"
                      ]

-- cleanText (String s) = String $ T.strip s

instance ToRecord Feature where
  toRecord feat = record row_fields
    where
      clean = T.unpack . T.strip . T.pack
      field_accessors = [ getPID
                        , (show . getHOUSE_NO )
                        , getSTREET_NM
                        , getZIP_CD
                        , getOWNER_NM
                        , getTAXPAYER_NM
                        , getTAXPAYER_NM_1
                        , getTAXPAYER_NM_2
                        , getTAXPAYER_NM_3
                        , getABBREV_ADDN_NM
                        , getBLOCK
                        , getLOT
                        , getHMSTD_CD1
                        , getHMSTD_CD1_NAME

                        , getADDITION_NO
                        , (show . getMKT_VAL_TOT)
                        , (show . getTAX_TOT)
                        , getFORFEIT_LAND_IND
                        , getBUILD_YR
                        , getMUNIC_CD
                        , getMUNIC_NM

                        , getABSTR_TORRENS_CD
                        , getTORRENS_TYP
                        , getCONDO_NO
                        , getCONTIG_IND1
                        , getCO_OP_IND
                        , (show . getNET_TAX_CAPACITY)
                        , (show . getEST_BLDG_MKT_VAL1)
                        , (show . getEST_BLDG_MKT_VAL2)
                        , (show . getEST_BLDG_MKT_VAL3)
                        , (show . getEST_BLDG_MKT_VAL4)
                        , (show . getEST_LAND_MKT_VAL1)
                        , (show . getEST_LAND_MKT_VAL2)
                        , (show . getEST_LAND_MKT_VAL3)
                        , (show . getEST_LAND_MKT_VAL4)
                        , (show . getFEATURECODE)
                        , getFRAC_HOUSE_NO
                        , getMAILING_MUNIC_CD
                        , getMAILING_MUNIC_NM
                        , getMETES_BNDS1
                        , getMETES_BNDS2
                        , getMETES_BNDS3
                        , getMETES_BNDS4
                        , getMORE_METES_BNDS_IND
                        , getMULTI_ADDR_IND
                        , (show . getOBJECTID)
                        ]

      row_fields = toField <$> [clean (f attrs) | f <- field_accessors]
      attrs = featureAttributes feat

