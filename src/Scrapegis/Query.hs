{-# LANGUAGE OverloadedStrings #-}

module Scrapegis.Query where

import System.IO ( stderr
                 , hPutStrLn
                 , hClose
                 , openFile
                 , IOMode(WriteMode)
                 )

import qualified Data.ByteString.Lazy.Char8 as D8
import Data.Text as T
import Data.List as L
import Control.Applicative

import Scrapegis.Hennepin as Henn
-- import Scrapegis.MockHennepin as Mock
import Scrapegis.Types
import Scrapegis.Export

makeQueryString :: String -> String -> String
makeQueryString "zip"      aarg = "ZIP_CD = '" ++ aarg ++ "'"
makeQueryString "owner"    aarg = "OWNER_NM LIKE '" ++ aarg ++ "'"
makeQueryString "taxpayer" aarg = "TAXPAYER_NM LIKE '" ++ aarg ++ "'"
makeQueryString "names"    aarg = "TAXPAYER_NM LIKE '" ++ aarg ++ "' OR " ++
                                  "OWNER_NM LIKE '" ++ aarg ++ "'"
makeQueryString "city"     _ = "MUNIC_CD = '01'" -- (minneapolis)
makeQueryString "pid"      aarg = "PID = '" ++ aarg ++ "'"
makeQueryString _ _ = ""

printTheThing :: Monad m => (D8.ByteString -> m b) -> OutputData -> m b
printTheThing f o = do printHeader >> printRecords
  where
    printHeader  = f $ csvHeader o
    printRecords = f $ featuresToCSV $ csvRecords o

handleOutput :: FilePath -> OutputData -> IO ()
handleOutput "stdout" o = do
  printTheThing (D8.putStrLn) o
handleOutput output_file o = do
  h <- openFile output_file WriteMode
  printTheThing (D8.hPut h) o
  hClose h
  hPutStrLn stderr $ "Written to: " ++ output_file

-- configured process
runQuery :: FilePath -> [Char] -> IO ()
runQuery output_file q = do
     hPutStrLn stderr $ "  Querying with: " ++ q
     records <- Henn.getHenCountyRecords (T.pack q)

     handleOutput output_file $ OutputData {
        csvHeader = feature_header_bs
      , csvRecords = L.concat $ getFeatures <$> records
     }


