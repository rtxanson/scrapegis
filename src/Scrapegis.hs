{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Scrapegis 
    ( run
    ) where

import System.IO ( stderr
                 , hPutStrLn
                 )

import Scrapegis.Query

import Control.Monad (when)

import System.Console.Docopt 
import System.Environment (getArgs)

import Data.List as L

patterns :: Docopt
patterns = [docoptFile|src/Usage.txt|]

run :: IO ()
run =  do
    opts <- parseArgsOrExit patterns =<< getArgs
    handleOpts opts

handleOpts :: Arguments -> IO ()
handleOpts opts = do

    whenCmd "query" $ do
        let query_string = getOpt "query_string"
        go query_string

    whenCmd "fetch" $ do
        whenCmd "zip" $ do
            let query_arg = getOpt "query_arg"
            let query_string = makeQueryString "zip" query_arg
            go query_string

        whenCmd "owner" $ do
            let query_arg = getOpt "query_arg"
            let query_string = makeQueryString "owner" query_arg
            go query_string

        whenCmd "taxpayer" $ do
            let query_arg = getOpt "query_arg"
            let query_string = makeQueryString "taxpayer" query_arg
            go query_string

        whenCmd "names" $ do
            let query_arg = getOpt "query_arg"
            let query_string = makeQueryString "names" query_arg
            go query_string

        whenCmd "city" $ do
            -- EDINA: 24
            -- ORONO: 38
            -- RICHFIELD: 42
            let query_arg = getOpt "query_arg"
            let query_string = makeQueryString "city" query_arg

            go query_string

        whenCmd "pid" $ do
            let query_arg = getOpt "query_arg"
            let query_string = makeQueryString "pid" query_arg

            go query_string

    hPutStrLn stderr $ "Done."

  where
      output_file = getArgWithDefault opts "stdout" (longOption "out")
      go q = runQuery output_file q

      whenCmd x = when $ opts `isPresent` (command x)
      getOpt  x =        L.concat $ opts `getAllArgs` (argument x)

