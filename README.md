# scrapegis

A tool for querying the Hennepin county property GIS server for data, and
dumping to CSV.

Wrote this more or less to learn more Haskell, apologies to anyone who actually needs to use it. 

## Installation

Build with `make all` or if for some crazy reason you need this installed:

    make global-install

## Usage

    scrapegis fetch city --csv [--out=<path>]
    scrapegis fetch zip <zip_code> --csv [--out=<path>]

Or within the sandbox:

    cabal run scrapegis ... etc.

Either specify an outfile, or it will be stdout.

See Usage.txt for more advanced examples.

## How to run tests

    cabal test
