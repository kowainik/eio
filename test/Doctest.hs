module Main (main) where

import EIO

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
    sourceFiles <- glob "src/**/*.hs"
    doctest
        $ "-XInstanceSigs"
        : "-XNoImplicitPrelude"
        : "-XOverloadedStrings"
        : "-XScopedTypeVariables"
        : "-XTypeApplications"
        : "-XDerivingStrategies"
        : "-XGeneralizedNewtypeDeriving"
        : "-XQualifiedDo"
        : sourceFiles
