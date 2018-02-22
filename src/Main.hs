-- Analysis for finding bulk data patterns in Fortran code


module Main where

import Analysis
import Camfort.Input

main :: IO ()
main = do
  let modFiles = []
  pfs <- readParseSrcDir modFiles "example.f90" []
  let analysisOutput = analysis pfs
  -- Just raw print the output for now
  print analysisOutput
