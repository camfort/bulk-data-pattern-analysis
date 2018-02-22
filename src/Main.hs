-- Analysis for finding bulk data patterns in Fortran code


module Main where

import Analysis
import Camfort.Input

main :: IO ()
main = do
  let modFiles = []
  file <- readParseSrcDir modFiles "exmaple.f90" []
  return ()
