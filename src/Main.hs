-- Analysis for finding bulk data patterns in Fortran code

-- High-level idea-
--    There are two categories:
--          0 - A program with global array that is indexed by global variables
--          1 - A program with stencil access with a single region forward
--              (matching the size of the stride)
-- Metrics:
--    What proportion of program units is this used in
--    How large is the array
--    How many variables used as subscripts
--    (Tentative) Coverage of subscripts
--    (Tentative) Overlapping subscripts

module Main where

import Analysis
import Camfort.Analysis.Input

main :: IO ()
main = do
  file <- readParseSrcDir "exmaple.f90" []
  return ()