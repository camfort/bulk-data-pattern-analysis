-- Analysis for finding bulk data patterns in Fortran code


module Main where

import Analysis
import System.Environment
import Camfort.Input
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

main :: IO ()
main = do
  args <- getArgs
  let file = case args of
        fn:_ -> fn
        []   -> "example.f90"
  let modFiles = []
  pfs <- readParseSrcDir modFiles file []
  let analysisOutput = analysis pfs
-- analysisOutput <- (analysis pfs `mappend`) <$> runAnalysisIO pfs
-- let analysisOutput = analysis pfs
  -- Just raw print the output for now
--  mapM_ print $ categoryOne analysisOutput
--  mapM_ print $ categoryTwo analysisOutput
  let amap = condenseCategoryOne analysisOutput
--  print amap
--  putStrLn "\n***\n"
  forM_ (M.toList $ filterCondensedCategoryOne amap) $ \ (name, pmap) -> do
    let perParam Nothing = "*"; perParam (Just k) = show k
    let perName (n, consts) = "{" ++ intercalate ";" (map perParam (S.toList consts)) ++ "}"
    let pstrs = map perName $ M.toList pmap
    putStrLn $ name ++ " (" ++ intercalate ", " pstrs ++ ")"
