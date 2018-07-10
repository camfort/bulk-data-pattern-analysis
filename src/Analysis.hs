-- Core analysis

module Analysis where

import qualified Camfort.Specification.Stencils.InferenceFrontend as IF

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU

import Data.Generics.Uniplate.Operations

import Control.Lens

import Camfort.Analysis.Annotations  (A, unitAnnotation)
import Camfort.Helpers
import qualified Camfort.Analysis as CA
import qualified Camfort.Specification.Stencils as Stencils
import Camfort.Specification.Stencils.Syntax

import Data.Monoid
import Data.Foldable (foldMap)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either
import Data.Maybe
import Data.List

import Debug.Trace

-- High-level idea-
--    There are two categories:
--          1 - A program with global array that is indexed by global variables
--          2 - A program with stencil access with a single region forward
--              (matching the size of the stride)
-- Metrics:
--    What proportion of program units is this used in
--    How large is the array
--    How many variables used as subscripts
--    (Tentative) Coverage of subscripts
--    (Tentative) Overlapping subscripts

-- Main data type for collecting analysis info
data AnalysisInfo = AnalysisInfo
  {
    categoryOne :: [(Filename, BulkDataArrayInfo)]
  , categoryTwo :: [(Filename, [([Variable], Specification)])]
  }
  deriving (Show, Eq)

-- Used to represent an array and any global (non-induction) variables
-- used to index it
data BulkDataArrayInfo = Array
  {
    arrayName  :: F.Name
  -- , declPos    :: FU.Position
  , srcSpan :: FU.SrcSpan
  , indexInfo :: [(Int, Maybe Integer)]
  }
  deriving (Show, Eq)

instance Monoid AnalysisInfo where
  mempty = AnalysisInfo { categoryOne = [], categoryTwo = [] }
  mappend x y =
    AnalysisInfo {
      categoryOne = categoryOne x ++ categoryOne y
    , categoryTwo = categoryTwo x ++ categoryTwo y
    }

analysis :: [(F.ProgramFile A, SourceText)] -> AnalysisInfo
analysis = foldMap analysisSingleFile

-- singleFile for now, but we need to think about cross file definition of
-- the global array
analysisSingleFile :: (F.ProgramFile A, SourceText) -> AnalysisInfo
analysisSingleFile (pf, src) = AnalysisInfo { categoryOne = accessInfo, categoryTwo = [] }
  where
     filename     = F.pfGetFilename pf
     pf'          = FAR.analyseRenames $ FA.initAnalysis pf
     (pf'', tenv) = FAT.analyseTypes pf'
     pf'''        = FAD.analyseConstExps $ FAB.analyseBBlocks pf''

     perArray :: [F.Index (FA.Analysis A)] -> [(Int, Maybe Integer)]
     perArray is = [ (n, do F.IxSingle _ _ _ e <- Just ix; FAD.ConstInt i <- FA.constExp (F.getAnnotation e); return i)
                   | (n, ix) <- zip [1..] is ]
     accessInfo =
       [ (filename, Array (FA.varName a) ss $ perArray (F.aStrip is))
       | F.ExpSubscript _ ss a@(F.ExpValue _ _ (F.ValVariable _)) is <- universeBi pf''' ]

     stencilsInfoM = Stencils.infer False '=' pf

-- map of information about constants used to index arrays
type AMap = M.Map F.Name (M.Map Int (S.Set (Maybe Integer)))

-- convert analysis info into a map of information about the constants used to index arrays
condenseCategoryOne :: AnalysisInfo -> AMap
condenseCategoryOne (AnalysisInfo c1 _) = amap
  where
    amap = M.fromListWith (M.unionWith S.union) [ (arrayName bdai, doIndices (indexInfo bdai)) | bdai <- map snd c1 ]
    doIndices is = M.fromList $ map (fmap S.singleton) is

-- filter only the interesting ones
filterCondensedCategoryOne :: AMap -> AMap
filterCondensedCategoryOne = M.filter (not . M.null . M.filter (all isJust . S.toList))

--------------------------------------------------

-- run stencils analysis
runAnalysisIO pfs = do
  let a1 = foldMap analysisSingleFile pfs
  a2 <- mapM runAnalysisIO1 $ pfs
  return $ foldl' (<>) mempty $ a2

-- from stencils analysis:
type LogLine = (FU.SrcSpan, Either [([Variable], Specification)] (String,Variable))

runAnalysisIO1 (pf, src) = do
  let fn = F.pfGetFilename pf
  report <- CA.runAnalysisT fn (CA.logOutputStd True) CA.LogInfo [] (CA.generalizePureAnalysis $ Stencils.infer False '=' pf)
  case report ^. CA.arResult of
    CA.ARSuccess (IF.StencilsReport loglines) -> return $ AnalysisInfo { categoryOne = [], categoryTwo = c2 }
      where
        c2 = map f loglines
        f :: (String, LogLine) -> (Filename, [([Variable], Specification)])
        f (fn, (_, Left vspecs)) = (fn, vspecs)
        f (fn, (_, Right _)) = (fn, [])
    CA.ARFailure ss msg -> return mempty
