-- Core analysis

module Analysis where

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU

import Camfort.Analysis.Annotations  (A, unitAnnotation)
import Camfort.Helpers

import Data.Monoid
import Data.Foldable (foldMap)
import qualified Data.Map as M

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
  }
  deriving (Show, Eq)

-- Used to represent an array and any global (non-induction) variables
-- used to index it
data BulkDataArrayInfo = Array
  {
    arrayName  :: F.Name
  -- , declPos    :: FU.Position
  , indexNames :: [F.Name]
  }
  deriving (Show, Eq)

instance Monoid AnalysisInfo where
  mempty = AnalysisInfo { categoryOne = [] }
  mappend x y =
    AnalysisInfo {
      categoryOne = categoryOne x ++ categoryOne y
    }

analysis :: [(F.ProgramFile A, SourceText)] -> AnalysisInfo
analysis = foldMap analysisSingleFile

-- singleFile for now, but we need to think about cross file definition of
-- the global array
analysisSingleFile :: (F.ProgramFile A, SourceText) -> AnalysisInfo
analysisSingleFile (pf, src) = AnalysisInfo { categoryOne = arrayInfo }
  where
     filename     = F.pfGetFilename pf
     pf'          = FA.initAnalysis pf
     (pf'', tenv) = FAT.analyseTypes pf'

     arrayInfo =
       [ (filename, Array var []) |
            -- forall types in the environment ... which are arrays
            (var, ty@(FA.IDType _ (Just FA.CTArray))) <- M.toList tenv
       ]
