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

data AnalysisInfo = AnalysisInfo
  {
    categoryOne :: [(Filename, FU.Position)]
  }

instance Monoid AnalysisInfo where
  mempty = AnalysisInfo { categoryOne = [] }
  mappend x y =
    AnalysisInfo {
      categoryOne = categoryOne x ++ categoryOne y
    }

analysis :: [(F.ProgramFile A, SourceText)] -> AnalysisInfo
analysis _ = mempty
