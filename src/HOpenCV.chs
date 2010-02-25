{-# LANGUAGE ForeignFunctionInterface #-}

#include "HOpenCV.h"

module HOpenCV where 

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable


{#pointer *IplImage as IplImage #}
{#pointer *CvCapture as CvCapture #}

{#fun new_capture as ^
      {fromIntegral `Int'} -> `CvCapture' id#}

{#fun del_capture as ^
      {id `CvCapture'} -> {}#}

{#fun query_frame as ^
      {id `CvCapture'} -> `IplImage' id#}


main = do
  capture <- newCapture
  frame <- queryFrame capture
  delCapture capture


