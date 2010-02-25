{-# LANGUAGE ForeignFunctionInterface #-}

#include "HOpenCV.h"

module HOpenCV where 

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import C2HS


{#pointer *IplImage as IplImage #}
{#pointer *CvCapture as CvCapture #}


{#fun new_capture as ^
      {fromIntegral `Int'} -> `CvCapture' id#}

{#fun del_capture as ^
      {id `CvCapture'} -> `()' id#}

{#fun query_frame as ^
      {id `CvCapture'} -> `IplImage' id#}

{#fun new_window as ^
      {fromIntegral `Int', fromIntegral `Int'} -> `()' id#}

{#fun del_window as ^
      {fromIntegral `Int'} -> `()' id#}

{#fun show_image as ^
      {fromIntegral `Int', id `IplImage'} -> `()' id#}

{#fun wait_key as ^
      {fromIntegral `Int'} -> `()' id#}
