{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Horch.Context where

import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline as C
import qualified Language.C.Types as C
import qualified Data.Map as Map

import Horch.Types
import Language.Haskell.TH
import qualified Language.C.Inline.Cpp as C

horchContext :: C.Context
horchContext =
  let ctx =
        mempty
          { C.ctxTypesTable =
              Map.fromList
                [ (C.TypeName "at::Tensor", [t| C'Tensor |])
                , (C.TypeName "at::Device", [t| C'Device |])
                , (C.TypeName "at::Dimname", [t| C'Dimname |])
                , (C.TypeName "at::Generator", [t| C'Generator |])
                , (C.TypeName "at::Scalar", [t| C'Scalar |])
                , (C.TypeName "at::QScheme", [t| C'QScheme |])
                , (C.TypeName "at::Storage", [t| C'Storage |])
                , (C.TypeName "std::string", [t| C'string |])
                ]
          }
   in C.cppCtx <> C.fptrCtx <> C.vecCtx <> ctx

horchIncludes :: DecsQ
horchIncludes = concat <$> sequence
  [ C.include "<ATen/Functions.h>"
  , C.include "<ATen/Tensor.h>"
  , C.include "<ATen/core/Formatting.h>"
  , C.include "<iostream>"
  ]