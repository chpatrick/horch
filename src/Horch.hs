{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Horch where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Control.Monad.Primitive
import qualified Data.Vector.Storable as VS

import qualified Data.Map as Map
import qualified Language.C.Types as C
import Control.Exception
import Foreign
import Foreign.C (peekCString)
import Data.Traversable
import Data.Foldable
import qualified System.FilePath as FP

import Horch.Types
import Horch.CodeGen
import Horch.Parser
import Horch.Util
import Text.Megaparsec
import Horch.Context (horchContext, horchIncludes)
import Data.Void
import Language.Haskell.TH.Syntax
import qualified Data.Yaml as Yaml
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Haskell.TH.Syntax (Quasi(qRunIO))
import Text.Megaparsec (errorBundlePretty)
import Data.Yaml (parseMonad)

type Shape = VS.Vector Int64

C.context horchContext
horchIncludes

-- ones :: PrimMonad m => Shape -> m (Tensor (PrimState m))
-- ones shape = initTensor [C.throwBlock| at::Tensor* {
--   return new at::Tensor(at::ones({ $vec-ptr:(const int64_t* shape), static_cast<size_t>($vec-len:shape) }));
-- }|]

toString :: PrimMonad m => Tensor (PrimState m) -> m String
toString (Tensor fptr) = unsafeIOToPrim $ bracket
  [C.throwBlock| std::string* {
    std::stringstream stream;
    stream << *$fptr-ptr:(at::Tensor* fptr);
    return new std::string(stream.str());
  }|]
  (\stringPtr -> [C.exp| void { delete $(std::string* stringPtr) } |])
  (\stringPtr -> [C.exp| const char* { $(std::string* stringPtr)->c_str() } |] >>= peekCString)

-- plus :: PrimMonad m => Tensor (PrimState m) -> Tensor (PrimState m) -> m (Tensor (PrimState m))
-- plus x y = initTensor [C.throwBlock| at::Tensor* {
--   return new at::Tensor(*$fptr-ptr:(at::Tensor* x) + *$fptr-ptr:(at::Tensor* y));
-- }|]

-- genFunction $ either undefined id $ parse functionDecl "" "sin(Tensor self) -> Tensor"
-- genFunction $ either undefined id $ parse functionDecl "" "ones(SymInt[] size, *, ScalarType? dtype=None, Layout? layout=None, Device? device=None, bool? pin_memory=None) -> Tensor"
-- genFunction $ either undefined id $ parse functionDecl "" "linear(Tensor input, Tensor weight, Tensor? bias=None) -> Tensor"
do
  loc <- location
  nativeFuncs :: [ Yaml.Object ] <- qRunIO $ Yaml.decodeFileThrow (FP.takeDirectory (loc_filename loc) FP.</> "native_functions.yaml")
  fmap concat $ for (take 2000 nativeFuncs) $ \func -> do
    manualCppBinding :: Maybe Bool <- parseMonad (Yaml..:? "manual_cpp_binding") func
    funcString :: T.Text <- parseMonad (Yaml..: "func") func
    qRunIO $ putStrLn ("Generating " ++ T.unpack funcString)
    case parse functionDecl "function string" funcString of
      Left err -> fail (errorBundlePretty err)
      Right funDecl -> genFunction funDecl