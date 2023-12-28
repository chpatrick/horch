{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Horch.Util where

import qualified Language.C.Inline as C
import Control.Monad.Primitive

import Horch.Context
import Foreign
import Horch.Types
import Control.Exception
import Foreign.C

C.context horchContext
horchIncludes

deleteTensor :: FunPtr (Ptr C'Tensor -> IO ())
deleteTensor = [C.funPtr| void deleteTensor(at::Tensor* ptr) { delete ptr; } |]

deleteQScheme :: FunPtr (Ptr C'QScheme -> IO ())
deleteQScheme = [C.funPtr| void deleteQScheme(at::QScheme* ptr) { delete ptr; } |]

deleteScalar :: FunPtr (Ptr C'Scalar -> IO ())
deleteScalar = [C.funPtr| void deleteScalar(at::Scalar* ptr) { delete ptr; } |]

initTensor :: IO (Ptr C'Tensor) -> IO (Tensor s)
initTensor doInit = mask_ (fmap Tensor . newForeignPtr deleteTensor =<< doInit)

scalarTypeToInt :: ScalarType -> CInt
scalarTypeToInt scalarType = case scalarType of
  ScalarByte -> [C.pure| int { static_cast<int>(c10::ScalarType::Byte) } |]
  ScalarChar -> [C.pure| int { static_cast<int>(c10::ScalarType::Char) } |]
  ScalarShort -> [C.pure| int { static_cast<int>(c10::ScalarType::Short) } |]
  ScalarInt -> [C.pure| int { static_cast<int>(c10::ScalarType::Int) } |]
  ScalarLong -> [C.pure| int { static_cast<int>(c10::ScalarType::Long) } |]
  ScalarHalf -> [C.pure| int { static_cast<int>(c10::ScalarType::Half) } |]
  ScalarFloat -> [C.pure| int { static_cast<int>(c10::ScalarType::Float) } |]
  ScalarDouble -> [C.pure| int { static_cast<int>(c10::ScalarType::Double) } |]
  ScalarComplexHalf -> [C.pure| int { static_cast<int>(c10::ScalarType::ComplexHalf) } |]
  ScalarComplexFloat -> [C.pure| int { static_cast<int>(c10::ScalarType::ComplexFloat) } |]
  ScalarComplexDouble -> [C.pure| int { static_cast<int>(c10::ScalarType::ComplexDouble) } |]
  ScalarBool -> [C.pure| int { static_cast<int>(c10::ScalarType::Bool) } |]
  ScalarQInt8 -> [C.pure| int { static_cast<int>(c10::ScalarType::QInt8) } |]
  ScalarQUInt8 -> [C.pure| int { static_cast<int>(c10::ScalarType::QUInt8) } |]
  ScalarQInt32 -> [C.pure| int { static_cast<int>(c10::ScalarType::QInt32) } |]
  ScalarBFloat16 -> [C.pure| int { static_cast<int>(c10::ScalarType::BFloat16) } |]
  ScalarQUInt4x2 -> [C.pure| int { static_cast<int>(c10::ScalarType::QUInt4x2) } |]
  ScalarQUInt2x4 -> [C.pure| int { static_cast<int>(c10::ScalarType::QUInt2x4) } |]

scalarTypeFromInt :: CInt -> ScalarType
scalarTypeFromInt scalarTypeInt
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::Byte) } |] = ScalarByte 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::Char) } |] = ScalarChar 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::Short) } |] = ScalarShort 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::Int) } |] = ScalarInt 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::Long) } |] = ScalarLong 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::Half) } |] = ScalarHalf 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::Float) } |] = ScalarFloat 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::Double) } |] = ScalarDouble 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::ComplexHalf) } |] = ScalarComplexHalf 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::ComplexFloat) } |] = ScalarComplexFloat 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::ComplexDouble) } |] = ScalarComplexDouble 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::Bool) } |] = ScalarBool 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::QInt8) } |] = ScalarQInt8 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::QUInt8) } |] = ScalarQUInt8 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::QInt32) } |] = ScalarQInt32 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::BFloat16) } |] = ScalarBFloat16 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::QUInt4x2) } |] = ScalarQUInt4x2 
  | scalarTypeInt == [C.pure| int { static_cast<int>(c10::ScalarType::QUInt2x4) } |] = ScalarQUInt2x4 
  | otherwise = error ("Unknown scalar type: " ++ show scalarTypeInt)

layoutToInt :: Layout -> CInt
layoutToInt layout = case layout of
  LayoutStrided -> [C.pure| int { static_cast<int>(c10::Layout::Strided) } |]
  LayoutSparse -> [C.pure| int { static_cast<int>(c10::Layout::Sparse) } |]
  LayoutSparseCsr -> [C.pure| int { static_cast<int>(c10::Layout::SparseCsr) } |]
  LayoutMkldnn -> [C.pure| int { static_cast<int>(c10::Layout::Mkldnn) } |]
  LayoutSparseCsc -> [C.pure| int { static_cast<int>(c10::Layout::SparseCsc) } |]
  LayoutSparseBsr -> [C.pure| int { static_cast<int>(c10::Layout::SparseBsr) } |]
  LayoutSparseBsc -> [C.pure| int { static_cast<int>(c10::Layout::SparseBsc) } |]

memoryFormatToInt :: MemoryFormat -> CInt
memoryFormatToInt memoryFormat = case memoryFormat of
  MemoryFormatContiguous -> [C.pure| int { static_cast<int>(c10::MemoryFormat::Contiguous) } |]
  MemoryFormatPreserve -> [C.pure| int { static_cast<int>(c10::MemoryFormat::Preserve) } |]
  MemoryFormatChannelsLast -> [C.pure| int { static_cast<int>(c10::MemoryFormat::ChannelsLast) } |]
  MemoryFormatChannelsLast3d -> [C.pure| int { static_cast<int>(c10::MemoryFormat::ChannelsLast3d) } |]