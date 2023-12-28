{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Horch.CodeGen where

import Horch.Parser

import Language.Haskell.TH
import Language.C.Inline.Cpp.Exception
import Language.Haskell.TH.Quote (QuasiQuoter(quoteExp))
import qualified Data.Text as T
import Data.List
import Data.Traversable
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Maybe

import Horch.Types
import Foreign
import Horch.Util
import Control.Monad.Primitive
import GHC.Stack (HasCallStack)
import Foreign.C
import Control.Monad.Cont
import Horch.Types (Tensor(tensorForeignPtr))
import Language.Haskell.TH.Syntax (Quasi(qRunIO))
import Data.Coerce (coerce)
import qualified Data.Text.Foreign as T
import Data.Char

paramName :: Parameter -> String -> String
paramName param suffix = prefix ++ T.unpack param.name ++ suffix
  where
    prefix
      | isUpper (T.head param.name) || param.name == "data" = "_"
      | otherwise = ""

renderFunction :: HasCallStack => FunDecl -> String
renderFunction funDecl = "void { " ++ preamble ++ (if funDecl.returnType /= ReturnTypeVoid then " auto result = " else "") ++ invocation ++ "; " ++ marshalOut ++ " }"
  where
    preamble = concatMap paramPreamble funDecl.parameters
    paramPreamble param =
        case paramType of
          TypeArray TypeTensor {} Nothing -> opaqueArrayPreamble "at::Tensor"
          TypeArray (TypeOptional TypeTensor {}) Nothing ->
            let doCopy = unlines
                  [ "for (size_t i = 0; i < $(size_t " ++ paramName param "_len)" ++ "; i++) {"
                  , "const at::Tensor* elemPtr = $(const at::Tensor** " ++ paramName param "_ptr" ++ ")[i];"
                  , paramName param "_list" ++ ".emplace_back(elemPtr != nullptr ? c10::optional<at::Tensor>(*elemPtr) : c10::optional<at::Tensor>());"
                  , "}" 
                  ]
            in "c10::List<c10::optional<at::Tensor>> " ++ paramName param "_list" ++ ";" ++
              if isOptional
                then "if ($(int " ++ paramName param "_present" ++ ") == 1) { " ++ doCopy ++ "}"
                else doCopy
          TypeArray TypeDimname _ -> opaqueArrayPreamble "at::Dimname"
          TypeArray TypeScalar Nothing -> opaqueArrayPreamble "at::Scalar"
          TypeArray TypeBool (Just size) -> 
            let doCopy = "for (size_t i = 0; i < " ++ show size ++ "; i++) { " ++ paramName param "_array" ++ "[i] = $(const int* " ++ paramName param "_ptr)" ++ "[i] == 1; }"
            in 
              "std::array<bool, " ++ show size ++ "> " ++ paramName param "_array" ++ ";" ++
                if isOptional
                  then "if ($(int " ++ paramName param "_present" ++ ") == 1) { " ++ doCopy ++ "}"
                  else doCopy
          TypeArray TypeSymInt _ ->
            let doCopy = "for (size_t i = 0; i < $(size_t " ++ paramName param "_len)" ++ "; i++) { " ++ paramName param "_vector" ++ ".emplace_back($(const int64_t* " ++ paramName param "_ptr" ++ ")[i]);}" 
            in "std::vector<c10::SymInt> " ++ paramName param "_vector" ++ ";" ++
              if isOptional
                then "if ($(int " ++ paramName param "_present" ++ ") == 1) { " ++ doCopy ++ "}"
                else doCopy
          _ -> ""
        where
          (isOptional, paramType) = case param.paramType of
            TypeOptional a -> (True, a)
            a -> (False, a)
          opaqueArrayPreamble typeName =
            "std::vector<" ++ typeName ++ "> " ++ paramName param "_vector;" ++
              if isOptional
                then "if ($(int " ++ paramName param "_present" ++ ") == 1) { " ++ doCopy ++ "}"
                else doCopy
            where doCopy = "for (size_t i = 0; i < $(size_t " ++ paramName param "_len)" ++ "; i++) { " ++ paramName param "_vector" ++ ".emplace_back(*$(" ++ typeName ++ "** " ++ paramName param "_ptr" ++ ")[i]);}"
    renderParameter param =
        if isOptional
          then "$(int " ++ paramName param "_present" ++ ") == 1 ? c10::optional<std::decay<decltype(" ++ baseParam ++ ")>::type>(" ++ baseParam ++ ") : c10::optional<std::decay<decltype(" ++ baseParam ++ ")>::type>()"
          else baseParam
        where
          (isOptional, paramType) = case param.paramType of
            TypeOptional a -> (True, a)
            a -> (False, a)
          baseParam = case paramType of
            TypeTensor {} -> "*$(at::Tensor* " ++ paramName param "_ptr)"
            TypeDevice {} -> "*$(at::Device* " ++ paramName param "_ptr)"
            TypeGenerator {} -> "*$(at::Generator* " ++ paramName param "_ptr)"
            TypeScalar {} -> "*$(at::Scalar* " ++ paramName param "_ptr)"
            TypeDimname {} -> "*$(at::Dimname* " ++ paramName param "_ptr)"
            TypeStorage {} -> "*$(at::Storage* " ++ paramName param "_ptr)"
            TypeBool -> "$(int " ++ paramName param "_c" ++ ") == 1"
            TypeScalarType -> "static_cast<c10::ScalarType>($(int " ++ paramName param "_c" ++ "))"
            TypeLayout -> "static_cast<c10::Layout>($(int " ++ paramName param "_c" ++ "))"
            TypeMemoryFormat -> "static_cast<c10::MemoryFormat>($(int " ++ paramName param "_c" ++ "))"
            TypeArray TypeSymInt _ -> "c10::makeArrayRef(" ++ paramName param "_vector" ++ ")"
            TypeArray TypeInt _ -> "c10::IntArrayRef($(const int64_t* " ++ paramName param "_ptr)" ++ ", $(size_t " ++ paramName param "_len)" ++ ")"
            TypeArray TypeFloat _ -> "c10::ArrayRef<double>($(const double* " ++ paramName param "_ptr)" ++ ", $(size_t " ++ paramName param "_len)" ++ ")"
            TypeArray TypeTensor {} Nothing -> "c10::makeArrayRef(" ++ paramName param "_vector" ++ ")"
            TypeArray (TypeOptional TypeTensor {}) Nothing -> paramName param "_list"
            TypeArray TypeDimname _ -> "c10::makeArrayRef(" ++ paramName param "_vector" ++ ")"
            TypeArray TypeScalar Nothing -> "c10::makeArrayRef(" ++ paramName param "_vector" ++ ")"
            TypeArray TypeBool (Just _) -> paramName param "_array"
            TypeInt -> "$(int64_t " ++ paramName param "_c" ++ ")"
            TypeSymInt -> "c10::SymInt($(int64_t " ++ paramName param "_c" ++ "))"
            TypeFloat -> "$(double " ++ paramName param "_c" ++ ")"
            TypeStr -> "c10::string_view($(const char* " ++ paramName param "_ptr" ++ "), $(size_t " ++ paramName param "_len" ++ "))"
            other -> error ("TODO: " ++ show other)
    invocation = "at::_ops::" ++ T.unpack (funDecl.name <> maybe "" (\overload -> "_" <> overload) funDecl.overload) ++ "::call(" ++ intercalate ", " (map renderParameter funDecl.parameters) ++ ")"
    returnTypes = case funDecl.returnType of
      ReturnTypeVoid -> []
      ReturnTypeBase t -> [ t ]
      ReturnTypeTuple ts -> map fst ts
    marshalOut = concat $ zipWith marshalRes [0..] returnTypes
    marshalRes idx type_ =
      case type_ of
        TypeTensor {} -> "*$(at::Tensor** result_" ++ show idx ++ ") = new at::Tensor(" ++ result ++ ");"
        TypeQScheme {} -> "*$(at::QScheme** result_" ++ show idx ++ ") = new at::QScheme(" ++ result ++ ");"
        TypeScalar {} -> "*$(at::Scalar** result_" ++ show idx ++ ") = new at::Scalar(" ++ result ++ ");"
        TypeBool -> "*$(int* result_" ++ show idx ++ ") = " ++ result ++ " ? 1 : 0;"
        TypeInt -> "*$(int64_t* result_" ++ show idx ++ ") = " ++ result ++ ";"
        TypeFloat -> "*$(double* result_" ++ show idx ++ ") = " ++ result ++ ";"
        TypeArray TypeTensor {} _ -> unlines
          [ "*$(at::Tensor*** result_" ++ show idx ++ ") = static_cast<at::Tensor**>(malloc(sizeof(at::Tensor*) * " ++ result ++ ".size()));"
          , "for (int i = 0; i < " ++ result ++ ".size(); i++) { (*$(at::Tensor*** result_" ++ show idx ++ "))[i] = new at::Tensor(" ++ result ++ "[i]); }"
          , "*$(size_t* result_" ++ show idx ++ "_count) = " ++ result ++ ".size();"
          ]
        TypeArray TypeInt {} _ -> unlines
          [ "*$(int64_t** result_" ++ show idx ++ ") = static_cast<int64_t*>(malloc(sizeof(int64_t) * " ++ result ++ ".size()));"
          , "for (int i = 0; i < " ++ result ++ ".size(); i++) { (*$(int64_t** result_" ++ show idx ++ "))[i] = " ++ result ++ "[i]; }"
          , "*$(size_t* result_" ++ show idx ++ "_count) = " ++ result ++ ".size();"
          ]
        TypeScalarType -> "*$(int* result_" ++ show idx ++ ") = static_cast<int>(" ++ result ++ ");"
        other -> error ("TODO: " ++ show other)
      where
        result = if length returnTypes == 1 then "result" else "std::get<" ++ show idx ++ ">(result)" 

marshalResultList :: Storable a => Ptr (Ptr a) -> Ptr CSize -> IO (VS.Vector a)
marshalResultList mallocPtrs sizePtr = do
  size <- peek sizePtr
  ptr <- peek mallocPtrs
  fptr <- newForeignPtr finalizerFree ptr
  return $ VS.unsafeFromForeignPtr0 fptr (fromIntegral size)

marshalFPtrArray
  :: (a -> Maybe (ForeignPtr b))
  -> Maybe (V.Vector a)
  -> (CSize -> Ptr (Ptr b) -> IO r) -> IO r
marshalFPtrArray toMbForeignPtr mbVec cont =
  let marshalElem elem = ContT $ \elemCont ->
        case toMbForeignPtr elem of
          Nothing -> elemCont nullPtr
          Just fptr -> withForeignPtr fptr elemCont
   in case mbVec of
        Nothing -> cont 0 nullPtr
        Just vec -> runContT (traverse marshalElem vec) $ \ptrVec ->
          VS.unsafeWith (V.convert ptrVec) (cont (fromIntegral (V.length vec) :: CSize))

genFunction :: HasCallStack => FunDecl -> DecsQ
genFunction funDecl = do
  let params = map (\param -> varP $ mkName $ paramName param "") funDecl.parameters

  let baseCall = quoteExp throwBlock (renderFunction funDecl)
  let unwrapParam param rest =
        if isOptional
          then [e|
              let $(varP (mkName (paramName param "_present"))) = fromBool (isJust $(paramExp))
              in $marshalParam
            |]
          else marshalParam
          where
            ( isOptional, paramType ) = case param.paramType of
              TypeOptional a -> ( True, a )
              a -> ( False, a )
            baseParamExp = varE (mkName (paramName param ""))
            paramExp = if isOptional
              then baseParamExp
              else [e| Just $(baseParamExp) |]
            marshalParam = case paramType of
              TypeTensor {} -> marshalOpaque [e|tensorForeignPtr|]
              TypeDevice -> marshalOpaque [e|deviceForeignPtr|]
              TypeGenerator -> marshalOpaque [e|generatorForeignPtr|]
              TypeScalar -> marshalOpaque [e|scalarForeignPtr|]
              TypeDimname -> marshalOpaque [e|dimnameForeignPtr|]
              TypeStorage -> marshalOpaque [e|storageForeignPtr|]
              TypeArray TypeSymInt _ -> [e|
                let cont $(varP (mkName (paramName param "_len"))) $(varP (mkName (paramName param "_ptr"))) = $rest
                in case $(paramExp) of
                  Nothing -> cont 0 nullPtr
                  Just vec -> VS.unsafeWith (V.convert vec) (cont (fromIntegral (V.length vec) :: CSize))
                |]
              TypeArray (TypeTensor {}) _ -> marshalOpaqueArray [e|Just . tensorForeignPtr|]
              TypeArray (TypeOptional TypeTensor {}) _ -> marshalOpaqueArray [e|fmap tensorForeignPtr|]
              TypeArray TypeDimname _ -> marshalOpaqueArray [e|Just . dimnameForeignPtr|]
              TypeArray TypeScalar Nothing -> marshalOpaqueArray [e|Just . scalarForeignPtr|]
              TypeArray TypeInt _ -> marshalArray paramExp
              TypeArray TypeFloat _ -> marshalArray [e| coerce $paramExp |]
              TypeArray TypeBool mbLen -> [e|
                 let cont $(varP (mkName (paramName param "_len"))) $(varP (mkName (paramName param "_ptr"))) = $rest
                 in case $(paramExp) of
                      Nothing -> cont 0 nullPtr
                      Just vec -> do
                        case mbLen of
                          Just size | V.length vec /= size -> fail ("Expected a vector with " ++ show size ++ " elements") 
                          Nothing -> return ()
                        VS.unsafeWith (V.convert (fmap fromBool vec)) (cont (fromIntegral (V.length vec) :: CSize))
                |]
              TypeStr -> [e|
                let cont $(varP (mkName (paramName param "_ptr"))) $(varP (mkName (paramName param "_len"))) = $rest
                 in case $(paramExp) of
                      Nothing -> cont nullPtr 0
                      Just str -> T.withCStringLen str $ \( ptr, len) -> cont ptr (fromIntegral len)
                |]
              TypeScalarType -> marshalPrimitive [e|0|] [e|scalarTypeToInt|]
              TypeLayout -> marshalPrimitive [e|0|] [e|layoutToInt|]
              TypeMemoryFormat -> marshalPrimitive [e|0|] [e|memoryFormatToInt|]
              TypeBool -> marshalPrimitive [e|0|] [e|fromBool|]
              TypeInt -> marshalPrimitive [e|0|] [e|id|]
              TypeSymInt -> marshalPrimitive [e|0|] [e|id|]
              TypeFloat -> marshalPrimitive [e|0|] [e|coerce|]
              other -> error ("TODO: " ++ show other)
            marshalPrimitive def toPrimitive =
              [e|
                let $(varP (mkName (paramName param "_c"))) = maybe $def $toPrimitive $(paramExp)
                 in $rest
                |]
            marshalOpaque toForeignPtr =
              [e|
                let cont $(varP (mkName (paramName param "_ptr"))) = $rest
                 in case $(paramExp) of
                      Nothing -> cont nullPtr
                      Just obj -> withForeignPtr ($toForeignPtr obj) cont
                |]
            marshalArray exp =
              [e|
                let cont $(varP (mkName (paramName param "_len"))) $(varP (mkName (paramName param "_ptr"))) = $rest
                 in case $exp of
                      Nothing -> cont 0 nullPtr
                      Just vec -> VS.unsafeWith (V.convert vec) (cont (fromIntegral (V.length vec) :: CSize))
                |]
            marshalOpaqueArray toMbForeignPtr = [e|
              marshalFPtrArray $toMbForeignPtr $paramExp $
                \ $(varP (mkName (paramName param "_len"))) $(varP (mkName (paramName param "_ptr"))) -> $rest
              |]
  
  let handleParameters = foldr unwrapParam baseCall funDecl.parameters
        
  let returnTypes = case funDecl.returnType of
        ReturnTypeVoid -> []
        ReturnTypeBase t -> [t]
        ReturnTypeTuple ts -> map fst ts

  let processResults = do
        let resultName idx = mkName ("result_" ++ show idx)
        let resultCountName idx = mkName ("result_" ++ show idx ++ "_count")
        let addAlloca ( idx, returnType ) rest = case returnType of
              TypeArray {} ->
                [e| alloca $ \ $(varP (resultName idx)) ->
                      alloca $ \ $(varP (resultCountName idx)) ->
                        $rest 
                |]
              _ -> [e| alloca $ \ $(varP (resultName idx)) -> $rest |]
        let readResult idx type_ =
              case type_ of
                TypeTensor {} -> [e| peek $result >>= newForeignPtr deleteTensor >>= (pure . Tensor) |]
                TypeQScheme -> [e| peek $result >>= newForeignPtr deleteQScheme >>= (pure . QScheme) |]
                TypeScalar -> [e| peek $result >>= newForeignPtr deleteScalar >>= (pure . Scalar) |]
                TypeArray TypeTensor {} _ -> [e|
                  (traverse (fmap Tensor . newForeignPtr deleteTensor) . V.convert) 
                  =<< marshalResultList
                    $result
                    $(varE (resultCountName idx))
                  |]
                TypeArray TypeInt _ -> [e| fmap V.convert $ marshalResultList
                    $result
                    $(varE (resultCountName idx))
                  |]
                TypeBool -> [e| toBool <$> peek $result |]
                TypeInt -> [e| peek $result |]
                TypeFloat -> [e| coerce <$> peek $result |]
                TypeScalarType -> [e| scalarTypeFromInt <$> peek $result |]
                other -> error ("TODO: " ++ show other)
              where
                result = varE (resultName idx)
        let readResults = case returnTypes of
              [] -> [e| return () |]
              [ t ] -> readResult 0 t
              ts -> do
                let firstRead : otherReads = zipWith readResult [0..] ts
                let tupleCon = case length returnTypes of
                      2 -> [e|(,)|]
                      3 -> [e|(,,)|]
                      4 -> [e|(,,,)|]
                      5 -> [e|(,,,,)|]
                      6 -> [e|(,,,,,)|]
                      7 -> [e|(,,,,,,)|]
                      other -> error ("TODO: " ++ show other)
                foldl (\exp r -> [e| $exp <*> $r |] ) [e|$tupleCon <$> $firstRead |] otherReads
        let baseAndRead = [e| $handleParameters >> $readResults |]
        foldr addAlloca baseAndRead (zip [0..] returnTypes)

  let body = [e|unsafeIOToPrim $ mask_ $processResults |]

  let funcName = mkName (T.unpack (fixedName <> maybe "" (\overload -> "_" <> overload) funDecl.overload))
        where
          fixedName = case funDecl.name of
            "where" -> "where_"
            "data" -> "data_"
            other -> other

  let monadVar = mkName "m"
  let stateVar = mkName "s"

  let callReturnTypeBase type_ = case type_ of
        TypeTensor {} -> [t| Tensor $(varT stateVar) |]
        TypeDimname {} -> [t| Dimname |]
        TypeBool -> [t| Bool |]
        TypeArray t Nothing -> [t| V.Vector $(callReturnTypeBase t) |]
        TypeInt -> [t| Int64 |]
        TypeFloat -> [t| Double |]
        TypeQScheme -> [t| QScheme |]
        TypeScalar -> [t| Scalar |]
        TypeScalarType -> [t| ScalarType |]
        other -> error ("TODO: " ++ show other)

  let callReturnType = case funDecl.returnType of
        ReturnTypeBase type_ -> callReturnTypeBase type_
        ReturnTypeVoid -> [t| () |]
        ReturnTypeTuple entries -> foldM (\acc type_ -> pure acc `appT` callReturnTypeBase type_) (TupleT (length entries)) (map fst entries)

  let addParamType param rest = [t| $(paramType param.paramType) -> $rest |]
        where
          paramType t = case t of
            TypeTensor {} -> [t| Tensor $(varT stateVar) |]
            TypeArray a _ -> [t| V.Vector $(paramType a) |]
            TypeBool -> [t| Bool |]
            TypeScalarType -> [t| ScalarType |]
            TypeLayout -> [t| Layout |]
            TypeDevice -> [t| Device |]
            TypeDimname -> [t| Dimname |]
            TypeSymInt -> [t| Int64 |]
            TypeInt -> [t| Int64 |]
            TypeFloat -> [t| Double |]
            TypeGenerator -> [t| Generator |]
            TypeMemoryFormat -> [t| MemoryFormat |]
            TypeScalar -> [t| Scalar |]
            TypeStr -> [t| T.Text |]
            TypeOptional a -> [t| Maybe $(paramType a) |]
            TypeQScheme -> [t| QScheme |]
            TypeStorage -> [t| Storage |]

  let expType =
        forallT
          [ PlainTV monadVar SpecifiedSpec, PlainTV stateVar SpecifiedSpec ]
          (fmap (:[]) [t| ( PrimMonad $(varT monadVar), $(varT stateVar) ~ PrimState $(varT monadVar) ) |])
          $ foldr addParamType (varT monadVar `appT` callReturnType) funDecl.parameters
  sig <- sigD funcName expType

  func <- funD funcName [ clause params (normalB body) [] ]

  return [ sig, func ]    