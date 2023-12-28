{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Horch.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Yaml as Yaml
import Data.Text (Text)
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative
import Data.Maybe
import Text.Read (Lexeme(Ident))
import qualified Data.Text as T

type Parser = Parsec Void Text

data TensorType = TensorType
  { group :: Maybe ( Text, Bool )
  } deriving (Eq, Ord, Show)

data Type
  = TypeTensor TensorType
  | TypeSymInt
  | TypeScalarType
  | TypeBool
  | TypeDevice
  | TypeLayout
  | TypeArray Type (Maybe Int)
  | TypeInt
  | TypeFloat
  | TypeDimname
  | TypeGenerator
  | TypeMemoryFormat
  | TypeScalar
  | TypeStr
  | TypeOptional Type
  | TypeQScheme
  | TypeStorage
   deriving (Eq, Ord, Show)

data ReturnType
  = ReturnTypeVoid
  | ReturnTypeBase Type
  | ReturnTypeTuple [ ( Type, Maybe Text ) ]
    deriving (Eq, Ord, Show)

data PythonValue
  = PythonNone
  | PythonBool Bool
  | PythonFloat Double
  | PythonInt Int
  | PythonList [ PythonValue ]
  | PythonIdent Text
  | PythonString Text
    deriving (Eq, Ord, Show)

data Parameter = Parameter
  { name :: Text
  , paramType :: Type
  , defaultValue :: Maybe PythonValue
  } deriving (Eq, Ord, Show)

data FunDecl = FunDecl
  { name :: Text
  , overload :: Maybe Text
  , parameters :: [ Parameter ]
  , returnType :: ReturnType
  } deriving (Eq, Ord, Show)

ident :: Parser Text
ident = takeWhile1P Nothing (\c -> 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || '0' <= c && c <= '9' || c == '_')

type_ :: Parser Type
type_ = do
    t <- baseType
    modifier t
  where
    baseType = asum
      [ do
          _ <- "Tensor"
          group <- optional $ do
            _ <- "("
            groupName <- ident
            writtenTo <- True <$ "!" <|> pure False
            _ <- optional " -> *" -- not sure what this means yet
            _ <- ")"
            return ( groupName, writtenTo )
          return $ TypeTensor TensorType { group }
      , TypeSymInt <$ "SymInt" 
      , TypeScalarType <$ "ScalarType"
      , TypeLayout <$ "Layout"
      , TypeDevice <$ "Device"
      , TypeDimname <$ "Dimname"
      , TypeGenerator <$ "Generator"
      , TypeMemoryFormat <$ "MemoryFormat"
      , TypeBool <$ "bool" 
      , TypeInt <$ "int"
      , TypeFloat <$ "float"
      , TypeScalar <$ "Scalar"
      , TypeStr <$ "str"
      , TypeQScheme <$ "QScheme"
      , TypeStorage <$ "Storage"
      ]
    modifier :: Type -> Parser Type
    modifier t = asum
      [ try $ do
          _ <- "["
          count <- optional L.decimal
          _ <- "]"
          modifier (TypeArray t count)
      , try $ do
        _ <- "?"
        modifier (TypeOptional t)
      , pure t
      ] 

parseReturnType :: Parser ReturnType
parseReturnType = do
  asum
    [ ReturnTypeVoid <$ "()"
    , ReturnTypeBase <$> type_
    , do
        _ <- "("
        tupleElems <- sepBy1 ((,) <$> type_ <*> optional (space *> ident)) ", "
        _ <- ")"
        return (ReturnTypeTuple tupleElems)
    ]

pythonValue :: Parser PythonValue
pythonValue = asum
  [ PythonNone <$ "None"
  , PythonBool <$> (True <$ "True" <|> False <$ "False")
  , try $ PythonFloat <$> L.signed (pure ()) L.float
  , PythonInt <$> L.signed (pure ()) L.decimal
  , do
      _ <- "["
      values <- pythonValue `sepBy` ","
      space
      _ <- "]"
      return (PythonList values)
  , PythonIdent <$> ident
  , do
      delim <- oneOf ['\'', '"' ]
      string <- manyTill L.charLiteral (char delim)
      return (PythonString (T.pack string))
  ]

parameter :: Parser Parameter
parameter = do
  paramType <- type_
  space
  name <- ident
  defaultValue <- optional ("=" *> pythonValue)
  return Parameter { paramType, name, defaultValue }

functionDecl :: Parser FunDecl
functionDecl = do
  name <- ident
  overload <- optional ("." *> ident)
  _ <- "("
  parameters <- catMaybes <$> sepBy (Nothing <$ "*" <|> Just <$> parameter) ", "
  _ <- ") -> "
  returnType <- parseReturnType

  return FunDecl { name, overload, parameters, returnType }