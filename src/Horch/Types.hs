module Horch.Types where

import Foreign.ForeignPtr

data C'Tensor
data C'Device
data C'Dimname
data C'Generator
data C'Scalar
data C'QScheme
data C'Storage
data C'string
  
newtype Tensor s = Tensor { tensorForeignPtr :: ForeignPtr C'Tensor }
newtype Device = Device { deviceForeignPtr :: ForeignPtr C'Device }
newtype Dimname = Dimname { dimnameForeignPtr :: ForeignPtr C'Dimname }
newtype Generator = Generator { generatorForeignPtr :: ForeignPtr C'Generator }
newtype Scalar = Scalar { scalarForeignPtr :: ForeignPtr C'Scalar }
newtype QScheme = QScheme { qSchemeForeignPtr :: ForeignPtr C'QScheme }
newtype Storage = Storage { storageForeignPtr :: ForeignPtr C'Storage }

data ScalarType
  = ScalarByte
  | ScalarChar
  | ScalarShort
  | ScalarInt
  | ScalarLong
  | ScalarHalf
  | ScalarFloat
  | ScalarDouble
  | ScalarComplexHalf
  | ScalarComplexFloat
  | ScalarComplexDouble
  | ScalarBool
  | ScalarQInt8
  | ScalarQUInt8
  | ScalarQInt32
  | ScalarBFloat16
  | ScalarQUInt4x2
  | ScalarQUInt2x4
    deriving (Eq, Ord, Show)


data Layout
  = LayoutStrided
  | LayoutSparse
  | LayoutSparseCsr
  | LayoutMkldnn
  | LayoutSparseCsc
  | LayoutSparseBsr
  | LayoutSparseBsc
    deriving (Eq, Ord, Show)

data MemoryFormat
  = MemoryFormatContiguous
  | MemoryFormatPreserve
  | MemoryFormatChannelsLast
  | MemoryFormatChannelsLast3d
  deriving (Eq, Ord, Show)