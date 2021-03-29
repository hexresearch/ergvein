-- |
module Data.Ergo.MIR.Constants where

import Data.Word

c_MaxPrimTypeCode, c_PrimRange, c_TupleTypeCode :: Word8
-- | Maximum type code for primitive types
c_MaxPrimTypeCode = 11
c_PrimRange       = c_MaxPrimTypeCode + 1
c_TupleTypeCode   = (c_MaxPrimTypeCode + 1) * 8

c_LastConstantCode,c_LastDataType :: Word8
c_LastConstantCode = c_LastDataType + 1
c_LastDataType     = 111
