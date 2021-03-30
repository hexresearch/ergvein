-- |
module Data.Ergo.MIR.Constants where

import Data.Word

c_MaxPrimTypeCode, c_PrimRange :: Word8
c_TupleTypeCode, c_SAnyTypeCode, c_SUnitTypeCode :: Word8
c_SBoxTypeCode, c_SAvlTreeTypeCode, c_SContextTypeCode :: Word8
c_SStringTypeCode, c_STypeVarTypeCode, c_SHeaderTypeCode :: Word8
c_SPreHeaderTypeCode, c_SGlobalTypeCode :: Word8
-- | Maximum type code for primitive types
c_MaxPrimTypeCode    = 11
c_PrimRange          = c_MaxPrimTypeCode + 1
c_TupleTypeCode      = (c_MaxPrimTypeCode + 1) * 8
c_SAnyTypeCode       = 97
c_SUnitTypeCode      = 98
c_SBoxTypeCode       = 99
c_SAvlTreeTypeCode   = 100
c_SContextTypeCode   = 101
c_SStringTypeCode    = 102
c_STypeVarTypeCode   = 103
c_SHeaderTypeCode    = 104
c_SPreHeaderTypeCode = 105
c_SGlobalTypeCode    = 106

c_LastConstantCode,c_LastDataType :: Word8
c_LastConstantCode = c_LastDataType + 1
c_LastDataType     = 111
