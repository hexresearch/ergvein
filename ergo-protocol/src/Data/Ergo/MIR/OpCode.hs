-- |
module Data.Ergo.MIR.OpCode where

import Data.Word
import Data.Persist
import Data.Ergo.MIR.Constants
import Data.Ergo.MIR.Parser

newtype OpCode = OpCode Word8
  deriving stock   (Eq)
  deriving newtype (Persist)
instance Show OpCode where
  show c
    | c == opTaggedVariableCode                    = "TaggedVariableCode"
    | c == opValUseCode                            = "ValUseCode"
    | c == opConstantPlaceholderCode               = "ConstantPlaceholderCode"
    | c == opSubstConstantsCode                    = "SubstConstantsCode"
    | c == opLongToByteArrayCode                   = "LongToByteArrayCode"
    | c == opByteArrayToBigIntCode                 = "ByteArrayToBigIntCode"
    | c == opByteArrayToLongCode                   = "ByteArrayToLongCode"
    | c == opDowncastCode                          = "DowncastCode"
    | c == opUpcastCode                            = "UpcastCode"
    | c == opTrueCode                              = "TrueCode"
    | c == opFalseCode                             = "FalseCode"
    | c == opUnitConstantCode                      = "UnitConstantCode"
    | c == opGroupGeneratorCode                    = "GroupGeneratorCode"
    | c == opConcreteCollectionCode                = "ConcreteCollectionCode"
    | c == opConcreteCollectionBooleanConstantCode = "ConcreteCollectionBooleanConstantCode"
    | c == opTupleCode                             = "TupleCode"
    | c == opSelect1Code                           = "Select1Code"
    | c == opSelect2Code                           = "Select2Code"
    | c == opSelect3Code                           = "Select3Code"
    | c == opSelect4Code                           = "Select4Code"
    | c == opSelect5Code                           = "Select5Code"
    | c == opSelectFieldCode                       = "SelectFieldCode"
    | c == opLtCode                                = "LtCode"
    | c == opLeCode                                = "LeCode"
    | c == opGtCode                                = "GtCode"
    | c == opGeCode                                = "GeCode"
    | c == opEqCode                                = "EqCode"
    | c == opNeqCode                               = "NeqCode"
    | c == opIfCode                                = "IfCode"
    | c == opAndCode                               = "AndCode"
    | c == opOrCode                                = "OrCode"
    | c == opAtLeastCode                           = "AtLeastCode"
    | c == opMinusCode                             = "MinusCode"
    | c == opPlusCode                              = "PlusCode"
    | c == opXorCode                               = "XorCode"
    | c == opMultiplyCode                          = "MultiplyCode"
    | c == opDivisionCode                          = "DivisionCode"
    | c == opModuloCode                            = "ModuloCode"
    | c == opExponentiateCode                      = "ExponentiateCode"
    | c == opMultiplyGroupCode                     = "MultiplyGroupCode"
    | c == opMinCode                               = "MinCode"
    | c == opMaxCode                               = "MaxCode"
    | c == opHeightCode                            = "HeightCode"
    | c == opInputsCode                            = "InputsCode"
    | c == opOutputsCode                           = "OutputsCode"
    | c == opLastBlockUtxoRootHashCode             = "LastBlockUtxoRootHashCode"
    | c == opSelfCode                              = "SelfCode"
    | c == opMinerPubkeyCode                       = "MinerPubkeyCode"
    | c == opMapCollectionCode                     = "MapCollectionCode"
    | c == opExistsCode                            = "ExistsCode"
    | c == opForAllCode                            = "ForAllCode"
    | c == opFoldCode                              = "FoldCode"
    | c == opSizeOfCode                            = "SizeOfCode"
    | c == opByIndexCode                           = "ByIndexCode"
    | c == opAppendCode                            = "AppendCode"
    | c == opSliceCode                             = "SliceCode"
    | c == opFilterCode                            = "FilterCode"
    | c == opAvlTreeCode                           = "AvlTreeCode"
    | c == opAvlTreeGetCode                        = "AvlTreeGetCode"
    | c == opFlatMapCollectionCode                 = "FlatMapCollectionCode"
    | c == opExtractAmountCode                     = "ExtractAmountCode"
    | c == opExtractScriptBytesCode                = "ExtractScriptBytesCode"
    | c == opExtractBytesCode                      = "ExtractBytesCode"
    | c == opExtractBytesWithNoRefCode             = "ExtractBytesWithNoRefCode"
    | c == opExtractIdCode                         = "ExtractIdCode"
    | c == opExtractRegisterAs                     = "ExtractRegisterAs"
    | c == opExtractCreationInfoCode               = "ExtractCreationInfoCode"
    | c == opCalcBlake2b256Code                    = "CalcBlake2b256Code"
    | c == opCalcSha256Code                        = "CalcSha256Code"
    | c == opProveDlogCode                         = "ProveDlogCode"
    | c == opProveDiffieHellmanTupleCode           = "ProveDiffieHellmanTupleCode"
    | c == opSigmaPropIsProvenCode                 = "SigmaPropIsProvenCode"
    | c == opSigmaPropBytesCode                    = "SigmaPropBytesCode"
    | c == opBoolToSigmaPropCode                   = "BoolToSigmaPropCode"
    | c == opTrivialPropFalseCode                  = "TrivialPropFalseCode"
    | c == opTrivialPropTrueCode                   = "TrivialPropTrueCode"
    | c == opDeserializeContextCode                = "DeserializeContextCode"
    | c == opValDefCode                            = "ValDefCode"
    | c == opFunDefCode                            = "FunDefCode"
    | c == opBlockValueCode                        = "BlockValueCode"
    | c == opFuncValueCode                         = "FuncValueCode"
    | c == opFuncApplyCode                         = "FuncApplyCode"
    | c == opPropertyCallCode                      = "PropertyCallCode"
    | c == opMethodCallCode                        = "MethodCallCode"
    | c == opGlobalCode                            = "GlobalCode"
    | c == opSomeValueCode                         = "SomeValueCode"
    | c == opNoneValueCode                         = "NoneValueCode"
    | c == opGetVarCode                            = "GetVarCode"
    | c == opOptionGetCode                         = "OptionGetCode"
    | c == opOptionGetOrElseCode                   = "OptionGetOrElseCode"
    | c == opOptionIsDefinedCode                   = "OptionIsDefinedCode"
    | c == opModQCode                              = "ModQCode"
    | c == opPlusModQCode                          = "PlusModQCode"
    | c == opMinusModQCode                         = "MinusModQCode"
    | c == opSigmaAndCode                          = "SigmaAndCode"
    | c == opSigmaOrCode                           = "SigmaOrCode"
    | c == opBinOrCode                             = "BinOrCode"
    | c == opBinAndCode                            = "BinAndCode"
    | c == opDecodePointCode                       = "DecodePointCode"
    | c == opLogicalNotCode                        = "LogicalNotCode"
    | c == opNegationCode                          = "NegationCode"
    | c == opBitInversionCode                      = "BitInversionCode"
    | c == opBitOrCode                             = "BitOrCode"
    | c == opBitAndCode                            = "BitAndCode"
    | c == opBinXorCode                            = "BinXorCode"
    | c == opBitXorCode                            = "BitXorCode"
    | c == opBitShiftRightCode                     = "BitShiftRightCode"
    | c == opBitShiftLeftCode                      = "BitShiftLeftCode"
    | c == opBitShiftRightZeroedCode               = "BitShiftRightZeroedCode"
    | c == opCollShiftRightCode                    = "CollShiftRightCode"
    | c == opCollShiftLeftCode                     = "CollShiftLeftCode"
    | c == opCollShiftRightZeroedCode              = "CollShiftRightZeroedCode"
    | c == opCollRotateLeftCode                    = "CollRotateLeftCode"
    | c == opCollRotateRightCode                   = "CollRotateRightCode"
    | c == opContextCode                           = "ContextCode"
    | otherwise                                    = "OpCode " ++ show c

newOpCode :: Word8 -> OpCode
newOpCode shift = OpCode (c_LastConstantCode + shift)


opTaggedVariableCode                    = newOpCode(1)
opValUseCode                            = newOpCode(2)
opConstantPlaceholderCode               = newOpCode(3)
opSubstConstantsCode                    = newOpCode(4)
-- reserved 5 - 9 (5)
opLongToByteArrayCode                   = newOpCode(10)
opByteArrayToBigIntCode                 = newOpCode(11)
opByteArrayToLongCode                   = newOpCode(12)
opDowncastCode                          = newOpCode(13)
opUpcastCode                            = newOpCode(14)

-- EvaluatedValue descendants
opTrueCode                              = newOpCode(15)
opFalseCode                             = newOpCode(16)
opUnitConstantCode                      = newOpCode(17)
opGroupGeneratorCode                    = newOpCode(18)
opConcreteCollectionCode                = newOpCode(19)
-- reserved 20 (1)
opConcreteCollectionBooleanConstantCode = newOpCode(21)

opTupleCode                             = newOpCode(22)
opSelect1Code                           = newOpCode(23)
opSelect2Code                           = newOpCode(24)
opSelect3Code                           = newOpCode(25)
opSelect4Code                           = newOpCode(26)
opSelect5Code                           = newOpCode(27)
opSelectFieldCode                       = newOpCode(28)
-- reserved 29-30 (2)

-- Relation descendants
opLtCode                                = newOpCode(31)
opLeCode                                = newOpCode(32)
opGtCode                                = newOpCode(33)
opGeCode                                = newOpCode(34)
opEqCode                                = newOpCode(35)
opNeqCode                               = newOpCode(36)
opIfCode                                = newOpCode(37)
opAndCode                               = newOpCode(38)
opOrCode                                = newOpCode(39)
opAtLeastCode                           = newOpCode(40)

-- Arithmetic codes
opMinusCode                             = newOpCode(41)
opPlusCode                              = newOpCode(42)
opXorCode                               = newOpCode(43)
opMultiplyCode                          = newOpCode(44)
opDivisionCode                          = newOpCode(45)
opModuloCode                            = newOpCode(46)
opExponentiateCode                      = newOpCode(47)
opMultiplyGroupCode                     = newOpCode(48)
opMinCode                               = newOpCode(49)
opMaxCode                               = newOpCode(50)

-- Environment codes
opHeightCode                            = newOpCode(51)
opInputsCode                            = newOpCode(52)
opOutputsCode                           = newOpCode(53)
opLastBlockUtxoRootHashCode             = newOpCode(54)
opSelfCode                              = newOpCode(55)
-- reserved 56 - 59 (4)
opMinerPubkeyCode                       = newOpCode(60)

-- Collection and tree operations codes
opMapCollectionCode                     = newOpCode(61)
opExistsCode                            = newOpCode(62)
opForAllCode                            = newOpCode(63)
opFoldCode                              = newOpCode(64)
opSizeOfCode                            = newOpCode(65)
opByIndexCode                           = newOpCode(66)
opAppendCode                            = newOpCode(67)
opSliceCode                             = newOpCode(68)
opFilterCode                            = newOpCode(69)
opAvlTreeCode                           = newOpCode(70)
opAvlTreeGetCode                        = newOpCode(71)
opFlatMapCollectionCode                 = newOpCode(72)
-- reserved 73 - 80 (8)

-- Type casts codes
opExtractAmountCode                     = newOpCode(81)
opExtractScriptBytesCode                = newOpCode(82)
opExtractBytesCode                      = newOpCode(83)
opExtractBytesWithNoRefCode             = newOpCode(84)
opExtractIdCode                         = newOpCode(85)
opExtractRegisterAs                     = newOpCode(86)
opExtractCreationInfoCode               = newOpCode(87)
-- reserved 88 - 90 (3)

-- Cryptographic operations codes
opCalcBlake2b256Code                    = newOpCode(91)
opCalcSha256Code                        = newOpCode(92)
opProveDlogCode                         = newOpCode(93)
opProveDiffieHellmanTupleCode           = newOpCode(94)
opSigmaPropIsProvenCode                 = newOpCode(95)
opSigmaPropBytesCode                    = newOpCode(96)
opBoolToSigmaPropCode                   = newOpCode(97)
-- we don't rely on this yet but it's nice to have TrivialPropFalseCode.toUByte < TrivialPropTrueCode.toUByte
opTrivialPropFalseCode                  = newOpCode(98)
opTrivialPropTrueCode                   = newOpCode(99)

-- Deserialization codes
opDeserializeContextCode                = newOpCode(100)
opDeserializeRegisterCode               = newOpCode(101) -- Block codes
opValDefCode                            = newOpCode(102)
opFunDefCode                            = newOpCode(103)
opBlockValueCode                        = newOpCode(104)
opFuncValueCode                         = newOpCode(105)
opFuncApplyCode                         = newOpCode(106)
opPropertyCallCode                      = newOpCode(107)
opMethodCallCode                        = newOpCode(108)
opGlobalCode                            = newOpCode(109)

opSomeValueCode                         = newOpCode(110)
opNoneValueCode                         = newOpCode(111)
-- reserved 112 - 114 (3)

opGetVarCode                            = newOpCode(115)
opOptionGetCode                         = newOpCode(116)
opOptionGetOrElseCode                   = newOpCode(117)
opOptionIsDefinedCode                   = newOpCode(118)

-- Modular arithmetic operations codes
opModQCode                              = newOpCode(119)
opPlusModQCode                          = newOpCode(120)
opMinusModQCode                         = newOpCode(121)

opSigmaAndCode                          = newOpCode(122)
opSigmaOrCode                           = newOpCode(123)
opBinOrCode                             = newOpCode(124)
opBinAndCode                            = newOpCode(125)

opDecodePointCode                       = newOpCode(126)

opLogicalNotCode                        = newOpCode(127)
opNegationCode                          = newOpCode(128)
opBitInversionCode                      = newOpCode(129)
opBitOrCode                             = newOpCode(130)
opBitAndCode                            = newOpCode(131)

opBinXorCode                            = newOpCode(132)

opBitXorCode                            = newOpCode(133)
opBitShiftRightCode                     = newOpCode(134)
opBitShiftLeftCode                      = newOpCode(135)
opBitShiftRightZeroedCode               = newOpCode(136)

opCollShiftRightCode                    = newOpCode(137)
opCollShiftLeftCode                     = newOpCode(138)
opCollShiftRightZeroedCode              = newOpCode(139)

opCollRotateLeftCode                    = newOpCode(140)
opCollRotateRightCode                   = newOpCode(141)

opContextCode                           = newOpCode(142)
opXorOfCode                             = newOpCode(143) -- equals to 255
