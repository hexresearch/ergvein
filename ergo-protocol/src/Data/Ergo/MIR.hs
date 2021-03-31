{-# LANGUAGE EmptyDataDeriving #-}
-- |
-- Middle intermediate representation (MIR) which is used in the
-- interpreter and serialization.
module Data.Ergo.MIR where

import Debug.Trace
import Data.ByteString (ByteString)
import Data.Int
import Data.Word
import Data.Persist
import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)
import Data.Ergo.Vlq
import Data.Ergo.Modifier
import Data.Ergo.MIR.Parser
import Data.Ergo.MIR.OpCode
import Data.Ergo.MIR.Constants

newtype STypeVar = STypeVar Text
  deriving Show

data STypeParam = STypeParam
  { tyParamIdent      :: STypeVar
  , tyParamUpperBound :: Maybe SType
  , tyParamLowerBound :: Maybe SType
  }
  deriving Show

data SType
  = TyVar STypeVar -- ^ Type variable (generic)
  | SAny           -- ^ TBD
  | SBoolean       -- ^ Boolean
  | SByte          -- ^ Signed byte
  | SShort         -- ^ Signed short (16-bit)
  | SInt           -- ^ Signed int (32-bit)
  | SLong          -- ^ Signed long (64-bit)
  | SBigInt        -- ^ 256-bit integer
  | SGroupElement  -- ^ Discrete logarithm prime-order group element [`EcPoint`]
  | SSigmaProp     -- ^ Proposition which can be proven and verified by sigma protocol.
  | SBox           -- ^ ErgoBox value
  | SAvlTree       -- ^ AVL tree value
  | SOption SType  -- ^ Optional value
  | SColl   SType    -- ^ Collection of elements of the same type
  | STuple (TupleItems SType) -- ^ Tuple (elements can have different types)
  | SFunc  SFunc   -- ^ Function (signature)
  | SContext       -- ^ Context object ("CONTEXT" in ErgoScript)
  deriving Show


data SFunc = MkSFunc
  { sfuncDom     :: [SType]
  , sfuncCodom   :: SType
  , sfuncTyParam :: [STypeParam]
  }
  deriving (Show)

-- FIXME: this is basically as parse structure of 
data SMethod = SMethod Word8 Word8 
  deriving Show

-- | Box ID
newtype IrBoxId = IrBoxId ShortByteString
  deriving Show

-- | Element in the tuple
newtype TupleIdx = TupleIdx Word8
  deriving stock Show
  deriving newtype Persist

-- | Index in constants array
newtype ConstIdx = ConstIdx Word32
  deriving Show
  deriving ErgoParser via AsVarInt Word32


-- | Variable ID
newtype ValId = ValId Word32
  deriving Show
  deriving ErgoParser via AsVarInt Word32

-- | Tuple items with bounds check (2..=255)
newtype TupleItems a = TupleItems [a]
  deriving Show

-- | Function argument
data FuncArg = FuncArg !ValId !SType
  deriving Show

 
data FuncValue = MkFuncValue
  { funcValArgs :: [FuncArg]
  , funcValBody :: Expr
-- FIXME: 
--  , funcValType :: SType
  }
  deriving Show

data Value
  = Boolean      !Bool               -- ^ Boolean
  | Byte         !Int8               -- ^ Byte
  | Short        !Int16              -- ^ Short
  | Int          !Int32              -- ^ Int
  | Long         !Int64              -- ^ Long
  | BigInt  {- FIXME -}              -- ^ Big integer
  | GroupElement !ECPoint            -- ^ GroupElement
  | SigmaProp    !SigmaProp          -- ^ Sigma property
  | CBox         !IrBoxId            -- ^ Box
  | AvlTree {-FIXME-}                -- ^ AVL tree
  | Coll         !SType [Value]      -- ^ Collection of values of the same type
  | Tup          !(TupleItems Value) -- ^ Tuple (arbitrary type values)
  | ValCtx {-FIXME-}                 -- ^ Transaction(and blockchain) context info
  | Opt          !(Maybe Value)      -- ^ Optional value
  | FuncVal      !FuncValue          -- ^ lambda
  deriving Show


data Expr
  = Const            SType Value     -- ^ Constant value
  | ConstPlaceholder {-SType-} !ConstIdx -- ^ Placeholder for a constant
  | SubstConstant
      Expr {- script bytes     -}
      Expr {- positions  [Int] -}
      Expr {- new values -}
  | Collection       SType [Expr]    -- ^ Collection declaration (array of expressions of the same type)
  | CalcBlake2b256   Expr            -- ^ Blake2b256 hash calculation
  | Context {- FIXME -}              -- ^ Context variables (external)
  | GlobalVars       GlobalVars      -- ^ Predefined global variables
  | FuncValue        FuncValue       -- ^ Function definition
  | Apply            !Expr !Expr     -- ^ Function application
  | MethodCall       MethodCall      -- ^ Method call
  | ProperyCall      Expr SMethod    -- ^ Property call
  | BlockValue       BlockValue      -- ^ Block (statements, followed by an expression)
  | ValDef           ValDef          -- ^ let-bound expression
  | ValUse           ValUse          -- ^ Reference to ValDef
  | If    !Expr !Expr !Expr             -- ^ If, non-lazy - evaluate both branches
  | BinOp !BinOp !Expr !Expr    -- ^ Binary operation
  | And        !Expr    -- ^ Logical AND
  | Or         !Expr    -- ^ Logical OR
  | LogicalNot !Expr    -- ^ Logical not
  | OptionGet  !Expr    -- ^ Returns the Option's value or error if no value
  | OptionIsDefined !Expr
  | ForAll Expr Expr
  | Exists Expr Expr
  | ExtractRegisterAs  ExtractRegisterAs -- ^ Extract register's value (box.RX properties)
  | ExtractScriptBytes Expr              -- ^ Extract box's guarding script serialized to bytes
  | ByIndex            ByIndex           -- ^ Collection, get element by index
  | SizeOf             Expr              -- ^ Collection size
  | Fold               Fold              -- ^ Collection fold op
  | Map                Map               -- ^ Collection map op
  | Filter             Filter            -- ^ Collection filter op
  | SelectField        Expr TupleIdx     -- ^ Tuple field access
  | ExtractAmount      Expr              -- ^ Box monetary value
  | BoolToSigmaProp    Expr              -- ^ Bool to Sigma Prop
  | Upcast             Expr SType        -- ^ Upcast numeric value to given type

  | ExtractCreationInfo Expr
  | ProveDLog Expr
  | DecodePoint Expr
  | SigmaAndExpr [Expr]
  | SigmaOrExpr  [Expr]
  
  deriving Show

data GlobalVars
  = Inputs  -- ^ Tx inputs
  | Outputs -- ^ Tx outputs
  | Height  -- ^ Current blockchain height
  | SelfBox -- ^ ErgoBox instance, which script is being evaluated
  | MinerPubKey
  | Global
  deriving Show

data MethodCall = MkMethodCall
  { methodObj    :: Expr
  , methodMethod :: SMethod
  , methodArgs   :: [Expr]
  }
  deriving Show
data BlockValue = MkBlockValue
  { blkValStmts :: [Expr]
  , blkExpr     :: Expr
  }
  deriving Show
-- | IR node for let-bound expressions `let x = rhs` which is ValDef.
-- These nodes are used to represent ErgoTrees after common sub-expression elimination.
-- This representation is more compact in serialized form.
-- @param id unique identifier of the variable in the current scope. */
data ValDef = MkValDef
  { valDefId  :: ValId
  , valDefRhs :: Expr
  }
  deriving Show
-- | Special node which represents a reference to ValDef in was
--   introduced as result of CSE.
data ValUse = MkValUse
  { valUseId :: ValId
{-  , valUseTy :: SType -} -- Obtained from type store
  }
  deriving Show
data ExtractRegisterAs = MkExtractRegisterAs
  { extractRegBox  :: Expr -- ^ Box to extract from
  , extractRegId   :: Int8 -- ^ Register ID
  , extractRegType :: SType -- ^ Type of register content
  }
  deriving Show
data ByIndex = MkByIndex
  { byIdxColl    :: Expr       -- ^ Collection to index
  , byIdxIndex   :: Expr       -- ^ Index
  , byIdxDefault :: Maybe Expr -- ^ Default value if out of range
  }
  deriving Show
data Fold = MkFold
  { foldInput :: Expr
  , foldBase  :: Expr
  , foldStep  :: Expr
  }
  deriving Show
data Map = MkMap
  { mapInput  :: Expr
  , mapMapper :: Expr
  , mapType   :: SFunc
  }
  deriving Show
data Filter = MkFilter
  { filterInput :: Expr
  , filterCond  :: Expr
  , filterType  :: SType
  }
  deriving Show

newtype ECPoint = ECPoint ByteString
  deriving Show via ModifierId

data SigmaProp
  = SigmaFalse
  | SigmaTrue
  | SigmaDLog    !ECPoint
  | SigmaDHTuple
  | SigmaAND            [SigmaProp]
  | SigmaOR             [SigmaProp]
  | SigmaThreshold !Int [SigmaProp]
  deriving Show

instance ErgoParser ECPoint where
  getErgo = ECPoint <$> getBytes 33

instance ErgoParser SigmaProp where
  getErgo = do
    op <- get
    if | op == opTrivialPropFalseCode -> pure SigmaFalse
       | op == opTrivialPropTrueCode  -> pure SigmaTrue
       | op == opProveDlogCode        -> SigmaDLog <$> getErgo
       | op == opProveDiffieHellmanTupleCode -> error "SigmaProp: parsing of DH tuple is not implemented"
       -- | op == opAndCode     -> SigmaAND <$> parseListOf getErgo
       -- | op == opOrCode      -> SigmaOR  <$> parseListOf getErgo
       | op == opAtLeastCode -> error "SigmaProp: THRESHOLD not implemented"
       | otherwise           -> fail $ "SigmaProp: unknown opcode " ++ show op


data ArithOp 
  = OpPlus     -- ^ Addition
  | OpMinus    -- ^ Subtraction
  | OpMultiply -- ^ Multiplication
  | OpDivide   -- ^ Division
  | OpMax      -- ^ Max of two values
  | OpMin      -- ^ Min of two values
  deriving Show

data RelationOp 
  = OpEq  -- ^ Equality
  | OpNEq -- ^ Non-equality
  | OpGE  -- ^ Greater of equal
  | OpGT  -- ^ Greater than..
  | OpLE  -- ^ Less or equal
  | OpLT  -- ^ Less then
  | OpAnd -- ^ Logical AND
  | OpOr  -- ^ Logical OR
  deriving Show

data BitOp 
  = OpBinOr
  | OpBinAnd
  | OpBinXor
  deriving Show

data BinOp
  = ArithOp    ArithOp
  | RelationOp RelationOp
  | BitOp      BitOp
  deriving Show


----------------------------------------------------------------
-- Parsing
----------------------------------------------------------------

instance ErgoParser FuncArg where
  getErgo = FuncArg <$> getErgo <*> getErgo

instance ErgoParser SType where
  getErgo = getSType

getSType :: Get SType
getSType = do
  c <- get @Word8
  traceShowM ("getSType",c)
  if  | c <= 0 -> fail "Invalid type prefix"
        -- Not a tuple we need to drill down
      | c < c_TupleTypeCode
      , (conId,primId) <- c `divMod` c_PrimRange -> case conId of
          -- Primitive
          0 -> getEmbeddableType primId
          --
          1 -> SColl           <$> getArgType primId
          2 -> SColl   . SColl <$> getArgType primId
          3 -> SOption         <$> getArgType primId
          4 -> SOption . SColl <$> getArgType primId
          -- 5 -> case primId of
          --   0 -> STuple . TupleItems <$> sequence [getSType, getSType]
          --   -- Pair of types where first is primitive (`(_, Int)`)
          --   _ -> do t1 <- getEmbeddableType primId
          --           t2 <- getSType
          --           pure $ STuple $ TupleItems [t1,t2]
          6 -> case primId of
            0 -> STuple . TupleItems <$> sequence [getSType, getSType, getSType]
            -- Pair of types where second is primitive (`(Int, _)`)
            _ -> do t2 <- getEmbeddableType primId
                    t1 <- getSType
                    traceShowM [t1,t2]
                    pure $ STuple $ TupleItems [t1,t2]
          _ -> error $ "Unhandled conId " ++ show conId ++ " primId " ++ show primId
        -- Tuple
      | c == c_TupleTypeCode      -> error "Cant handle TupleTypeCode"
      | c == c_SAnyTypeCode       -> pure SAny
      | c == c_SUnitTypeCode      -> error "Can't handle SUnit"
      | c == c_SBoxTypeCode       -> pure SBox
      | c == c_SAvlTreeTypeCode   -> pure SAvlTree
      | c == c_SContextTypeCode   -> pure SContext
      | c == c_SStringTypeCode    -> error "Can't handle SString"
      | c == c_STypeVarTypeCode   -> error "Can't handle STypeVarHeader"
      | c == c_SHeaderTypeCode    -> error "Can't handle SHeader"
      | c == c_SPreHeaderTypeCode -> error "Can't handle SPreHeader"
      | c == c_SGlobalTypeCode    -> error "Can't handle SGlobal"
      | otherwise -> fail $ "Unknown type code " ++ show c

getArgType :: Word8 -> Get SType
getArgType = \case
  0      -> getSType
  primId -> getEmbeddableType primId

getEmbeddableType :: Word8 -> Get SType
getEmbeddableType = \case
  0 -> error "NULL?"
  1 -> pure SBoolean
  2 -> pure SByte
  3 -> pure SShort
  4 -> pure SInt
  5 -> pure SLong
  6 -> pure SBigInt
  7 -> pure SGroupElement
  8 -> pure SSigmaProp
  _ -> fail "Invalid primitive type"
