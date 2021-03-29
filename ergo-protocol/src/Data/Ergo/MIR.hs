{-# LANGUAGE EmptyDataDeriving #-}
-- |
-- Middle intermediate representation (MIR) which is used in the
-- interpreter and serialization.
module Data.Ergo.MIR where

import Data.Int
import Data.Word
import Data.Persist
import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)
import Data.Ergo.Vlq
import Data.Ergo.MIR.Parser

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

data SMethod -- FIXME
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

-- | Tuple items with bounds check (2..=255)
newtype TupleItems a = TupleItems [a]
  deriving Show

-- | Function argument
data FuncArg = FuncArg !ValId !SType
  deriving Show

data FuncValue = MkFuncValue
  { funcValArgs :: [FuncArg]
  , funcValBody :: Expr
  , funcValType :: SType
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
  deriving Show

data GlobalVars
  = Inputs  -- ^ Tx inputs
  | Outputs -- ^ Tx outputs
  | Height  -- ^ Current blockchain height
  | SelfBox -- ^ ErgoBox instance, which script is being evaluated
  | MinerPubKey
  deriving Show

data MethodCall = MkMethodCall
  { methodObj    :: Expr
  , methodMethod :: SMethod
  , methodArgs   :: [Expr]
  }
  deriving Show
data BlockValue = MksBlockValue
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
  , valUseTy :: SType
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

data ECPoint             -- FIXME
  deriving Show
data SigmaProp           -- FIXME
  deriving Show

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

data BinOp
  = ArithOp ArithOp
  | RelationOp RelationOp
  deriving Show
