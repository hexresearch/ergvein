-- |
-- Middle intermediate representation (MIR) which is used in the
-- interpreter and serialization.
module Data.Ergo.MIR where

import Data.Int
import Data.Word
import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)

newtype STypeVar = STypeVar Text
data STypeParam = STypeParam
  { tyParamIdent      :: STypeVar
  , tyParamUpperBound :: Maybe SType
  , tyParamLowerBound :: Maybe SType
  }

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


data SFunc = MkSFunc
  { sfuncDom     :: [SType]
  , sfuncCodom   :: SType
  , sfuncTyParam :: [STypeParam]
  }

data SMethod -- FIXME


-- | Box ID
newtype IrBoxId = IrBoxId ShortByteString

-- | Element in the tuple
newtype TupleIdx = TupleIdx Word8

-- | Index in constants array
newtype ConstIdx = ConstIdx Word32

-- | Variable ID
newtype ValId = ValId Word32

-- | Tuple items with bounds check (2..=255)
newtype TupleItems a = TupleItems [a]

-- | Function argument
data FuncArg = FuncArg !ValId !SType

data FuncValue = MkFuncValue
  { funcValArgs :: [FuncArg]
  , funcValBody :: Expr
  , funcValType :: SType
  }

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



data Expr
  = Const            SType Value     -- ^ Constant value
  | ConstPlaceholder SType !ConstIdx -- ^ Placeholder for a constant
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
  | BinOp !Expr !BinOp !Expr    -- ^ Binary operation
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


data GlobalVars
  = Inputs  -- ^ Tx inputs
  | Outputs -- ^ Tx outputs
  | Height  -- ^ Current blockchain height
  | SelfBox -- ^ ErgoBox instance, which script is being evaluated


data MethodCall = MkMethodCall
  { methodObj    :: Expr
  , methodMethod :: SMethod
  , methodArgs   :: [Expr]
  }

data BlockValue = MksBlockValue
  { blkValStmts :: [Expr]
  , blkExpr     :: Expr
  }

-- | IR node for let-bound expressions `let x = rhs` which is ValDef.
-- These nodes are used to represent ErgoTrees after common sub-expression elimination.
-- This representation is more compact in serialized form.
-- @param id unique identifier of the variable in the current scope. */
data ValDef = MkValDef
  { valDefId  :: ValId
  , valDefRhs :: Expr
  }

-- | Special node which represents a reference to ValDef in was
--   introduced as result of CSE.
data ValUse = MkValUse
  { valUseId :: ValId
  , valUseTy :: SType
  }

data ExtractRegisterAs = MkExtractRegisterAs
  { extractRegBox  :: Expr -- ^ Box to extract from
  , extractRegId   :: Int8 -- ^ Register ID
  , extractRegType :: SType -- ^ Type of register content
  }
data ByIndex = MkByIndex
  { byIdxColl    :: Expr       -- ^ Collection to index
  , byIdxIndex   :: Expr       -- ^ Index
  , byIdxDefault :: Maybe Expr -- ^ Default value if out of range
  }
data Fold = MkFold
  { foldInput :: Expr
  , foldBase  :: Expr
  , foldStep  :: Expr
  }
data Map = MkMap
  { mapInput  :: Expr
  , mapMapper :: Expr
  , mapType   :: SFunc
  }
data Filter = MkFilter
  { filterInput :: Expr
  , filterCond  :: Expr
  , filterType  :: SType
  }

data ECPoint             -- FIXME
data SigmaProp           -- FIXME


data ArithOp 
  = OpPlus     -- ^ Addition
  | OpMinus    -- ^ Subtraction
  | OpMultiply -- ^ Multiplication
  | OpDivide   -- ^ Division
  | OpMax      -- ^ Max of two values
  | OpMin      -- ^ Min of two values
   
data RelationOp 
  = OpEq  -- ^ Equality
  | OpNEq -- ^ Non-equality
  | OpGE  -- ^ Greater of equal
  | OpGT  -- ^ Greater than..
  | OpLE  -- ^ Less or equal
  | OpLT  -- ^ Less then
  | OpAnd -- ^ Logical AND
  | OpOr  -- ^ Logical OR

data BinOp
  = ArithOp ArithOp
  | RelationOp RelationOp
