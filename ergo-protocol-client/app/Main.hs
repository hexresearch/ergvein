{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
module Main where

import Debug.Trace
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Bits
import Data.Ergo.Modifier
import Data.Ergo.Protocol
import Data.Ergo.Vlq
import Data.Ergo.MIR
import Data.Ergo.MIR.OpCode
import Data.Ergo.MIR.Parser
import Data.Ergo.MIR.Constants
import Data.Ergo.Protocol.Client
import Data.Ergo.Block (BlockHeader)
import Data.Maybe
import Data.Persist
import Data.Persist.Internal (Get(..),(:!:)(..))
import Data.Time
import Data.Word
import Data.Int
import Options.Generic
import Data.Vector.Generic ((!))
import qualified Data.Vector as V
import Text.Groom

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteArray  as BA
import qualified Foreign.Storable as Foreign
import Crypto.Hash

data Options = Options {
  nodeAddress  :: Maybe String <?> "Address of node"
, nodePort     :: Maybe Int <?> "Port of node"
, testnet      :: Bool <?> "Is this testnet network"
} deriving (Generic)

instance ParseRecord Options

getNodeAddress :: Options -> String
getNodeAddress Options{..} = fromMaybe "127.0.0.1" $ unHelpful nodeAddress

getNodePort :: Options -> Int
getNodePort Options{..} = fromMaybe (if unHelpful testnet then 19030 else 9030) $ unHelpful nodePort

main :: IO ()
main = do
  opts@Options{..} <- getRecord "Ergo protocol client example"
  let net = if unHelpful testnet then Testnet else Mainnet
  inChan <- newTChanIO
  let conf = SocketConf {
          _socketConfPeer = Peer (getNodeAddress opts) (show $ getNodePort opts)
        , _socketConfSocks = Nothing
        , _socketConfReopen = Just (3.0, 5)
        }
  outChan <- ergoSocket net inChan conf
  forever $ do
    ev <- atomically $ readTChan outChan
    case ev of
      SockOutInbound (MsgHandshake h) -> do
        t <- getCurrentTime
        atomically $ writeTChan inChan $ SockInSendEvent $ MsgHandshake $ makeHandshake 0 t
        threadDelay 1000000
        let requiredBlock = "8cf6dca6b9505243e36192fa107735024c0000cf4594b1daa2dc4e13ee86f26f" -- H=414474
            -- requiredBlock = "2cc7c4f1f609694b83df093192e5bf3f4ad441a3c8f1959a28d51eb16fa94b19"
            --
            merkleR c = ModifierId
                      $ BA.convert
                      $ hash @_ @Blake2b_256
                      $ BS.singleton c
                     <> unModifierId requiredBlock
                     <> unModifierId "722f9306300d0d96fe8c10de830216d700131614f9e6ce2496e8dba1cbb45951"
            ty = ModifierBlockTxs
        print requiredBlock
        print $ merkleR 102
        putStrLn "----"
        atomically $ writeTChan inChan $ SockInSendEvent $ MsgOther $ MsgRequestModifier $ RequestModifierMsg ty
          -- [requiredBlock]
          $ V.fromList [merkleR c | c <- [102]]
        -- atomically $ writeTChan inChan $ SockInSendEvent $ MsgOther $ MsgSyncInfo $ SyncInfo [
          -- "efa4abde000dca13fd08220d48f70c3e64b49d91102af9d047fd6f35826352e9"
          -- ]
        pure ()
      SockOutInbound (MsgOther (MsgInv (InvMsg itype is))) -> do
        atomically $ writeTChan inChan $ SockInSendEvent $ MsgOther $ MsgRequestModifier $ RequestModifierMsg itype $ V.singleton $ V.head is
        pure ()
      _ -> pure ()
    case ev of
      SockOutInbound (MsgHandshake h) -> putStrLn "HANDSHAKE" >> print h
      SockOutInbound (MsgOther msg) -> case msg of
        MsgInv      _ -> putStrLn "MsgInv"
        MsgSyncInfo (SyncInfo bids) -> do
          putStrLn "MsgSyncInfo"
          -- mapM_ print bids
        MsgRequestModifier _ -> putStrLn "MsgRequestModifier"
        MsgModifier (ModifierMsg ty m) -> do
          putStrLn "MsgModifier"
          print ty
          let bs = modifierBody $ m ! 0
          either print putStrLn $ flip runGet bs $ do
            modId <- ModifierId <$> getBytes 32
            n     <- decodeVarInt @Word32
            tx <- parseTX
            return (tx)
          -- print $ ModifierId $ BS.drop 32 $ bs
          -- print $ decode @BlockHeader bs
          error "ABORT"
          -- putStrLn "================"
      _ -> print ev


parseTX = do
  -- Inputs
  ins      <- parseListOf @Word16 $ getErgo @ErgoInput
  dataIns  <- parseListOf @Word16 $ getErgo @ErgoDataInput
  tokenIds <- parseShortListOf $ getErgo @TokenId
  -- -- -- Outputs
  nOut <- decodeVarInt @Word16
  ----- Start decoding ouput --
  --
  -- Parse list of constants
  box1 <- getErgo @ErgoBox
  box2 <- getErgo @ErgoBox
  -- t  <- getSType
  -- l  <- decodeVarInt @Int32
    -- tree           <- undefined
  --   creationHeight <- decodeVarInt @Word32
  --   -- Tokens
  --   tokensData     <- parseShortListOf $ do
  --     (,) <$> (ModifierId <$> getBytes 32) <*> decodeVarInt @Word64
  --   -- Registers
  --   nRegs <- get @Word8
  --   -- regs  <- replicateM (fromIntegral nRegs) $ do
  --   --   undefined
  --   pure (value,tree,creationHeight,tokensData,nRegs)
  -- bbs   <- ModifierId <$> getBytes 16
  -- xx <- decodeVarInt @Word16
  -- remAfter <- remaining
  -- tx       <- ModifierId <$> getBytes (228-(remBefore - remAfter))
  pure $ unlines
    [ "----------------"
    , show ins
    , show dataIns
    , show nOut
    , groom box2
    -- , show l
    -- , show opcode
    -- , show xx
    -- , show tx
    ]
   -- (ins,dataIns,tokens,v1,v2,v3)
  -- nOut <- decodeVarInt @Word16
  -- outs <- replicateM tokensCount undefined
  -- undefined

----------------------------------------------------------------
-- Transactions
----------------------------------------------------------------

data ErgoInput = ErgoInput ModifierId ByteString
  deriving Show

newtype ErgoDataInput = ErgoDataInput ModifierId
  deriving stock   Show
  deriving newtype ErgoParser

newtype TokenId = TokenId ModifierId
  deriving stock   Show
  deriving newtype ErgoParser

data ErgoBox = ErgoBox
  { eboxValue       :: !Word64
  , eboxConstants   :: [(SType,Value)]
  , eboxSpendScript :: !Expr
  , eboxHeight      :: !Word32
  , eboxTokens      :: [(ModifierId, Word32)]
  }
  deriving stock (Show)

data ErgoTx = ErgoTx
  { txInputs  :: [ErgoInput]
  , txDataIn  :: [ErgoDataInput]
  , txTokens  :: [TokenId]
  , txOutputs :: [ErgoBox]
  }
  deriving stock (Show)

instance ErgoParser ErgoInput where
  getErgo = do
    bid   <- ModifierId <$> getBytes 32
    n     <- decodeVarInt @Word16
    proof <- getBytes (fromIntegral n)
    -- We don't parse context extension here
    get @Word8 >>= \case
      0 -> pure ()
      _ -> error "FIXME: ErgoInput: we don't parse ContextExtension"
    pure $ ErgoInput bid proof

instance ErgoParser ErgoBox where
  getErgo = do
    eboxValue     <- decodeVarInt @Word64
    eth           <- decodeErgoTreeHeader
    eboxConstants <- case ergoConstantSegreation eth of
      False -> pure []
      True  -> parseListOf @Word32 parseConstant
    -- Parse transaction body
    eboxSpendScript <- parseValue
    eboxHeight      <- decodeVarInt @Word32
    eboxTokens      <- parseShortListOf $ (,) <$> getErgo <*> decodeVarInt @Word32
    --
    get @Word8 >>= \case
      0 -> pure ()
      _ -> error "FIXME: ErgoBox: registers aren't parsed"
    pure ErgoBox{..}

instance ErgoParser ErgoTx where
  getErgo = do
    txInputs  <- parseListOf @Word16 $ getErgo @ErgoInput
    txDataIn  <- parseListOf @Word16 $ getErgo @ErgoDataInput
    txTokens  <- parseShortListOf $ getErgo @TokenId
    txOutputs <- parseShortListOf getErgo
    pure ErgoTx{..}

instance ErgoParser ModifierId where
  getErgo = ModifierId <$> getBytes 32

parseConstant = do ty <- getSType
                   c  <- getConstant ty
                   pure (ty,c)

parseValue :: Get Expr
parseValue = do
  peek >>= \case
    b | b < c_LastConstantCode -> uncurry Const <$> parseConstant
      | otherwise              -> parseAST



getSType = do
  get @Word8 >>= \case
    c | c <= 0 -> fail "Invalid type prefix"
        -- Not a tuple we need to drill down
      | c < c_TupleTypeCode
      , (conId,primId) <- c `divMod` c_PrimRange -> case conId of
          -- Primitive
          0 -> getEmbeddableType primId
          --
          1 -> SColl <$> getArgType primId
          _ -> fail "AA"
        -- Tuple
      | otherwise -> fail "TupleTypeCode"

getArgType = \case
  0      -> getSType
  primId -> getEmbeddableType primId

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

getConstant = \case
  SBoolean -> Boolean . (/=0) <$> get @Word8
  SByte    -> Byte  <$> get @Int8
  SShort   -> Short <$> decodeVarInt
  SInt     -> Int   <$> decodeVarInt
  SLong    -> Long  <$> decodeVarInt
  SColl ty -> deserializeColl ty
  s -> error ("getConstant: " ++ show s)

deserializeColl ty = do
  len <- fromIntegral <$> decodeVarInt @Word16
  traceShowM ty
  Coll ty <$> case ty of
    SBoolean -> error "SColl SBoolean"
    SByte    -> replicateM len (Byte <$> get @Int8)
    _        -> replicateM len (getConstant ty)

-- tyCode_SAny = 97 :: Word8

data ErgoTreeHeader = ErgoTreeHeader
  { ergoTreeVersion        :: !Word8
  , ergoTreeSize           :: Maybe Word32
  , ergoConstantSegreation :: Bool
  }
  deriving Show

decodeErgoTreeHeader :: Get ErgoTreeHeader
decodeErgoTreeHeader = do
  w <- get @Word8
  let ergoTreeVersion  = w .&. 0x07
      sizeFlag         = (w .&. 0x08) /= 0
  when (ergoTreeVersion > 0 && not sizeFlag) $
    fail "Size flag must be set for ErgoTree version > 0"
  ergoTreeSize <- case sizeFlag of
    True  -> Just <$> decodeVarInt
    False -> pure Nothing
  pure ErgoTreeHeader{ ergoConstantSegreation = (w .&. 0x10) /= 0
                     , ..
                     }

parseListOf :: forall n a. (Integral n, VarInt n) => Get a -> Get [a]
parseListOf getV = do
  n <- decodeVarInt @n
  replicateM (fromIntegral n) getV

parseShortListOf :: Get a -> Get [a]
parseShortListOf getV = do
  n <- get @Word8
  replicateM (fromIntegral n) getV

----------------------------------------------------------------
-- AST
----------------------------------------------------------------

parseAST :: Get Expr 
parseAST = do
  op <- get
  if | op == opTaggedVariableCode                    -> error "OPCODE: opTaggedVariableCode"
     | op == opValUseCode                            -> error "OPCODE: opValUseCode"
     -- FIXME: We need access to constant store here
     | op == opConstantPlaceholderCode               -> ConstPlaceholder <$> getErgo
     | op == opSubstConstantsCode                    -> SubstConstant <$> parseValue <*> parseValue <*> parseValue
     | op == opLongToByteArrayCode                   -> error "OPCODE: opLongToByteArrayCode"
     | op == opByteArrayToBigIntCode                 -> error "OPCODE: opByteArrayToBigIntCode"
     | op == opByteArrayToLongCode                   -> error "OPCODE: opByteArrayToLongCode"
     | op == opDowncastCode                          -> error "OPCODE: opDowncastCode"
     | op == opUpcastCode                            -> Upcast <$> parseValue <*> getSType
     | op == opTrueCode                              -> error "OPCODE: opTrueCode"
     | op == opFalseCode                             -> error "OPCODE: opFalseCode"
     | op == opUnitConstantCode                      -> error "OPCODE: opUnitConstantCode"
     | op == opGroupGeneratorCode                    -> error "OPCODE: opGroupGeneratorCode"
     | op == opConcreteCollectionCode                -> do
         size <- decodeVarInt @Word16
         ty   <- getSType
         xs   <- replicateM (fromIntegral size) parseValue
         pure $ Collection ty xs
     | op == opConcreteCollectionBooleanConstantCode -> error "OPCODE: opConcreteCollectionBooleanConstantCode"
     | op == opTupleCode                             -> error "OPCODE: opTupleCode"
     | op == opSelect1Code                           -> error "OPCODE: opSelect1Code"
     | op == opSelect2Code                           -> error "OPCODE: opSelect2Code"
     | op == opSelect3Code                           -> error "OPCODE: opSelect3Code"
     | op == opSelect4Code                           -> error "OPCODE: opSelect4Code"
     | op == opSelect5Code                           -> error "OPCODE: opSelect5Code"
     | op == opSelectFieldCode                       -> SelectField <$> parseValue <*> get
     | op == opLtCode                                -> relationOp OpLT
     | op == opLeCode                                -> relationOp OpLE
     | op == opGtCode                                -> relationOp OpGT
     | op == opGeCode                                -> relationOp OpGE
     | op == opEqCode                                -> relationOp OpEq
     | op == opNeqCode                               -> relationOp OpNEq
     | op == opIfCode                                -> If <$> parseValue <*> parseValue <*> parseValue
     | op == opAndCode                               -> And <$> parseValue
     | op == opOrCode                                -> Or  <$> parseValue
     | op == opAtLeastCode                           -> error "OPCODE: opAtLeastCode"
     | op == opMinusCode                             -> arithOp OpMinus
     | op == opPlusCode                              -> arithOp OpPlus
     | op == opXorCode                               -> error "OPCODE: opXorCode"
     | op == opMultiplyCode                          -> arithOp OpMultiply
     | op == opDivisionCode                          -> arithOp OpDivide
     | op == opModuloCode                            -> error "OPCODE: opModuloCode"
     | op == opExponentiateCode                      -> error "OPCODE: opExponentiateCode"
     | op == opMultiplyGroupCode                     -> error "OPCODE: opMultiplyGroupCode"
     | op == opMinCode                               -> error "OPCODE: opMinCode"
     | op == opMaxCode                               -> error "OPCODE: opMaxCode"
     | op == opHeightCode                            -> pure $ GlobalVars Height
     | op == opInputsCode                            -> error "OPCODE: opInputsCode"
     | op == opOutputsCode                           -> pure $ GlobalVars Outputs
     | op == opLastBlockUtxoRootHashCode             -> error "OPCODE: opLastBlockUtxoRootHashCode"
     | op == opSelfCode                              -> pure $ GlobalVars SelfBox
     | op == opMinerPubkeyCode                       -> pure $ GlobalVars MinerPubKey
     | op == opMapCollectionCode                     -> error "OPCODE: opMapCollectionCode"
     | op == opExistsCode                            -> error "OPCODE: opExistsCode"
     | op == opForAllCode                            -> error "OPCODE: opForAllCode"
     | op == opFoldCode                              -> error "OPCODE: opFoldCode"
     | op == opSizeOfCode                            -> SizeOf <$> parseValue
     | op == opByIndexCode                           ->
         ByIndex <$> (MkByIndex <$> parseValue <*> parseValue <*> getOptional parseValue)
     | op == opAppendCode                            -> error "OPCODE: opAppendCode"
     | op == opSliceCode                             -> error "OPCODE: opSliceCode"
     | op == opFilterCode                            -> error "OPCODE: opFilterCode"
     | op == opAvlTreeCode                           -> error "OPCODE: opAvlTreeCode"
     | op == opAvlTreeGetCode                        -> error "OPCODE: opAvlTreeGetCode"
     | op == opFlatMapCollectionCode                 -> error "OPCODE: opFlatMapCollectionCode"
     | op == opExtractAmountCode                     -> ExtractAmount      <$> parseValue
     | op == opExtractScriptBytesCode                -> ExtractScriptBytes <$> parseValue
     | op == opExtractBytesCode                      -> error "OPCODE: opExtractBytesCode"
     | op == opExtractBytesWithNoRefCode             -> error "OPCODE: opExtractBytesWithNoRefCode"
     | op == opExtractIdCode                         -> error "OPCODE: opExtractIdCode"
     | op == opExtractRegisterAs                     -> error "OPCODE: opExtractRegisterAs"
     | op == opExtractCreationInfoCode               -> ExtractCreationInfo <$> parseValue
     | op == opCalcBlake2b256Code                    -> error "OPCODE: opCalcBlake2b256Code"
     | op == opCalcSha256Code                        -> error "OPCODE: opCalcSha256Code"
     | op == opProveDlogCode                         -> ProveDLog <$> parseValue
     | op == opProveDiffieHellmanTupleCode           -> error "OPCODE: opProveDiffieHellmanTupleCode"
     | op == opSigmaPropIsProvenCode                 -> error "OPCODE: opSigmaPropIsProvenCode"
     | op == opSigmaPropBytesCode                    -> error "OPCODE: opSigmaPropBytesCode"
     | op == opBoolToSigmaPropCode                   -> BoolToSigmaProp <$> parseValue
     | op == opTrivialPropFalseCode                  -> error "OPCODE: opTrivialPropFalseCode"
     | op == opTrivialPropTrueCode                   -> error "OPCODE: opTrivialPropTrueCode"
     | op == opDeserializeContextCode                -> error "OPCODE: opDeserializeContextCode"
     | op == opDeserializeRegisterCode               -> error "OPCODE: opDeserializeRegisterCode"
     | op == opValDefCode                            -> error "OPCODE: opValDefCode"
     | op == opFunDefCode                            -> error "OPCODE: opFunDefCode"
     | op == opBlockValueCode                        -> error "OPCODE: opBlockValueCode"
     | op == opFuncValueCode                         -> error "OPCODE: opFuncValueCode"
     | op == opFuncApplyCode                         -> error "OPCODE: opFuncApplyCode"
     | op == opPropertyCallCode                      -> error "OPCODE: opPropertyCallCode"
     | op == opMethodCallCode                        -> error "OPCODE: opMethodCallCode"
     | op == opGlobalCode                            -> error "OPCODE: opGlobalCode"
     | op == opSomeValueCode                         -> error "OPCODE: opSomeValueCode"
     | op == opNoneValueCode                         -> error "OPCODE: opNoneValueCode"
     | op == opGetVarCode                            -> error "OPCODE: opGetVarCode"
     | op == opOptionGetCode                         -> error "OPCODE: opOptionGetCode"
     | op == opOptionGetOrElseCode                   -> error "OPCODE: opOptionGetOrElseCode"
     | op == opOptionIsDefinedCode                   -> error "OPCODE: opOptionIsDefinedCode"
     | op == opModQCode                              -> error "OPCODE: opModQCode"
     | op == opPlusModQCode                          -> error "OPCODE: opPlusModQCode"
     | op == opMinusModQCode                         -> error "OPCODE: opMinusModQCode"
     | op == opSigmaAndCode                          -> error "OPCODE: opSigmaAndCode"
     | op == opSigmaOrCode                           -> error "OPCODE: opSigmaOrCode"
     | op == opBinOrCode                             -> error "OPCODE: opBinOrCode"
     | op == opBinAndCode                            -> error "OPCODE: opBinAndCode"
     | op == opDecodePointCode                       -> DecodePoint <$> parseValue
     | op == opLogicalNotCode                        -> error "OPCODE: opLogicalNotCode"
     | op == opNegationCode                          -> error "OPCODE: opNegationCode"
     | op == opBitInversionCode                      -> error "OPCODE: opBitInversionCode"
     | op == opBitOrCode                             -> error "OPCODE: opBitOrCode"
     | op == opBitAndCode                            -> error "OPCODE: opBitAndCode"
     | op == opBinXorCode                            -> error "OPCODE: opBinXorCode"
     | op == opBitXorCode                            -> error "OPCODE: opBitXorCode"
     | op == opBitShiftRightCode                     -> error "OPCODE: opBitShiftRightCode"
     | op == opBitShiftLeftCode                      -> error "OPCODE: opBitShiftLeftCode"
     | op == opBitShiftRightZeroedCode               -> error "OPCODE: opBitShiftRightZeroedCode"
     | op == opCollShiftRightCode                    -> error "OPCODE: opCollShiftRightCode"
     | op == opCollShiftLeftCode                     -> error "OPCODE: opCollShiftLeftCode"
     | op == opCollShiftRightZeroedCode              -> error "OPCODE: opCollShiftRightZeroedCode"
     | op == opCollRotateLeftCode                    -> error "OPCODE: opCollRotateLeftCode"
     | op == opCollRotateRightCode                   -> error "OPCODE: opCollRotateRightCode"
     | op == opContextCode                           -> error "OPCODE: opContextCode"
     | op == opXorOfCode                             -> error "OPCODE: opXorOfCode"
     | otherwise -> error "Unhandled opcode: op"
  where
    relationOp op = BinOp (RelationOp op) <$> parseValue <*> parseValue
    arithOp    op = BinOp (ArithOp    op) <$> parseValue <*> parseValue

{- HEADER
BlockHeader
  { version          = 1
  , parentId         = 8bdd043dab20aa690afc9a18fc4797de4f02f049f5c16f9657646c753d69582e
  , adProofsRoot     = 4527a2a7bcee7f77b5697f505e5effc5342750f58a52dddfe407a3ce3bd3abd0
  , transactionsRoot = 722f9306300d0d96fe8c10de830216d700131614f9e6ce2496e8dba1cbb45951
  , stateRoot        = 6c06d6277d40aeb958c5631515dc3ec3d11d8504e62de77df024d0ca67242fb512
  , timestamp        = 2021-01-28 22:49:59.636 UTC
  , extensionRoot    = a1a3933312467ce53d41fdc20e38c603e8fd89999371c60d7537c5d5760ef7c4
  , nBits            = Difficulty {unDifficulty = 2572389057560576}
  , height           = 414474
  , votes            = ParamVotes 4 3 0
  , powSolution = AutolykosSolution
    { minerPubKey = 02bb8eb301ab3d5d14515e33760d0dfb4f7191312a640db64a3a1aeeac9703f2d3
    , oneTimePubKey = 026d7b267c33120d15c267664081a6b77a6dcae6b35147db2c3e1195573119cb14
    , nonce = "\NUL\b\161\209\ETX\136\SOH\ETB"
    , distance = 35863003992655055679291741607273543535646500642591973829915050
    }
  }
-}


peek :: Get Word8
peek = do
  ensure 1
  Get $ \_ ptr -> do w <- Foreign.peek ptr
                     pure (ptr :!: w)

getOptional :: Get a -> Get (Maybe a)
getOptional getVal = get @Word8 >>= \case
  0 -> pure Nothing
  _ -> Just <$> getVal
