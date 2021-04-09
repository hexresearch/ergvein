{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NumDecimals           #-}
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
import Data.Ergo.Block (BlockHeader(..),Digest32(..))
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
import Text.Printf

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

data WaitingFor
  = OnlyStarted
  | AskedHeader !ModifierId
  | AskedBlock  !BlockHeader !ModifierId

mainLoop outChan inChan = go 
  where
    handleMsg expecting msg f = case msg of
      MsgInv             _ -> putStrLn "MsgInv" >> go expecting      
      MsgSyncInfo        _ -> putStrLn "MsgSyncInfo" >> go expecting
      MsgRequestModifier _ -> putStrLn "MsgRequestModifier" >> go expecting
      MsgModifier (ModifierMsg _ [m]) -> f m
      MsgModifier _ -> putStrLn "MsgModifier (other)" >> go expecting
    --
    go expecting = case expecting of
      OnlyStarted -> do
        let requiredBlock = "eac5edd4b7d602acae0842b03af3724b76a72bbc608a4bbea2fcf2391f4dacaa" -- H=414477
        send requiredBlock
        go $ AskedHeader requiredBlock
      --
      AskedHeader modId -> atomically (readTChan outChan) >>= \case
        SockOutInbound (MsgOther msg) -> handleMsg expecting msg $ \m -> do
          let blk = decode @BlockHeader $ modifierBody m
          case blk of
            Right b | bid <- blockId b
                    , bid == modId -> do
                        let mid = ModifierId . BA.convert . hash @_ @Blake2b_256
                                $ BS.singleton 102
                               <> unModifierId bid
                               <> unDigest32 (transactionsRoot b)
                        printf "H=%6i: bid= %s\n" (height b) (show bid)
                        send mid
                        go $ AskedBlock b mid
            _ -> do putStrLn "MsgModifier (unsettling)"
                    go expecting              
      --
      AskedBlock blk modId -> atomically (readTChan outChan) >>= \case
        SockOutInbound (MsgOther msg) -> handleMsg expecting msg $ \m -> case () of
          _| ModifierId (BS.take 32 bs) == blockId blk -> do
               -- BS.writeFile (printf "../BLK/H_%06i" (height blk)) $ BS.drop 32 bs
               let bid = parentId blk
               send bid
               printf "Sending bid = %s\n" (show bid)
               go $ AskedHeader bid
           | otherwise -> do
               putStrLn "MsgModifier (worrying)"
               go $ AskedBlock blk modId 
           where
             bs = modifierBody m
    --
    send modId = atomically $ writeTChan inChan
               $ SockInSendEvent $ MsgOther $ MsgRequestModifier $ RequestModifierMsg ModifierBlockTxs [modId]


blockId :: BlockHeader -> ModifierId
blockId = ModifierId . BA.convert . hash @_ @Blake2b_256 . encode

handshakeLoop outChan inChan = go
  where
    go = atomically (readTChan outChan) >>= \case
      SockOutInbound (MsgHandshake h) -> do
        putStrLn "HANDSHAKE"
        t <- getCurrentTime
        atomically $ writeTChan inChan $ SockInSendEvent $ MsgHandshake $ makeHandshake 0 t
        threadDelay 0.5e6
      _ -> go

  

main :: IO ()
main = do
  opts@Options{..} <- getRecord "Ergo protocol client example"
  let net = if unHelpful testnet then Testnet else Mainnet
  inChan <- newTChanIO
  let conf = SocketConf
        { _socketConfPeer   = Peer (getNodeAddress opts) (show $ getNodePort opts)
        , _socketConfSocks  = Nothing
        , _socketConfReopen = Just (3.0, 5)
        }
  outChan <- ergoSocket net inChan conf
  --
  handshakeLoop outChan inChan
  mainLoop outChan inChan OnlyStarted
  
  -- let loop mod = do
  --        >>= \case
  --         SockOutInbound (MsgHandshake h)
  --       case ev of
        
  -- forever $ do
  --   ev <- atomically $ readTChan outChan
  --   case ev of
  --     SockOutInbound (MsgHandshake h) -> do
  --       t <- getCurrentTime
  --       atomically $ writeTChan inChan $ SockInSendEvent $ MsgHandshake $ makeHandshake 0 t
  --       threadDelay 1000000
  --       let requiredBlock = "8cf6dca6b9505243e36192fa107735024c0000cf4594b1daa2dc4e13ee86f26f" -- H=414474
  --       -- let requiredBlock = "eac5edd4b7d602acae0842b03af3724b76a72bbc608a4bbea2fcf2391f4dacaa" -- H=414477
  --           merkleR c = ModifierId
  --                     $ BA.convert
  --                     $ hash @_ @Blake2b_256
  --                     $ BS.singleton c
  --                    <> unModifierId requiredBlock
  --                    -- <> unModifierId "01caff0fb8e66f459f6a1f8878972145f31ad11533705d6da901803bd8d71efe"
  --                    <> unModifierId "722f9306300d0d96fe8c10de830216d700131614f9e6ce2496e8dba1cbb45951"
  --           ty = ModifierBlockTxs
  --       print requiredBlock
  --       print $ merkleR 102
  --       putStrLn "----"
  --       atomically $ writeTChan inChan $ SockInSendEvent $ MsgOther $ MsgRequestModifier $ RequestModifierMsg ty
  --         -- [requiredBlock]
  --         $ V.fromList [merkleR c | c <- [102]]
  --       -- atomically $ writeTChan inChan $ SockInSendEvent $ MsgOther $ MsgSyncInfo $ SyncInfo [
  --         -- "efa4abde000dca13fd08220d48f70c3e64b49d91102af9d047fd6f35826352e9"
  --         -- ]
  --       pure ()
  --     SockOutInbound (MsgOther (MsgInv (InvMsg itype is))) -> do
  --       atomically $ writeTChan inChan $ SockInSendEvent $ MsgOther $ MsgRequestModifier $ RequestModifierMsg itype $ V.singleton $ V.head is
  --       pure ()
  --     _ -> pure ()
  --   case ev of
  --     SockOutInbound (MsgHandshake h) -> putStrLn "HANDSHAKE" >> print h
  --     SockOutInbound (MsgOther msg) -> case msg of
  --       MsgInv      _ -> putStrLn "MsgInv"
  --       MsgSyncInfo (SyncInfo bids) -> do
  --         putStrLn "MsgSyncInfo"
  --         -- mapM_ print bids
  --       MsgRequestModifier _ -> putStrLn "MsgRequestModifier"
  --       MsgModifier (ModifierMsg ty m) -> do
  --         putStrLn "MsgModifier"
  --         print ty
  --         let bs = modifierBody $ m ! 0
  --         print $ BS.take 32 bs
  --         BS.writeFile "../H_414474" $ BS.drop 32 bs
  --         putStrLn "================================================================"
  --         either print putStrLn $ flip runGet bs $ do            
  --           modId <- ModifierId <$> getBytes 32
  --           n   <- decodeVarInt @Word32
  --           txs <- replicateM 2 (getErgo @ErgoTx)
  --             -- parseLongListOf (getErgo @ErgoTx)
  --           return $ unlines
  --             [ show n
  --             , unlines $ map groom txs
  --             ]
  --         -- print $ ModifierId $ BS.drop 32 $ bs
  --         -- print $ decode @BlockHeader bs
  --         error "ABORT"
  --         -- putStrLn "================"
  --     _ -> print ev


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
  , eboxRegisters   :: [Expr]
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
      n -> error $ "FIXME: ErgoInput: we don't parse ContextExtension " ++ show n
    pure $ ErgoInput bid proof

instance ErgoParser ErgoBox where
  getErgo = do
    eboxValue     <- decodeVarInt @Word64
    eth           <- decodeErgoTreeHeader
    eboxConstants <- case ergoConstantSegreation eth of
      False -> pure []
      True  -> parseLongListOf parseConstant
    -- Parse transaction body
    eboxSpendScript <- parseValue
    eboxHeight      <- decodeVarInt @Word32
    eboxTokens      <- parseShortListOf $ (,) <$> getErgo <*> decodeVarInt @Word32
    --
    eboxRegisters <- get @Word8 >>= \case
      0 -> pure []
      -- FIXME: Do we read full registesr
      n -> do
        traceM $ "-- REGISTERS -- " ++ show n
        replicateM (fromIntegral n) parseValue
    pure ErgoBox{..}

instance ErgoParser ErgoTx where
  getErgo = do
    txInputs  <- parseListOf $ getErgo @ErgoInput
    txDataIn  <- parseListOf $ getErgo @ErgoDataInput
    txTokens  <- parseShortListOf $ getErgo @TokenId
    txOutputs <- parseShortListOf getErgo
    pure ErgoTx{..}

instance ErgoParser ModifierId where
  getErgo = ModifierId <$> getBytes 32

parseConstant :: Get (SType, Value)
parseConstant = do
  ty <- getSType
  c  <- getConstant ty
  pure (ty,c)

getConstant = \case
  SBoolean -> Boolean . (/=0) <$> get @Word8
  SByte    -> Byte  <$> get @Int8
  SShort   -> Short <$> decodeVarInt
  SInt     -> Int   <$> decodeVarInt
  SLong    -> Long  <$> decodeVarInt
  SColl ty -> deserializeColl ty
  SSigmaProp -> SigmaProp <$> getErgo
  s -> error ("getConstant: " ++ show s)

deserializeColl ty = do
  len <- fromIntegral <$> decodeVarInt @Word16
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



----------------------------------------------------------------
-- AST
----------------------------------------------------------------

lvlStr lvl = concat (replicate lvl "| ")

parseValue :: Get Expr
parseValue = parseValueW 0

parseValueW :: Int -> Get Expr
parseValueW lvl = do
  peek >>= \case
    b | b < c_LastConstantCode -> do
          traceM $ lvlStr lvl ++ "parseConstant: " ++ show b
          r <- uncurry Const <$> parseConstant
          traceM $ lvlStr lvl ++ "parseConstant: " ++ show r
          pure r
      | otherwise              -> parseAST lvl

parseAST :: Int -> Get Expr 
parseAST lvl = do
  op <- get
  traceM $ lvlStr lvl ++ "parseAST " ++ show op
  r <- if | op == opTaggedVariableCode                    -> error "OPCODE: opTaggedVariableCode"
          | op == opValUseCode                            -> do
              e <- getErgo
              traceM $ lvlStr lvl ++ "  " ++ show e
              pure $ ValUse $ (MkValUse e)
          -- FIXME: We need access to constant store here
          | op == opConstantPlaceholderCode               -> ConstPlaceholder <$> getErgo
          | op == opSubstConstantsCode                    ->
              SubstConstant <$> goRec <*> goRec <*> goRec
          | op == opLongToByteArrayCode                   -> error "OPCODE: opLongToByteArrayCode"
          | op == opByteArrayToBigIntCode                 -> error "OPCODE: opByteArrayToBigIntCode"
          | op == opByteArrayToLongCode                   -> error "OPCODE: opByteArrayToLongCode"
          | op == opDowncastCode                          -> error "OPCODE: opDowncastCode"
          | op == opUpcastCode                            -> Upcast <$> goRec <*> getSType
          | op == opTrueCode                              -> error "OPCODE: opTrueCode"
          | op == opFalseCode                             -> error "OPCODE: opFalseCode"
          | op == opUnitConstantCode                      -> error "OPCODE: opUnitConstantCode"
          | op == opGroupGeneratorCode                    -> error "OPCODE: opGroupGeneratorCode"
          | op == opConcreteCollectionCode                -> do
              size <- decodeVarInt @Word16
              ty   <- getSType
              xs   <- replicateM (fromIntegral size) goRec
              pure $ Collection ty xs
          | op == opConcreteCollectionBooleanConstantCode -> error "OPCODE: opConcreteCollectionBooleanConstantCode"
          | op == opTupleCode                             -> error "OPCODE: opTupleCode"
          | op == opSelect1Code                           -> error "OPCODE: opSelect1Code"
          | op == opSelect2Code                           -> error "OPCODE: opSelect2Code"
          | op == opSelect3Code                           -> error "OPCODE: opSelect3Code"
          | op == opSelect4Code                           -> error "OPCODE: opSelect4Code"
          | op == opSelect5Code                           -> error "OPCODE: opSelect5Code"
          | op == opSelectFieldCode                       -> SelectField <$> goRec <*> get
          | op == opLtCode                                -> relationOp OpLT
          | op == opLeCode                                -> relationOp OpLE
          | op == opGtCode                                -> relationOp OpGT
          | op == opGeCode                                -> relationOp OpGE
          | op == opEqCode                                -> relationOp OpEq
          | op == opNeqCode                               -> relationOp OpNEq
          | op == opIfCode                                -> If <$> goRec <*> goRec <*> goRec
          | op == opAndCode                               -> And <$> goRec
          | op == opOrCode                                -> Or  <$> goRec
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
          | op == opInputsCode                            -> pure $ GlobalVars Inputs
          | op == opOutputsCode                           -> pure $ GlobalVars Outputs
          | op == opLastBlockUtxoRootHashCode             -> error "OPCODE: opLastBlockUtxoRootHashCode"
          | op == opSelfCode                              -> pure $ GlobalVars SelfBox
          | op == opMinerPubkeyCode                       -> pure $ GlobalVars MinerPubKey
          | op == opMapCollectionCode                     -> error "OPCODE: opMapCollectionCode"
          | op == opExistsCode                            -> Exists <$> goRec <*> goRec
          | op == opForAllCode                            -> ForAll <$> goRec <*> goRec
          | op == opFoldCode                              -> error "OPCODE: opFoldCode"
          | op == opSizeOfCode                            -> SizeOf <$> goRec
          | op == opByIndexCode                           ->
              ByIndex <$> (MkByIndex <$> goRec <*> goRec <*> getOptional goRec)
          | op == opAppendCode                            -> error "OPCODE: opAppendCode"
          | op == opSliceCode                             -> error "OPCODE: opSliceCode"
          | op == opFilterCode                            -> error "OPCODE: opFilterCode"
          | op == opAvlTreeCode                           -> error "OPCODE: opAvlTreeCode"
          | op == opAvlTreeGetCode                        -> error "OPCODE: opAvlTreeGetCode"
          | op == opFlatMapCollectionCode                 -> error "OPCODE: opFlatMapCollectionCode"
          | op == opExtractAmountCode                     -> ExtractAmount      <$> goRec
          | op == opExtractScriptBytesCode                -> ExtractScriptBytes <$> goRec
          | op == opExtractBytesCode                      -> error "OPCODE: opExtractBytesCode"
          | op == opExtractBytesWithNoRefCode             -> error "OPCODE: opExtractBytesWithNoRefCode"
          | op == opExtractIdCode                         -> error "OPCODE: opExtractIdCode"
          | op == opExtractRegisterAs                     ->
              ExtractRegisterAs <$> (MkExtractRegisterAs <$> goRec <*> get <*> getSType)
          | op == opExtractCreationInfoCode               -> ExtractCreationInfo <$> goRec
          | op == opCalcBlake2b256Code                    -> CalcBlake2b256 <$> goRec
          | op == opCalcSha256Code                        -> error "OPCODE: opCalcSha256Code"
          | op == opProveDlogCode                         -> ProveDLog <$> goRec
          | op == opProveDiffieHellmanTupleCode           -> ProveDHTuple <$> goRec <*> goRec <*> goRec <*> goRec
          | op == opSigmaPropIsProvenCode                 -> error "OPCODE: opSigmaPropIsProvenCode"
          | op == opSigmaPropBytesCode                    -> error "OPCODE: opSigmaPropBytesCode"
          | op == opBoolToSigmaPropCode                   -> BoolToSigmaProp <$> goRec
          | op == opTrivialPropFalseCode                  -> error "OPCODE: opTrivialPropFalseCode"
          | op == opTrivialPropTrueCode                   -> error "OPCODE: opTrivialPropTrueCode"
          | op == opDeserializeContextCode                -> error "OPCODE: opDeserializeContextCode"
          | op == opDeserializeRegisterCode               -> error "OPCODE: opDeserializeRegisterCode"
          | op == opValDefCode                            -> do
              e <- getErgo
              traceM $ lvlStr lvl ++ "  " ++ show e
              ValDef <$> (MkValDef <$> pure e <*> goRec)
          | op == opFunDefCode                            -> error "OPCODE: opFunDefCode"
          | op == opBlockValueCode                        -> do
              BlockValue <$> (MkBlockValue <$> parseLongListOf goRec <*> goRec)
          | op == opFuncValueCode                         -> do
              funcValArgs <- parseLongListOf $ getErgo @FuncArg
              traceM $ lvlStr lvl ++ "  " ++ show funcValArgs
              funcValBody <- goRec
              pure $ FuncValue MkFuncValue{..}
          | op == opFuncApplyCode                         -> Apply <$> goRec <*> goRec
          | op == opPropertyCallCode                      -> do
              typeId   <- get @Word8
              methodId <- get @Word8
              obj      <- goRec
              pure $ ProperyCall obj (SMethod typeId methodId)
          | op == opMethodCallCode                        -> error "OPCODE: opMethodCallCode"
          | op == opGlobalCode                            -> pure $ GlobalVars Global
          | op == opSomeValueCode                         -> error "OPCODE: opSomeValueCode"
          | op == opNoneValueCode                         -> pure $ NoneValue
          | op == opGetVarCode                            -> error "OPCODE: opGetVarCode"
          | op == opOptionGetCode                         -> OptionGet <$> goRec
          | op == opOptionGetOrElseCode                   -> error "OPCODE: opOptionGetOrElseCode"
          | op == opOptionIsDefinedCode                   -> OptionIsDefined <$> goRec
          | op == opModQCode                              -> error "OPCODE: opModQCode"
          | op == opPlusModQCode                          -> error "OPCODE: opPlusModQCode"
          | op == opMinusModQCode                         -> error "OPCODE: opMinusModQCode"
          | op == opSigmaAndCode                          -> SigmaAndExpr <$> parseLongListOf goRec
          | op == opSigmaOrCode                           -> SigmaOrExpr  <$> parseLongListOf goRec
          | op == opBinOrCode                             -> bitOp OpBinOr
          | op == opBinAndCode                            -> bitOp OpBinAnd
          | op == opDecodePointCode                       -> DecodePoint <$> goRec
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
  traceM $ lvlStr lvl ++ ")"
  pure r
  where
    relationOp op = BinOp (RelationOp op) <$> goRec <*> goRec
    arithOp    op = BinOp (ArithOp    op) <$> goRec <*> goRec
    bitOp      op = BinOp (BitOp      op) <$> goRec <*> goRec
    goRec = parseValueW (lvl + 1)

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
