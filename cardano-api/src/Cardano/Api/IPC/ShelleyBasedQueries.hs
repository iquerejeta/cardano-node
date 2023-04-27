{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.IPC.ShelleyBasedQueries
  ( LocalStateQueryExpr
  , determineEraInMode
  , determineShelleyBasedEra
  , executeLocalStateQueryExpr
  , queryExpr
  ) where

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except.Extra (left)
import           Control.Tracer
import qualified Data.ByteString.Lazy as LBS
import           Data.Kind
import qualified Data.Map.Strict as Map
import           Data.Void

import           Cardano.Ledger.Shelley.Scripts ()
import qualified Ouroboros.Consensus.Block as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Consensus.Ledger.Query as Consensus
import qualified Ouroboros.Consensus.Network.NodeToClient as Consensus
import qualified Ouroboros.Consensus.Node.NetworkProtocolVersion as Consensus
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import qualified Ouroboros.Network.Mux as Net
import           Ouroboros.Network.NodeToClient (NodeToClientProtocols (..),
                   NodeToClientVersionData (..))
import qualified Ouroboros.Network.NodeToClient as Net
import           Ouroboros.Network.Protocol.ChainSync.Client as Net.Sync
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined as Net.SyncP
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import           Ouroboros.Network.Protocol.LocalTxMonitor.Client (localTxMonitorClientPeer)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx


import           Cardano.Api.Block
import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.InMode
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Types (LocalStateQueryExpr (..), LocalStateQueryExprWithError)
import           Cardano.Api.IPC.Version
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.Protocol
import           Cardano.Api.Query


{- HLINT ignore "Use const" -}
{- HLINT ignore "Use let" -}

{-
So we need an open type family that instantiates the correct
error type based on the query given. The catch is, SBE queries can
return Either
-}

class Instantiate (q :: Type) where
  type InstantiateError q :: Type
  type ReduceQuery q :: Type -> Type
  type QuerySpecificLocalNodeClientProtocols q :: Type

instance Instantiate (QueryInModeEraSbe mode result) where
  type InstantiateError (QueryInModeEraSbe mode result) = SBEQueryError
  type ReduceQuery (QueryInModeEraSbe mode result) = QueryInModeEraSbe mode
  type QuerySpecificLocalNodeClientProtocols (QueryInModeEraSbe mode result) = LocalNodeClientProtocolsInModeSbe mode
--- Types and functions for connectToLocalNodeWithVersion

type LocalNodeClientProtocolsInModeSbe mode =
  LocalNodeClientProtocols
    (BlockInMode mode)
    ChainPoint
    ChainTip
    SlotNo
    (TxInMode mode)
    (TxIdInMode mode)
    (TxValidationErrorInMode mode)
    (QueryInModeEraSbe mode)
    IO

-- | Convert from the mode-parametrised style to the block-parametrised style.
--
mkLocalNodeClientParamsSbe :: forall mode block.
                           ConsensusBlockForMode mode ~ block
                        => ConsensusModeParams mode
                        -> (NodeToClientVersion -> LocalNodeClientProtocolsInModeSbe mode)
                        -> LocalNodeClientParams
mkLocalNodeClientParamsSbe modeparams clients =
    -- For each of the possible consensus modes we pick the concrete block type
    -- (by picking the appropriate 'ProtocolClient' value).
    --
    -- Though it is not immediately visible, this point where we use
    -- 'LocalNodeClientParams' is also where we pick up the necessary class
    -- instances. This works because in each case we have a monomorphic block
    -- type and the instances are all in scope. This is why the use of
    -- LocalNodeClientParams is repeated within each branch of the case:
    -- because it is only within each branch that the GADT match makes the
    -- block type monomorphic.
    --
    case modeparams of
      ByronModeParams epochSlots ->
        LocalNodeClientParamsSingleBlock
          (ProtocolClientInfoArgsByron epochSlots)
          (convLocalNodeClientProtocolsSbe ByronMode . clients)

      ShelleyModeParams ->
        LocalNodeClientParamsSingleBlock
          ProtocolClientInfoArgsShelley
          (convLocalNodeClientProtocolsSbe ShelleyMode . clients)

      CardanoModeParams epochSlots ->
       LocalNodeClientParamsCardano
         (ProtocolClientInfoArgsCardano epochSlots)
         (convLocalNodeClientProtocolsSbe CardanoMode . clients)


convLocalNodeClientProtocolsSbe :: forall mode block.
                                ConsensusBlockForMode mode ~ block
                             => ConsensusMode mode
                             -> LocalNodeClientProtocolsInModeSbe mode
                             -> LocalNodeClientProtocolsForBlock block
convLocalNodeClientProtocolsSbe
    mode
    LocalNodeClientProtocols {
      localChainSyncClient,
      localTxSubmissionClient,
      localStateQueryClient,
      localTxMonitoringClient
    } =
    LocalNodeClientProtocolsForBlock {
      localChainSyncClientForBlock    = case localChainSyncClient of
        NoLocalChainSyncClient -> NoLocalChainSyncClient
        LocalChainSyncClientPipelined clientPipelined -> LocalChainSyncClientPipelined $ convLocalChainSyncClientPipelined mode clientPipelined
        LocalChainSyncClient client -> LocalChainSyncClient $ convLocalChainSyncClient mode client,

      localTxSubmissionClientForBlock = convLocalTxSubmissionClient mode <$>
                                          localTxSubmissionClient,

      localStateQueryClientForBlock   = convLocalStateQueryClientSbe mode <$>
                                          localStateQueryClient,

      localTxMonitoringClientForBlock = convLocalTxMonitoringClient mode <$>
                                          localTxMonitoringClient

    }

convLocalStateQueryClientSbe
  :: forall mode block m a.
     (ConsensusBlockForMode mode ~ block, Functor m)
  => ConsensusMode mode
  -> LocalStateQueryClient (BlockInMode mode) ChainPoint (QueryInModeEraSbe mode) m a
  -> LocalStateQueryClient block (Consensus.Point block)
                           (Consensus.Query block) m a
convLocalStateQueryClientSbe mode =
    Net.Query.mapLocalStateQueryClient
      (toConsensusPointInMode mode)
      toConsensusQuerySbe
      fromConsensusQueryResultSbe

-- | Execute a local state query expression.
executeLocalStateQueryExpr
  :: LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> LocalStateQueryExprWithError SBEQueryError (BlockInMode mode) ChainPoint (QueryInModeEraSbe mode) () IO a
  -> IO (Either SBEQueryError a)
executeLocalStateQueryExpr connectInfo mpoint f = do
  tmvResultLocalState <- newEmptyTMVarIO
  let waitResult = readTMVar tmvResultLocalState

  connectToLocalNodeWithVersionSbe
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just $ setupLocalStateQueryExpr waitResult mpoint tmvResultLocalState ntcVersion f
      , localTxSubmissionClient = Nothing
      , localTxMonitoringClient = Nothing
      }
    )

  atomically waitResult

-- | Establish a connection to a local node and execute the given set of
-- protocol handlers parameterized on the negotiated node-to-client protocol
-- version.
--
connectToLocalNodeWithVersionSbe :: LocalNodeConnectInfo mode
                              -> (NodeToClientVersion -> LocalNodeClientProtocolsInModeSbe mode)
                              -> IO ()
connectToLocalNodeWithVersionSbe LocalNodeConnectInfo {
                     localNodeSocketPath,
                     localNodeNetworkId,
                     localConsensusModeParams
                   } clients =
    Net.withIOManager $ \iomgr ->
      Net.connectTo
        (Net.localSnocket iomgr)
        Net.NetworkConnectTracers {
          Net.nctMuxTracer       = nullTracer,
          Net.nctHandshakeTracer = nullTracer
        }
        versionedProtocls
        localNodeSocketPath
  where
    versionedProtocls =
      -- First convert from the mode-parametrised view of things to the
      -- block-parametrised view and then do the final setup for the versioned
      -- bundles of mini-protocols.
      case mkLocalNodeClientParamsSbe localConsensusModeParams clients of
        LocalNodeClientParamsSingleBlock ptcl clients' ->
          mkVersionedProtocols localNodeNetworkId ptcl clients'
        LocalNodeClientParamsCardano ptcl clients' ->
          mkVersionedProtocols localNodeNetworkId ptcl clients'


mkVersionedProtocols :: forall block.
                        ( Consensus.ShowQuery (Consensus.Query block)
                        , ProtocolClient block
                        )
                     => NetworkId
                     -> ProtocolClientInfoArgs block
                     -> (NodeToClientVersion -> LocalNodeClientProtocolsForBlock block)
                     -> Net.Versions
                          Net.NodeToClientVersion
                          Net.NodeToClientVersionData
                          (Net.OuroborosApplication
                             Net.InitiatorMode
                             Net.LocalAddress
                             LBS.ByteString IO () Void)
mkVersionedProtocols networkid ptcl unversionedClients =
     --TODO: really we should construct specific combinations of
     -- protocols for the versions we know about, with different protocol
     -- versions taking different sets of typed client protocols.
    Net.foldMapVersions
      (\(ptclVersion, ptclBlockVersion) ->
          Net.versionedNodeToClientProtocols
            ptclVersion
            NodeToClientVersionData {
              networkMagic = toNetworkMagic networkid
            }
            (\_connid _ctl -> protocols (unversionedClients ptclVersion) ptclBlockVersion ptclVersion))
      (Map.toList (Consensus.supportedNodeToClientVersions proxy))
  where
    proxy :: Proxy block
    proxy = Proxy

    protocols :: LocalNodeClientProtocolsForBlock block
              -> Consensus.BlockNodeToClientVersion block
              -> NodeToClientVersion
              -> NodeToClientProtocols Net.InitiatorMode LBS.ByteString IO () Void
    protocols
      LocalNodeClientProtocolsForBlock {
        localChainSyncClientForBlock,
        localTxSubmissionClientForBlock,
        localStateQueryClientForBlock,
        localTxMonitoringClientForBlock
      }
      ptclBlockVersion
      ptclVersion =
        NodeToClientProtocols {
          localChainSyncProtocol =
            Net.InitiatorProtocolOnly $ case localChainSyncClientForBlock of
              NoLocalChainSyncClient
                -> Net.MuxPeer nullTracer cChainSyncCodec Net.chainSyncPeerNull
              LocalChainSyncClient client
                -> Net.MuxPeer
                      nullTracer
                      cChainSyncCodec
                      (Net.Sync.chainSyncClientPeer client)
              LocalChainSyncClientPipelined clientPipelined
                -> Net.MuxPeerPipelined
                      nullTracer
                      cChainSyncCodec
                      (Net.SyncP.chainSyncClientPeerPipelined clientPipelined)

        , localTxSubmissionProtocol =
            Net.InitiatorProtocolOnly $
              Net.MuxPeer
                nullTracer
                cTxSubmissionCodec
                (maybe Net.localTxSubmissionPeerNull
                       Net.Tx.localTxSubmissionClientPeer
                       localTxSubmissionClientForBlock)

        , localStateQueryProtocol =
            Net.InitiatorProtocolOnly $
              Net.MuxPeer
                nullTracer
                cStateQueryCodec
                (maybe Net.localStateQueryPeerNull
                       Net.Query.localStateQueryClientPeer
                       localStateQueryClientForBlock)
        , localTxMonitorProtocol =
            Net.InitiatorProtocolOnly $
              Net.MuxPeer
                nullTracer
                cTxMonitorCodec
                (maybe Net.localTxMonitorPeerNull
                       localTxMonitorClientPeer
                       localTxMonitoringClientForBlock)
        }
      where
        Consensus.Codecs {
          Consensus.cChainSyncCodec,
          Consensus.cTxMonitorCodec,
          Consensus.cTxSubmissionCodec,
          Consensus.cStateQueryCodec
        } = Consensus.clientCodecs codecConfig ptclBlockVersion ptclVersion

    codecConfig :: Consensus.CodecConfig block
    codecConfig = Consensus.pClientInfoCodecConfig
                    (protocolClientInfo ptcl)

-- | Get the node server's Node-to-Client version.
getNtcVersion :: LocalStateQueryExpr block point (QueryInModeEraSbe mode) r IO NodeToClientVersion
getNtcVersion = LocalStateQueryExpr ask

determineShelleyBasedEra
  :: CardanoEra era
  -> LocalStateQueryExprWithError SBEQueryError block point (QueryInMode mode) r IO (ShelleyBasedEra era)
determineShelleyBasedEra era =
  case cardanoEraStyle era of
    LegacyByronEra -> left (ExpectedShelleyBasedEra' ())
    ShelleyBasedEra sbe -> return sbe


determineEraInMode
  :: CardanoEra era
  -> ConsensusModeParams mode
  -> LocalStateQueryExprWithError SBEQueryError block point (QueryInModeEraSbe mode) r IO (EraInMode era mode)
determineEraInMode era cModeParams = do
  let cMode = consensusModeOnly cModeParams
  case toEraInMode era cMode of
    Nothing -> left $ EraInModeE (anyCardanoEra era) (AnyConsensusMode cMode)
    Just eInMode -> return eInMode



-- | Use 'queryExpr' in a do block to construct monadic local state queries.
setupLocalStateQueryExpr ::
     STM x
     -- ^ An STM expression that only returns when all protocols are complete.
     -- Protocols must wait until 'waitDone' returns because premature exit will
     -- cause other incomplete protocols to abort which may lead to deadlock.
  -> Maybe ChainPoint
  -> TMVar (Either SBEQueryError a)
  -> NodeToClientVersion
  -> LocalStateQueryExprWithError SBEQueryError (BlockInMode mode) ChainPoint (QueryInModeEraSbe mode) () IO a
  -> Net.Query.LocalStateQueryClient (BlockInMode mode) ChainPoint (QueryInModeEraSbe mode) IO ()
setupLocalStateQueryExpr waitDone mPointVar' resultVar' ntcVersion f =
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar' $
    Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired =
      runContT (runReaderT (runLocalStateQueryExpr $ runExceptT f) ntcVersion) $ \result -> do
        atomically $ putTMVar resultVar' result
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

    , Net.Query.recvMsgFailure = \failure -> do
        atomically $ putTMVar resultVar' (Left $ SbeAllErr (AquFail $ toAcquiringFailure failure))
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgDone ()
    }





-- Has failure modes: UnsuppVer, QueryEraMismatch'
-- | Handles queries that return `Either EraMismatch a`
-- Not sure which is correct

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
queryExpr
  :: QueryInModeEraSbe mode (Either EraMismatch a)
  -> LocalStateQueryExprWithError SBEQueryError block point (QueryInModeEraSbe mode) r IO a
queryExpr query = do
  let minNtcVersion = nodeToClientVersionOf query
  ntcVersion <- lift getNtcVersion
  if ntcVersion >= minNtcVersion
    then do
      res <- lift . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f -> pure $
               Net.Query.SendMsgQuery query $
                 Net.Query.ClientStQuerying
                 { Net.Query.recvMsgResult = f
                 }
      case res of
        Left eraMismatch -> left $ QueryEraMismatch' eraMismatch
        Right v -> return v
    else left $ SbeAllErr $ UnsuppVer $ UnsupportedNtcVersionError minNtcVersion ntcVersion
