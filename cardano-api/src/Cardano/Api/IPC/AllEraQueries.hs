{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.IPC.Monad
  ( LocalStateQueryExpr
  , determineEraInMode
  , determineEraExpr
  , determineShelleyBasedEra
  , executeLocalStateQueryExpr
  , queryExpr
  , queryExprE
  ) where

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except.Extra (left)
import           Data.Kind
import           Data.Proxy

import           Cardano.Ledger.Shelley.Scripts ()
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))

import           Cardano.Api.Block
import           Cardano.Api.Eras
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Version (NodeToClientVersionOf (nodeToClientVersionOf))
import           Cardano.Api.Modes
import           Cardano.Api.Query


{- HLINT ignore "Use const" -}
{- HLINT ignore "Use let" -}


-- | Execute a local state query expression.
executeLocalStateQueryExpr
  :: LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> LocalStateQueryExprWithError error (BlockInMode mode) ChainPoint q () IO a
  -> IO (Either error a)
executeLocalStateQueryExpr connectInfo mpoint f = do
  tmvResultLocalState <- newEmptyTMVarIO
  let waitResult = readTMVar tmvResultLocalState

  connectToLocalNodeWithVersionGeneralized
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just undefined -- setupLocalStateQueryExpr waitResult mpoint tmvResultLocalState ntcVersion f
      , localTxSubmissionClient = Nothing
      , localTxMonitoringClient = Nothing
      }
    )

  atomically waitResult




-- | Get the node server's Node-to-Client version.
getNtcVersion :: LocalStateQueryExpr block point (QueryInMode mode) r IO NodeToClientVersion
getNtcVersion = LocalStateQueryExpr ask


getNtcVersionF :: LocalStateQueryExprWithError (InstantiateSubQueryTypeError q) block point q r IO NodeToClientVersion
getNtcVersionF = lift $ LocalStateQueryExpr ask

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
queryExpr
  :: forall q a block point r. InstantiateSubQueryType q
  => q a
  -> LocalStateQueryExprWithError (InstantiateSubQueryTypeError q) block point q r IO a
queryExpr query = do
  let minNtcVersion = nodeToClientVersionOf query
  ntcVersion <- getNtcVersionF
  if ntcVersion >= minNtcVersion
    then
      lift . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f -> pure $
        Net.Query.SendMsgQuery query $
          Net.Query.ClientStQuerying
          { Net.Query.recvMsgResult = f
          }
    else left $ throwUnsupportedNtc (Proxy  @q) minNtcVersion ntcVersion


-- | A monad expression that determines what era the node is in.
determineEraExpr ::
     ConsensusModeParams mode
  -> LocalStateQueryExprWithError AllEraError block point (QueryInMode mode) r IO AnyCardanoEra
determineEraExpr cModeParams =
  case consensusModeOnly cModeParams of
    ByronMode -> return $ AnyCardanoEra ByronEra
    ShelleyMode -> return $ AnyCardanoEra ShelleyEra
    CardanoMode -> queryExpr $ QueryCurrentEra CardanoModeIsMultiEra




type LocalStateQueryExprWithError e block point query r m a
  = ExceptT e (LocalStateQueryExpr block point query r m) a

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
  -> LocalStateQueryExprWithError (InstantiateSubQueryTypeError q) block point q r IO (EraInMode era mode)
determineEraInMode era cModeParams = do
  let cMode = consensusModeOnly cModeParams
  case toEraInMode era cMode of
    Nothing -> left $ EraInModeE (anyCardanoEra era) (AnyConsensusMode cMode)
    Just eInMode -> return eInMode



-- Queries the work in any era

executeLocalStateQueryExprQueryInMode
  :: LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> LocalStateQueryExprWithError error (BlockInMode mode) ChainPoint (QueryInMode mode) () IO a
  -> IO (Either AllEraError a)
executeLocalStateQueryExprQueryInMode connectInfo mpoint f = do
  tmvResultLocalState <- newEmptyTMVarIO
  let waitResult = readTMVar tmvResultLocalState

  connectToLocalNodeWithVersion
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just $ setupLocalStateQueryExprQueryInMode waitResult mpoint tmvResultLocalState ntcVersion f
      , localTxSubmissionClient = Nothing
      , localTxMonitoringClient = Nothing
      }
    )

  atomically waitResult

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
setupLocalStateQueryExprQueryInMode ::
     STM x
     -- ^ An STM expression that only returns when all protocols are complete.
     -- Protocols must wait until 'waitDone' returns because premature exit will
     -- cause other incomplete protocols to abort which may lead to deadlock.
  -> Maybe ChainPoint
  -> TMVar (Either AllEraError a)
  -> NodeToClientVersion
  -> LocalStateQueryExprWithError AllEraError (BlockInMode mode) ChainPoint (QueryInMode mode) () IO a
  -> Net.Query.LocalStateQueryClient (BlockInMode mode) ChainPoint (QueryInMode mode) IO ()
setupLocalStateQueryExprQueryInMode waitDone mPointVar' resultVar' ntcVersion f =
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar' $
    Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired =
      runContT (runReaderT (runLocalStateQueryExpr $ runExceptT f) ntcVersion) $ \result -> do
        atomically $ putTMVar resultVar' result
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

    , Net.Query.recvMsgFailure = \failure -> do
        atomically $ putTMVar resultVar' (Left (AquFail $ toAcquiringFailure failure))
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgDone ()
    }


queryExprQueryInMode
  :: QueryInMode mode a
  -> LocalStateQueryExprWithError AllEraError block point (QueryInMode mode) r IO a
queryExprQueryInMode q = do
  let minNtcVersion = nodeToClientVersionOf q
  ntcVersion <- lift getNtcVersion
  if ntcVersion >= minNtcVersion
    then
      lift . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f -> pure $
        Net.Query.SendMsgQuery q $
          Net.Query.ClientStQuerying
          { Net.Query.recvMsgResult = f
          }
    else left $ UnsuppVer $ UnsupportedNtcVersionError minNtcVersion ntcVersion





executeLocalStateQueryExprTotal
  :: LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> LocalStateQueryExprWithError (InstantiateSubQueryTypeError q) (InstantiateSubQueryTypeBlockInMode q) ChainPoint q () IO a
  -> IO (Either (InstantiateSubQueryTypeError q) a)
executeLocalStateQueryExprTotal = undefined

executeLocalStateQueryExprF
  :: LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> LocalStateQueryExprWithError (InstantiateSubQueryTypeError q) (InstantiateSubQueryTypeBlockInMode q) ChainPoint q () IO a
  -> IO (Either (InstantiateSubQueryTypeError q) a)
executeLocalStateQueryExprF connectInfo mpoint f = do
  tmvResultLocalState <- newEmptyTMVarIO
  let waitResult = readTMVar tmvResultLocalState

  connectToLocalNodeWithVersion
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just $ setupLocalStateQueryExprF waitResult mpoint tmvResultLocalState ntcVersion f
      , localTxSubmissionClient = Nothing
      , localTxMonitoringClient = Nothing
      }
    )

  atomically waitResult

-- You need to propagate the error type to the top level
-- the error type is tied to the constructor of QueryTotal

setupLocalStateQueryExprTotal :: forall x mode a e.
     STM x
     -- ^ An STM expression that only returns when all protocols are complete.
     -- Protocols must wait until 'waitDone' returns because premature exit will
     -- cause other incomplete protocols to abort which may lead to deadlock.
  -> Maybe ChainPoint
  -> TMVar (Either e a)
  -> NodeToClientVersion
  -> LocalStateQueryExprWithError e (BlockInMode mode) ChainPoint (QueryTotal mode) () IO a
  -> Net.Query.LocalStateQueryClient (BlockInMode mode) ChainPoint (QueryTotal mode) IO ()
setupLocalStateQueryExprTotal waitDone mPointVar' resultVar' ntcVersion f =
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar' $
    Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired =
      runContT (runReaderT (runLocalStateQueryExpr $ runExceptT f) ntcVersion) $ \result -> do
        atomically $ putTMVar resultVar' result
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

    , Net.Query.recvMsgFailure = \failure -> do
        atomically $ putTMVar resultVar' (Left $ throwAcquiringFailureError (Proxy :: Proxy q) failure)
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgDone ()
    }

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
setupLocalStateQueryExpr :: forall x q a.
     STM x
     -- ^ An STM expression that only returns when all protocols are complete.
     -- Protocols must wait until 'waitDone' returns because premature exit will
     -- cause other incomplete protocols to abort which may lead to deadlock.
  -> Maybe ChainPoint
  -> TMVar (Either (InstantiateSubQueryTypeError q) a)
  -> NodeToClientVersion
  -> LocalStateQueryExprWithError (InstantiateSubQueryTypeError q) (InstantiateSubQueryTypeBlockInMode q) ChainPoint q () IO a
  -> Net.Query.LocalStateQueryClient (InstantiateSubQueryTypeBlockInMode q) ChainPoint q IO ()
setupLocalStateQueryExpr waitDone mPointVar' resultVar' ntcVersion f =
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar' $
    Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired =
      runContT (runReaderT (runLocalStateQueryExpr $ runExceptT f) ntcVersion) $ \result -> do
        atomically $ putTMVar resultVar' result
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

    , Net.Query.recvMsgFailure = \failure -> do
        atomically $ putTMVar resultVar' (Left $ throwAcquiringFailureError (Proxy :: Proxy q) failure)
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgDone ()
    }


-- Shelley based era queries and queries that work in any era

executeLocalStateQueryExprSbeQueries
  :: LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> LocalStateQueryExprWithError error (BlockInMode mode) ChainPoint (QueryInModeEraSbe mode) () IO a
  -> IO (Either SBEQueryError a)
executeLocalStateQueryExprSbeQueries = undefined

getNtcVersionSbe :: LocalStateQueryExpr block point (QueryInModeEraSbe mode) r IO NodeToClientVersion
getNtcVersionSbe = LocalStateQueryExpr ask


-- Queries that can be done across eras and in shelley based eras


-- We do not get access to the full Query type therefore the class is defined over
-- q :: * -> * e.g QueryInModeEraSbe mode vs QueryInMode mode result which would be q :: *
class InstantiateSubQueryType (q :: * -> *) where
  type InstantiateSubQueryTypeError q :: Type
  type InstantiateSubQueryTypeBlockInMode q :: Type
  throwAcquiringFailureError :: Proxy q -> AcquireFailure -> InstantiateSubQueryTypeError q
  throwUnsupportedNtc :: Proxy q -> NodeToClientVersion -> NodeToClientVersion -> InstantiateSubQueryTypeError q

instance InstantiateSubQueryType (QueryInModeEraSbe mode) where
  type InstantiateSubQueryTypeError (QueryInModeEraSbe mode) = SBEQueryError
  type InstantiateSubQueryTypeBlockInMode (QueryInModeEraSbe mode) = BlockInMode mode
  throwAcquiringFailureError _ aqf = SbeAllErr $ AquFail $ toAcquiringFailure aqf
  throwUnsupportedNtc p minNtcVersion ntcVersion = SbeAllErr $ UnsuppVer $ UnsupportedNtcVersionError minNtcVersion ntcVersion


-- Has failure modes: UnsuppVer, QueryEraMismatch'
-- | Handles queries that return `Either EraMismatch a`
queryExprE
  :: ConsensusModeParams mode
  -> CardanoEra era
  -> QueryInShelleyBasedEra era a
  -> LocalStateQueryExprWithError SBEQueryError block point (QueryInModeEraSbe mode) r IO a
queryExprE cModeParams era qSbe  = do
  eInMode <- determineEraInMode era cModeParams
  sbe <- determineShelleyBasedEra era
  let q = QueryInModeEraSbe' eInMode $ QueryInShelleyBasedEra sbe qSbe

  result <- queryExpr q
  case result of
    Left eraMismatch -> left $ QueryEraMismatch' eraMismatch
    Right a -> return a

