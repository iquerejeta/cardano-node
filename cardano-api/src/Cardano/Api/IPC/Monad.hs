{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.Api.IPC.Monad
  ( LocalStateQueryExpr
  , executeLocalStateQueryExpr
  , queryExpr
  , determineEraExpr
    -- * Updated query expression functions
  , LocalStateQueryError(..)
  , determineEraExprN
  , determineEraInModeN
  , determineShelleyBasedEraN
  , executeLocalStateQueryExprN
  , setupLocalStateQueryExprN
  , queryExprN
  , queryExprE
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Except.Extra (left)

import           Cardano.Ledger.Shelley.Scripts ()
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query

import           Cardano.Api.Block
import           Cardano.Api.Eras
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Version
import           Cardano.Api.Modes


{- HLINT ignore "Use const" -}
{- HLINT ignore "Use let" -}

-- | Monadic type for constructing local state query expressions.
--
-- Use 'queryExpr' in a do block to construct queries of this type and convert
-- the expression to a 'Net.Query.LocalStateQueryClient' with 'setupLocalStateQueryExpr'.
--
-- Some consideration was made to use Applicative instead of Monad as the abstraction in
-- order to support pipelining, but we actually have a fair amount of code where the next
-- query depends on the result of the former and therefore actually need Monad.
--
-- In order to make pipelining still possible we can explore the use of Selective Functors
-- which would allow us to straddle both worlds.
newtype LocalStateQueryExpr block point query r m a = LocalStateQueryExpr
  { runLocalStateQueryExpr :: ReaderT NodeToClientVersion (ContT (Net.Query.ClientStAcquired block point query m r) m) a
  } deriving (Functor, Applicative, Monad, MonadReader NodeToClientVersion, MonadIO)

-- | Execute a local state query expression.
executeLocalStateQueryExpr
  :: LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> LocalStateQueryExpr (BlockInMode mode) ChainPoint (QueryInMode mode) () IO a
  -> IO (Either AcquiringFailure a)
executeLocalStateQueryExpr connectInfo mpoint f = do
  tmvResultLocalState <- newEmptyTMVarIO
  let waitResult = readTMVar tmvResultLocalState

  connectToLocalNodeWithVersion
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

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
setupLocalStateQueryExpr ::
     STM x
     -- ^ An STM expression that only returns when all protocols are complete.
     -- Protocols must wait until 'waitDone' returns because premature exit will
     -- cause other incomplete protocols to abort which may lead to deadlock.
  -> Maybe ChainPoint
  -> TMVar (Either AcquiringFailure a)
  -> NodeToClientVersion
  -> LocalStateQueryExpr (BlockInMode mode) ChainPoint (QueryInMode mode) () IO a
  -> Net.Query.LocalStateQueryClient (BlockInMode mode) ChainPoint (QueryInMode mode) IO ()
setupLocalStateQueryExpr waitDone mPointVar' resultVar' ntcVersion f =
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar' $
    Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired = runContT (runReaderT (runLocalStateQueryExpr f) ntcVersion) $ \result -> do
        atomically $ putTMVar resultVar' (Right result)
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

    , Net.Query.recvMsgFailure = \failure -> do
        atomically $ putTMVar resultVar' (Left (toAcquiringFailure failure))
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgDone ()
    }

-- | Get the node server's Node-to-Client version.
getNtcVersion :: LocalStateQueryExpr block point (QueryInMode mode) r IO NodeToClientVersion
getNtcVersion = LocalStateQueryExpr ask

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
queryExpr :: QueryInMode mode a -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError a)
queryExpr q = do
  let minNtcVersion = nodeToClientVersionOf q
  ntcVersion <- getNtcVersion
  if ntcVersion >= minNtcVersion
    then
      fmap Right . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f -> pure $
        Net.Query.SendMsgQuery q $
          Net.Query.ClientStQuerying
          { Net.Query.recvMsgResult = f
          }
    else pure (Left (UnsupportedNtcVersionError minNtcVersion ntcVersion))

-- | A monad expression that determines what era the node is in.
determineEraExpr ::
     ConsensusModeParams mode
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError AnyCardanoEra)
determineEraExpr cModeParams = runExceptT $
  case consensusModeOnly cModeParams of
    ByronMode -> return $ AnyCardanoEra ByronEra
    ShelleyMode -> return $ AnyCardanoEra ShelleyEra
    CardanoMode -> ExceptT $ queryExpr $ QueryCurrentEra CardanoModeIsMultiEra


data LocalStateQueryError
  = AcquiringFailureError AcquiringFailure
  | UnsupportedNtcVersionError' UnsupportedNtcVersionError
  | EraMismatchError' EraMismatch
  | EraInModeError AnyCardanoEra AnyConsensusMode
  | ExpectedShelleyBasedEra
  deriving Show

type LocalStateQueryExprWithError block point query r m a
  = ExceptT LocalStateQueryError (LocalStateQueryExpr block point query r m) a

executeLocalStateQueryExprN
  :: LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> LocalStateQueryExprWithError (BlockInMode mode) ChainPoint (QueryInMode mode) () IO a
  -> IO (Either LocalStateQueryError a)
executeLocalStateQueryExprN connectInfo mpoint f = do
  tmvResultLocalState <- newEmptyTMVarIO
  let waitResult = readTMVar tmvResultLocalState

  connectToLocalNodeWithVersion
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just $ setupLocalStateQueryExprN waitResult mpoint tmvResultLocalState ntcVersion f
      , localTxSubmissionClient = Nothing
      , localTxMonitoringClient = Nothing
      }
    )

  atomically waitResult


setupLocalStateQueryExprN ::
     STM x
     -- ^ An STM expression that only returns when all protocols are complete.
     -- Protocols must wait until 'waitDone' returns because premature exit will
     -- cause other incomplete protocols to abort which may lead to deadlock.
  -> Maybe ChainPoint
  -> TMVar (Either LocalStateQueryError a)
  -> NodeToClientVersion
  -> LocalStateQueryExprWithError (BlockInMode mode) ChainPoint (QueryInMode mode) () IO a
  -> Net.Query.LocalStateQueryClient (BlockInMode mode) ChainPoint (QueryInMode mode) IO ()
setupLocalStateQueryExprN waitDone mPointVar' resultVar' ntcVersion f =
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar' $
    Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired =
      runContT (runReaderT (runLocalStateQueryExpr $ runExceptT f) ntcVersion) $ \result -> do
        atomically $ putTMVar resultVar' result
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

    , Net.Query.recvMsgFailure = \failure -> do
        atomically $ putTMVar resultVar' (Left (AcquiringFailureError $ toAcquiringFailure failure))
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgDone ()
    }

-- Update the queryExpr function
queryExprN :: QueryInMode mode a -> LocalStateQueryExprWithError block point (QueryInMode mode) r IO a
queryExprN q = do
  let minNtcVersion = nodeToClientVersionOf q
  ntcVersion <- lift getNtcVersion
  if ntcVersion >= minNtcVersion
    then
      lift . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f -> pure $
        Net.Query.SendMsgQuery q $
          Net.Query.ClientStQuerying
          { Net.Query.recvMsgResult = f
          }
    else left (UnsupportedNtcVersionError' $ UnsupportedNtcVersionError minNtcVersion ntcVersion)

-- | Handles queries that return `Either EraMismatch a`
queryExprE :: QueryInMode mode (Either EraMismatch a) -> LocalStateQueryExprWithError block point (QueryInMode mode) r IO a
queryExprE q = do
  result <- queryExprN q
  case result of
    Left eraMismatch -> left $ EraMismatchError' eraMismatch
    Right a -> return a

-- | A monad expression that determines what era the node is in.
determineEraExprN ::
     ConsensusModeParams mode
  -> LocalStateQueryExprWithError block point (QueryInMode mode) r IO AnyCardanoEra
determineEraExprN cModeParams =
  case consensusModeOnly cModeParams of
    ByronMode -> return $ AnyCardanoEra ByronEra
    ShelleyMode -> return $ AnyCardanoEra ShelleyEra
    CardanoMode -> queryExprN $ QueryCurrentEra CardanoModeIsMultiEra

determineEraInModeN
  :: CardanoEra era
  -> ConsensusModeParams mode
  -> LocalStateQueryExprWithError block point (QueryInMode mode) r IO (EraInMode era mode)
determineEraInModeN era cModeParams = do
  let cMode = consensusModeOnly cModeParams
  case toEraInMode era cMode of
    Nothing -> left $ EraInModeError (anyCardanoEra era) (AnyConsensusMode cMode)
    Just eInMode -> return eInMode

determineShelleyBasedEraN
  :: CardanoEra era
  -> LocalStateQueryExprWithError block point (QueryInMode mode) r IO (ShelleyBasedEra era)
determineShelleyBasedEraN era =
  case cardanoEraStyle era of
    LegacyByronEra -> left ExpectedShelleyBasedEra
    ShelleyBasedEra sbe -> return sbe
