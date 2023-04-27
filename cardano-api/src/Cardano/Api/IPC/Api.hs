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

module Cardano.Api.IPC.Api
  (
  ) where

import           Control.Monad.Except
import           Data.Kind
import           Data.Proxy

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except.Extra (left)

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
