{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Api.Environment
  ( EnvSocketError(..)
  , SocketPath(..)
  , readEnvSocketPath
  , renderEnvSocketError
  ) where

import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Prettyprinter as PP
import           System.Environment (lookupEnv)

import           Cardano.Api.Pretty

newtype SocketPath
  = SocketPath { unSocketPath :: FilePath }
  deriving (FromJSON, Show, Eq, Ord)

newtype EnvSocketError = CliEnvVarLookup Text deriving Show

renderEnvSocketError :: EnvSocketError -> Doc Ann
renderEnvSocketError err =
  case err of
    CliEnvVarLookup txt ->
      PP.vsep
        [ "Error while looking up environment variable: CARDANO_NODE_SOCKET_PATH.  Error: "
        , PP.indent 2 $ pretty txt
        ]

-- | Read the node socket path from the environment.
-- Fails if the environment variable is not set.
readEnvSocketPath :: IO (Either EnvSocketError SocketPath)
readEnvSocketPath = do
    mEnvName <- lookupEnv envName
    case mEnvName of
      Just sPath ->
        return . Right $ SocketPath sPath
      Nothing ->
        return . Left $ CliEnvVarLookup (Text.pack envName)
  where
    envName :: String
    envName = "CARDANO_NODE_SOCKET_PATH"
