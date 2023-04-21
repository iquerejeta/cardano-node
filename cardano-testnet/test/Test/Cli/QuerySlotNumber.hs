{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}

module Test.Cli.QuerySlotNumber
  ( hprop_querySlotNumber
  ) where

import           Data.Either (isLeft)
import           Data.Monoid (Last (..))
import qualified Data.Time.Clock as DT
import qualified Data.Time.Format as DT
import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified Hedgehog.Internal.Property as H
import           Prelude
import qualified System.Directory as IO
import           System.Environment (getEnvironment)
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Cardano.Testnet
import           Testnet.Babbage (startTimeOffsetSeconds)
import qualified Testnet.Util.Base as H
import           Testnet.Util.Process
import           Testnet.Util.Runtime

-- | Tests @query slot-number@ cardano-cli command that it returns correct slot numbers for provided utc time
hprop_querySlotNumber :: Property
hprop_querySlotNumber = H.integrationRetryWorkspace 2 "query-slot-number" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  base <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf@Conf { tempBaseAbsPath } <- H.noteShowM $
    mkConf (ProjectBase base) (YamlFilePath configurationTemplate) tempAbsBasePath' Nothing

  let
    testnetOptions = BabbageOnlyTestnetOptions $ babbageDefaultTestnetOptions
      { babbageNodeLoggingFormat = NodeLoggingFormatAsJson
      }
  tr@TestnetRuntime
    { testnetMagic
    , poolNodes
    } <- testnet testnetOptions conf
  ShelleyGenesis{sgSystemStart, sgSlotLength} <- H.noteShowM $ shelleyGenesis tr

  -- there's an offset for start time, so we have to account for that here
  let startTime = ((-1) * startTimeOffsetSeconds) `DT.addUTCTime` sgSystemStart
      slotLength = fromNominalDiffTimeMicro sgSlotLength
      -- how many slots can the checked value differ from
      -- we have 1s precision for UTC timestamp CLI argument, so this value tells how many slots in 1s can be
      slotPrecision = round $ 1 / slotLength
      -- first era has epochsize 100 - that's a different value than in ShelleyGenesis
      epochSize = 100
      passedSlots = 100 :: Int
      passedTime = fromIntegral passedSlots * fromNominalDiffTimeMicro sgSlotLength

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  env <- H.evalIO getEnvironment
  execConfig <- H.noteShow H.ExecConfig
    { H.execConfigEnv = Last $ Just $
      [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName poolSprocket1)
      ]
      -- The environment must be passed onto child process on Windows in order to
      -- successfully start that process.
      <> env
    , H.execConfigCwd = Last $ Just tempBaseAbsPath
    }

  -- {{{ Test cases
  id do
    H.note_ "Double check that start time offset is larger than the passed time we test"
    H.diff startTimeOffsetSeconds (>) passedTime

  id do
    H.note_ "Try to retrieve slot 5s before genesis"
    testTime <- H.note . formatTime $ (-5) `DT.addUTCTime` startTime
    (result, _) <- H.runTestT $ execCli' execConfig
      [ "query", "slot-number"
      , "--testnet-magic", show @Int testnetMagic
      , testTime
      ]
    H.assert $ isLeft result

  id do
    H.note_ "Retrieve slot number for the start time"
    testTime <- H.note $ formatTime startTime
    let expectedSlot = 0
    slot <- H.readM @Int =<< execCli' execConfig
      [ "query", "slot-number"
      , "--testnet-magic", show @Int testnetMagic
      , testTime
      ]
    H.assertWithinTolerance slot expectedSlot slotPrecision

  id do
    H.note_ "Retrieve slot number for some delay"
    testTime <- H.note . formatTime $ passedTime `DT.addUTCTime` startTime
    let expectedSlot = passedSlots
    slot <- H.readM @Int =<< execCli' execConfig
      [ "query", "slot-number"
      , "--testnet-magic", show @Int testnetMagic
      , testTime
      ]
    H.assertWithinTolerance slot expectedSlot slotPrecision

  id do
    H.note_ "Retrieve slot number for next epoch"
    let epochDuration = slotLength * fromIntegral epochSize
    testTime <- H.note . formatTime $ epochDuration `DT.addUTCTime` startTime
    let expectedSlot = epochSize
    slot <- H.readM @Int =<< execCli' execConfig
      [ "query", "slot-number"
      , "--testnet-magic", show @Int testnetMagic
      , testTime
      ]
    H.assertWithinTolerance slot expectedSlot slotPrecision

  id do
    H.note_ "Try to retrieve slot far in the future"
    -- 10 epochs ahead
    let timeOffset = slotLength * fromIntegral epochSize * 10
    testTime <- H.note . formatTime $ (startTimeOffsetSeconds + timeOffset) `DT.addUTCTime` startTime
    (result, _) <- H.runTestT $ execCli' execConfig
      [ "query", "slot-number"
      , "--testnet-magic", show @Int testnetMagic
      , testTime
      ]
    H.assert $ isLeft result

  -- }}}

formatTime :: DT.UTCTime -> String
formatTime = DT.formatTime DT.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

