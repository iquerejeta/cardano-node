{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.CLI.Helpers
  ( HelpersError(..)
  , Some(..)
  , printWarning
  , deprecationWarning
  , ensureNewFile
  , ensureNewFileLBS
  , pPrintCBOR
  , readCBOR
  , renderHelpersError
  , validateCBOR
  , hushM
  ) where

import           Cardano.Prelude
import           Prelude (String)
import qualified GHC.Show as Show

import           Codec.CBOR.Pretty (prettyHexEnc)
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Term (decodeTerm, encodeTerm)
import           Control.Monad.Trans.Except.Extra (handleIOExceptT, left)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text
import qualified System.Console.ANSI as ANSI
import           System.Console.ANSI
import qualified System.IO as IO

import           Cardano.Binary (Decoder, fromCBOR)
import           Cardano.Chain.Block (fromCBORABlockOrBoundary)
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.CLI.Types

import qualified System.Directory as IO

data HelpersError
  = CBORPrettyPrintError !DeserialiseFailure
  | CBORDecodingError !DeserialiseFailure
  | IOError' !FilePath !IOException
  | OutputMustNotAlreadyExist FilePath
  | ReadCBORFileFailure !FilePath !Text
  deriving Show

renderHelpersError :: HelpersError -> Text
renderHelpersError err =
  case err of
    OutputMustNotAlreadyExist fp -> "Output file/directory must not already exist: " <> Text.pack fp
    ReadCBORFileFailure fp err' -> "CBOR read failure at: " <> Text.pack fp <> Text.pack (show err')
    CBORPrettyPrintError err' -> "Error with CBOR decoding: " <> Text.pack (show err')
    CBORDecodingError err' -> "Error with CBOR decoding: " <> Text.pack (show err')
    IOError' fp ioE -> "Error at: " <> Text.pack fp <> " Error: " <> Text.pack (show ioE)

decodeCBOR
  :: LByteString
  -> (forall s. Decoder s a)
  -> Either HelpersError (LB.ByteString, a)
decodeCBOR bs decoder =
  first CBORDecodingError $ deserialiseFromBytes decoder bs

printWarning :: String -> IO ()
printWarning warning = do
  ANSI.hSetSGR IO.stderr [SetColor Foreground Vivid Yellow]
  IO.hPutStrLn IO.stderr $ "WARNING: " <> warning
  ANSI.hSetSGR IO.stderr [Reset]
    -- We need to flush, or otherwise what's on stdout may have the wrong colour
    -- since it's likely sharing a console with stderr
  IO.hFlush IO.stderr

deprecationWarning :: String -> IO ()
deprecationWarning cmd = printWarning $
  "This CLI command is deprecated.  Please use " <> cmd <> " command instead."

-- | Checks if a path exists and throws and error if it does.
ensureNewFile :: (FilePath -> a -> IO ()) -> FilePath -> a -> ExceptT HelpersError IO ()
ensureNewFile writer outFile blob = do
  exists <- liftIO $ IO.doesPathExist outFile
  when exists $
    left $ OutputMustNotAlreadyExist outFile
  liftIO $ writer outFile blob

ensureNewFileLBS :: FilePath -> ByteString -> ExceptT HelpersError IO ()
ensureNewFileLBS = ensureNewFile BS.writeFile

pPrintCBOR :: LByteString -> ExceptT HelpersError IO ()
pPrintCBOR bs = do
  case deserialiseFromBytes decodeTerm bs of
    Left err -> left $ CBORPrettyPrintError err
    Right (remaining, decodedVal) -> do
      liftIO . putTextLn . toS . prettyHexEnc $ encodeTerm decodedVal
      unless (LB.null remaining) $
        pPrintCBOR remaining

readCBOR :: FilePath -> ExceptT HelpersError IO LByteString
readCBOR fp =
  handleIOExceptT
    (ReadCBORFileFailure fp . toS . displayException)
    (LB.readFile fp)

validateCBOR :: CBORObject -> LByteString -> Either HelpersError Text
validateCBOR cborObject bs =
  case cborObject of
    CBORBlockByron epochSlots -> do
      () <$ decodeCBOR bs (fromCBORABlockOrBoundary epochSlots)
      Right "Valid Byron block."

    CBORDelegationCertificateByron -> do
      () <$ decodeCBOR bs (fromCBOR :: Decoder s Delegation.Certificate)
      Right "Valid Byron delegation certificate."

    CBORTxByron -> do
      () <$ decodeCBOR bs (fromCBOR :: Decoder s UTxO.Tx)
      Right "Valid Byron Tx."

    CBORUpdateProposalByron -> do
      () <$ decodeCBOR bs (fromCBOR :: Decoder s Update.Proposal)
      Right "Valid Byron update proposal."

    CBORVoteByron -> do
      () <$ decodeCBOR bs (fromCBOR :: Decoder s Update.Vote)
      Right "Valid Byron vote."

-- | Convert an Either to a Maybe and execute the supplied handler
-- in the Left case.
hushM :: forall e m a. Monad m => Either e a -> (e -> m ()) -> m (Maybe a)
hushM r f = case r of
  Right a -> return (Just a)
  Left e -> f e >> return Nothing

data Some (f :: k -> Type) where
    Some :: f a -> Some f

instance Show.Show (Some f) where
  show = const "Some(..)"
