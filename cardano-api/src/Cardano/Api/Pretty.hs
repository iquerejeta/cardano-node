module Cardano.Api.Pretty
  ( Pretty(..),
    Doc,
    Ann,
    putLn,
    hPutLn,
    renderLazyDefault,
    renderStringDefault,
    renderDefault,

    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,

    InstanceShow(..),
  ) where

import           Control.Exception (bracket_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text (Text)
import           Prettyprinter
import           Prettyprinter.Render.Terminal

import qualified Control.Concurrent.QSem as IO
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.IO as TextLazy
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO

type Ann = AnsiStyle

newtype InstanceShow a = InstanceShow a deriving Show

instance Show a => Pretty (InstanceShow a) where
  pretty (InstanceShow a) = pretty (show a)

sem :: IO.QSem
sem = IO.unsafePerformIO $ IO.newQSem 1
{-# NOINLINE sem #-}

consoleBracket :: IO a -> IO a
consoleBracket = bracket_ (IO.waitQSem sem) (IO.signalQSem sem)

putLn :: MonadIO m => Doc AnsiStyle -> m ()
putLn = liftIO . consoleBracket . TextLazy.putStrLn . renderLazyDefault

hPutLn :: MonadIO m => IO.Handle -> Doc AnsiStyle -> m ()
hPutLn h = liftIO . consoleBracket . TextLazy.hPutStr h . renderLazyDefault

renderStringDefault :: Doc AnsiStyle -> String
renderStringDefault = TextLazy.unpack . renderLazyDefault

renderLazyDefault :: Doc AnsiStyle -> TextLazy.Text
renderLazyDefault = renderLazy . layoutPretty defaultLayoutOptions

renderDefault :: Doc AnsiStyle -> Text
renderDefault = renderStrict . layoutPretty defaultLayoutOptions

black :: Doc AnsiStyle -> Doc AnsiStyle
black = annotate (color Black)

red :: Doc AnsiStyle -> Doc AnsiStyle
red = annotate (color Red)

green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate (color Green)

yellow :: Doc AnsiStyle -> Doc AnsiStyle
yellow = annotate (color Yellow)

blue :: Doc AnsiStyle -> Doc AnsiStyle
blue = annotate (color Blue)

magenta :: Doc AnsiStyle -> Doc AnsiStyle
magenta = annotate (color Magenta)

cyan :: Doc AnsiStyle -> Doc AnsiStyle
cyan = annotate (color Cyan)

white :: Doc AnsiStyle -> Doc AnsiStyle
white = annotate (color White)
