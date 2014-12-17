-- | Internal format starters.

module Formatting.Internal where

import           Formatting.Holey

import qualified Data.Text as S (Text)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as T
import           System.IO

-- | Run the formatter and return a lazy 'Text' value.
format :: Format Text a -> a
format m = runHM m T.toLazyText

-- | Run the formatter and return a strict 'S.Text' value.
sformat :: Format S.Text a -> a
sformat m = runHM m (T.toStrict . T.toLazyText)

-- | Run the formatter and return a 'Builder' value.
bprint :: Format Builder a -> a
bprint m = runHM m id

-- | Run the formatter and print out the text to stdout.
fprint :: Format (IO ()) a -> a
fprint m = runHM m (T.putStr . T.toLazyText)

-- | Run the formatter and put the output onto the given 'Handle'.
hprint :: Handle -> Format (IO ()) a -> a
hprint h m = runHM m (T.hPutStr h . T.toLazyText)
