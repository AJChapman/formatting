{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

import           Criterion                  (bench, env, nf)
import           Criterion.Main             (defaultMain)
import           Test.QuickCheck

import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT

import           Formatting                 ((%))
import qualified Formatting                 as F
import qualified Formatting.ShortFormatters as F

-- From string-interpolate's benchmarks

stringF :: String -> String
stringF = F.formatToString ("A fine day to die, " % F.s % ".")

multiStringF :: (Int, String, Bool) -> String
multiStringF (x, y, z) =
  F.formatToString (" foo " % F.d % " bar " % F.s % " baz " % F.sh % " quux ") x y z

textF :: T.Text -> T.Text
textF = F.sformat ("A fine day to die, " % F.st % ".")

multiTextF :: (Int, T.Text, Bool) -> T.Text
multiTextF (x, y, z) =
  F.sformat (" foo " % F.d % " bar " % F.st % " baz " % F.sh % " quux ") x y z

lazyTextF :: LT.Text -> LT.Text
lazyTextF = F.format ("A find day to die, " % F.t % ".")

multiLazyTextF :: (Int, LT.Text, Bool) -> LT.Text
multiLazyTextF (x, y, z) =
  F.format (" foo " % F.d % " bar " % F.t % " baz " % F.sh % " quux ") x y z

main :: IO ()
main = defaultMain
    [ bench "Small Strings"                     $ nf stringF        "William"
    , bench "Small Text"                        $ nf textF          "William"
    , bench "Small Lazy Text"                   $ nf lazyTextF      "William"
    , bench "Multiple Interpolations String"    $ nf multiStringF   (42, "CATALLAXY", True)
    , bench "Multiple Interpolations Text"      $ nf multiTextF     (42, "CATALLAXY", True)
    , bench "Multiple Interpolations Lazy Text" $ nf multiLazyTextF (42, "CATALLAXY", True)
    , env largeishText $ \ ~t ->
        bench "Largeish Text"                   $ nf textF          t
    , env largeishLazyText $ \ ~lt ->
        bench "Largeish Lazy Text"              $ nf lazyTextF      lt
    ]

largeishText :: IO T.Text
largeishText =
  generate $ T.pack <$> Prelude.take 100000 <$> infiniteListOf arbitrary

largeishLazyText :: IO LT.Text
largeishLazyText =
  generate $ LT.pack <$> Prelude.take 100000 <$> infiniteListOf arbitrary
