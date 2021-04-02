{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

import           Criterion                  (bench, bgroup, env, nf, whnf)
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

integerF :: Integer -> LT.Text
integerF = F.format F.int

buildF :: F.Buildable a => a -> LT.Text
buildF = F.format F.build

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
    , bgroup "Integers" $
      (\n -> bench (show n) $ whnf integerF n) <$> integersToTest
    , bgroup "Buildable (Integer)" $
      (\n -> bench (show n) $ whnf buildF n) <$> integersToTest
    ]
  where
    integersToTest :: [Integer]
    integersToTest = [0, 1, -1, 10, -10, 99, -99, 100, 123, 12345678, maxIntInteger, -maxIntInteger, maxIntInteger * 2]

    maxIntInteger :: Integer
    maxIntInteger = fromIntegral (maxBound @Int)

largeishText :: IO T.Text
largeishText =
  generate $ T.pack <$> Prelude.take 100000 <$> infiniteListOf arbitrary

largeishLazyText :: IO LT.Text
largeishLazyText =
  generate $ LT.pack <$> Prelude.take 100000 <$> infiniteListOf arbitrary
