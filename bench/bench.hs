{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

import           Criterion                  (Benchmark, bench, bgroup, env, nf)
import           Criterion.Main             (defaultMain)
import           Test.QuickCheck

import           Control.DeepSeq
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
main = defaultMain $
  [ benches @String "Small Strings Bench" "William" $
        [ ("formatting", stringF) ]
  , benches @T.Text "Small Text Bench" "William" $
        [ ("formatting", textF) ]
  , benches @LT.Text "Small Lazy Text Bench" "William" $
        [ ("formatting", lazyTextF) ]
  , benches @String "Multiple Interpolations String Bench" (42, "CATALLAXY", True) $
        [ ("formatting", multiStringF) ]
  , benches @T.Text "Multiple Interpolations Text Bench" (42, "CATALLAXY", True) $
        [ ("formatting", multiTextF) ]
  , benches @LT.Text "Multiple Interpolations Lazy Text Bench" (42, "CATALLAXY", True) $
        [ ("formatting", multiLazyTextF) ]
  , env largeishText $ \ ~t -> benches @T.Text "Largeish Text Bench" t $
        [ ("formatting", textF) ]
  , env largeishLazyText $ \ ~lt -> benches @LT.Text "Largeish Lazy Text Bench" lt $
        [ ("formatting", lazyTextF) ]
  ]

largeishText :: IO T.Text
largeishText =
  generate $ T.pack <$> Prelude.take 100000 <$> infiniteListOf arbitrary

largeishLazyText :: IO LT.Text
largeishLazyText =
  generate $ LT.pack <$> Prelude.take 100000 <$> infiniteListOf arbitrary

benches :: forall b a. NFData b => String -> a -> [(String, a -> b)] -> Benchmark
benches groupname arg fs = bgroup groupname (fmap benchF fs)
  where benchF (bname, f) = bench bname $ nf f arg
