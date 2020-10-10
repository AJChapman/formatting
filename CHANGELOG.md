7.0.1

* Removed unnecessary dependencies on array and bytestring
* Fixed compile in older GHCs

7.0.0.1

* Added README.md to extra-source-files so it shows up on Hackage

7.0.0

* Introduced `Formatting.Combinators`.
* Fixed: #62 and #60: incorrect formatting of Integral types that do not have negative values (e.g. Word)
* Fixed: #59 rendering of floats e.g. 0.01 as "0.01" rather than "1.0e-2"
* Added dependency of double-conversion to provide fast and correct rendering of floating-point numbers (including the fix for #59).
* Make compatible with bytestring-0.11.0.0
* Removed -O2 ghc flag
* Updated .cabal file version from 1.8 to 2.4

6.3.7

* Introduced instance `Buildable a => Buildable [a]`.

6.3.6

* Bring back `int :: Integral a => Format r (a -> r)`

6.3.5

* Avoid pointless conversions on Float/Double.

6.3.3

* The `Data.Text.Format` hierarchy was reexported as
  `Formatting.Internal.Raw`.

6.3.1

* Proper GHC 7.10 -> GHC 8.4 support

6.3.0

* Folded the `text-format` package into this package, removed the
  `double-conversion` dependency. Lost the following functions in
  this:
  * `prec`
  * `expt`
* Added a test suite with regression tests:
  * Fixed: #31
  * Fixed: #28
  * Fixed: https://github.com/bos/text-format/issues/18

6.2.5

* Changed microseconds to display as "us" to avoid unicode issues.

6.2.1

* Added bytesDecimal

6.2.0

* Dropped Holey/HoleyT in favour of simpler Format type.
* Added Monoid instance.
* Added back Category instance.
* Dropped Functor instance.

6.1.1

* Add support for GHC 7.10 (time update).

6.1.0

* Add formatter for TimeSpec.

6.0.0

* Changed the type of `Format`. Now you write `Format r (a -> r)` instead
  of `Format a`.
* Add `formatToString` function.
