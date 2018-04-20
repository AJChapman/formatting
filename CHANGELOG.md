HEAD

* Introduced instance `Buildable a => Buildable [a]`.

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
  * Fixed: https://github.com/chrisdone/formatting/issues/31
  * Fixed: https://github.com/chrisdone/formatting/issues/28
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
