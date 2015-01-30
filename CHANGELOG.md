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
