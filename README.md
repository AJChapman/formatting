# formatting [![Build Status](https://travis-ci.org/chrisdone/formatting.png)](https://travis-ci.org/chrisdone/formatting) [![Hackage](https://img.shields.io/hackage/v/formatting.svg?style=flat)](https://hackage.haskell.org/package/formatting)

A type-safe text formatting library based on combinators.

[Haddock API documentation](http://chrisdone.github.io/formatting/).

## Introduction

Call with a formatting function like:

* `format` - Format to a lazy `Text` value.
* `sformat` - Format to a strict `Text` value.
* `bprint` - Format to a `Builder` value.
* `fprint` - Format to stdout.
* `hprint` - Format to a `Handle`.

Call syntax:

    format <format combinators> <param1> <param2>

## Comparison

Example:

``` haskell
format ("Person's name is " % text %  ", age is " % hex) "Dave" 54
```

or with short-names:

``` haskell
format ("Person's name is " % t % ", age is " % x) "Dave" 54
```

Similar to C's `printf`:

``` c
printf("Person's name is %s, age is %x","Dave",54);
```

and Common Lisp's `FORMAT`:

``` lisp
(format nil "Person's name is ~a, age is ~x" "Dave" 54)
```

## "Hello, World!": Texts

``` haskell
> format (text % "!") "Hi!"
"Hi!!"
> format (string % "!") "Hi!"
"Hi!!"
```

## 123: Integers

``` haskell
> format int 23
"23"
```

## 23.4: Decimals

``` haskell
> format (fixed 0) 23.3
"23"
> format (fixed 2) 23.3333
"23.33"
> format shortest 23.3333
"23.3333"
> format shortest 0.0
"0"
> format sci 2.3
"2.3"
> format (scifmt Fixed (Just 0)) 2.3
"2"
```

## 1,242: Commas

``` haskell
> format commas 123456778
"123,456,778"
> format commas 1234
"1,234"
```

## 1st: Ordinals

``` haskell
> format ords 1
"1st"
> format ords 2
"2nd"
> format ords 3
"3rd"
> format ords 4
"4th"
```

## 3F: Hex

``` haskell
> format hex 15
"f"
> format hex 25
"19"
```

## Monday 1st June: Dates & times

``` haskell
> now <- getCurrentTime
> later <- getCurrentTime
> format (dayOfMonth % "/" % month % "/" % year) now now now
"16/06/2014"
> format day now
"167"
> format hms now
"08:24:41"
> format tz now
"+0000"
> format datetime now
"Mon Jun 16 08:24:41 UTC 2014"
> format century now
"20"
> format (dayOfMonthOrd % " of " % monthName) now now
"16th of June"
```

## 3 years ago: Time spans

``` haskell
> format (diff False) (diffUTCTime later now)
"2 seconds"
> format (diff True) (diffUTCTime later now)
"in 2 seconds"
> format (diff True) (diffUTCTime now later)
"2 seconds ago"
> format (seconds 0 % " secs") (diffUTCTime now later)
"2 secs"
```

``` haskell
> let Just old = parseTime defaultTimeLocale "%Y" "1980" :: Maybe UTCTime
> format (years 0) (diffUTCTime now old)
"34"
> format (diff True) (diffUTCTime now old)
"in 35 years"
> format (diff True) (diffUTCTime old now)
"35 years ago"
> format (days 0) (diffUTCTime old now)
"12585"
> format (days 0 % " days") (diffUTCTime old now)
"12585 days"
```

## Compose formatters

`%.` is like `%` but feeds one formatter into another:

``` haskell
λ> format (left 2 '0' %. hex) 10
"0a"
```

## Extension

You can include things verbatim in the formatter:

``` haskell
> format (now "This is printed now.")
"This is printed now."
```

Although with `OverloadedStrings` you can just use string literals:

``` haskell
> format "This is printed now."
"This is printed now."
```

You can handle things later which makes the formatter accept arguments:

``` haskell
> format (later (const "This is printed later.")) ()
"This is printed later."
```

The type of the function passed to `later` should return an instance
of `Monoid`.

``` haskell
later :: (a -> Builder) -> Format r (a -> r)
```

The function you format with (`format`, `bprint`, etc.)
will determine the monoid of choice. In the case of this library, the
top-level formating functions expect you to build a text `Builder`:

``` haskell
format :: Format Text a -> a
```

Because builders are efficient generators.

So in this case we will be expected to produce Builders from arguments:

``` haskell
format . later :: (a -> Builder) -> a -> Text
```

To do that for common types you can just re-use the formatting library
and use bprint:

``` haskell
λ> :t bprint
bprint :: Format Builder a -> a
> :t bprint int 23
bprint int 23 :: Builder
```

Coming back to `later`, we can now use it to build our own printer
combinators:

``` haskell
> let mint = later (maybe "" (bprint int))
> :t mint
mint :: Integral a => Format r (Maybe a -> r)
```

Now `mint` is a formatter to show `Maybe Integer`:

``` haskell
> format mint (readMaybe "23")
"23"
> format mint (readMaybe "foo")
""
```

Although a better, more general combinator might be:

``` haskell
> let mfmt x f = later (maybe x (bprint f))
```

Now you can use it to maybe format things:

``` haskell
> format (mfmt "Nope!" int) (readMaybe "foo")
"Nope!"
```

## Scientific

If you're using a type which provides its own builder, like the
`Scientific` type:

``` haskell
import Data.Text.Lazy.Builder.Scientific
scientificBuilder :: Scientific -> Builder
formatScientificBuilder :: FPFormat -> Maybe Int -> Scientific -> Builder
```

Then you can use `later` easily:

``` haskell
> format (later scientificBuilder) 23.4
"23.4"
```

Actually, there are now already two handy combinators (`sci` and
`scifmt`) for the `Scientific` type as shown above in the Decimals
section.
