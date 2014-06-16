# formatting

A type-safe text formatting library based on combinators.

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
