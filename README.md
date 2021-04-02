# formatting [![Build Status](https://travis-ci.org/AJChapman/formatting.png)](https://travis-ci.org/AJChapman/formatting) [![Hackage](https://img.shields.io/hackage/v/formatting.svg?style=flat)](https://hackage.haskell.org/package/formatting)

Formatting is a type-safe and flexible library for formatting text from built-in or custom data types.

- [Hackage Documentation](https://hackage.haskell.org/package/formatting)
- [The original blog post introducing the library](https://chrisdone.com/posts/formatting/), but note that some of the types have changed: `Holey` is no longer used, and `Format`'s type has changed to `newtype Format r a = Format {runFormat :: (Builder -> r) -> a}`

## Usage

You will probably need the `OverloadedStrings` language extension, and to import `Formatting`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Formatting
```

You may also need some or all of these:

```haskell
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
```

Now a simple example:

```haskell
> format ("Person's name is " % text % " and age is " % int) "Dave" 54
"Person's name is Dave and age is 54"
```

In this example, the formatters are two string literals (which take no arguments), and two formatters which take arguments: `text`, which takes a lazy `Text`,  and `int` which takes any `Integral`, such as `Int`.
They are all joined together using the `%` operator, producing a formatter which takes two arguments: a lazy `Text` and an `Integral`.
It produces a lazy `Text`, because we used `format`.
To produce other string types, or print the result instead, refer to this table:

| To produce a  | use                |
| ------------- | ------------------ |
| `TL.Text`     | [`format`]         |
| `T.Text`      | [`sformat`]        |
| `Builder`     | [`bformat`]        |
| `String`      | [`formatToString`] |

To print the values instead, refer to this table:

| To print to                   | use           |
| ----------------------------- | ------------- |
| `stdout`                      | [`fprint`]    |
| `stdout`, appending a newline | [`fprintLn`]  |
| a handle                      | [`hprint`]    |
| a handle, appending a newline | [`hprintLn`]  |

Apart from the `%` operator, formatters can also be joined using the monoid append operator (`<>`) to avoid repeating the same argument, they can be chained using `%.`, and there are also formatter combinators for composing more advanced combinators.
More on this below.

### Formatter Quick Reference

Built-in formatters:

| To format a                            | e.g.                     | as               | use                                | short form |
| --------------------------------------:| ------------------------ | ---------------- | ---------------------------------- | ---------- |
|                            lazy `Text` | `"Hello"`                | `"Hello"`        | [`text`]                           | [`t`]      |
|                          strict `Text` | `"World!"`               | `"World!"`       | [`stext`]                          | [`st`]     | 
|                               `String` | `"Goodbye"`              | `"Goodbye"`      | [`string`]                         | [`s`]      |
|                              `Builder` | `"Bathtub"`              | `"Bathtub"`      | [`builder`]                        |            |
|                          `Show a => a` | `[1, 2, 3]`              | `"[1, 2, 3]"`    | [`shown`]                          | [`sh`]     |
|                                 `Char` | `'!'`                    | `"!"`            | [`char`]                           | [`c`]      |
|                      `Integral a => a` | `23`                     | `"23"`           | [`int`]                            | [`d`]      |
|                          `Real a => a` | `123.32`                 | `"123.32"`       | [`float`]                          | [`sf`]     |
|                          `Real a => a` | `123.32`                 | `"123.320"`      | [`fixed`] `3`                      | [`f`]      |
|                           `Scientific` | `scientific 60221409 16` | `"6.0221409e23"` | [`sci`]                            |            |
|                           `Scientific` | `scientific 60221409 16` | `"6.022e23"`     | [`scifmt`] `Exponent (Just 3)`     |            |
|         `Buildable n, Integral n => n` | `123456`                 | `"12.34.56"`     | [`groupInt`] `2 '.'`               |            |
|         `Buildable n, Integral n => n` | `12000`                  | `"12,000"`       | [`commas`]                         |            |
|                      `Integral n => n` | `32`                     | `"32nd"`         | [`ords`]                           |            |
|                     `Num a, Eq a => a` | `1`                      | `"1 ant"`        | `int <>` [`plural`] `"ant" "ants"` |            |
|                     `Num a, Eq a => a` | `2`                      | `"2 ants"`       | `int <>` [`plural`] `"ant" "ants"` |            |
|                          `Enum a => a` | `a`                      | `"97"`           | [`asInt`]                          |            |
|                      `Integral a => a` | `23`                     | `"10111"`        | [`bin`]                            | [`b`]      |
|                      `Integral a => a` | `23`                     | `"0b10111"`      | [`prefixBin`]                      |            |
|                      `Integral a => a` | `23`                     | `"27"`           | [`oct`]                            | [`o`]      |
|                      `Integral a => a` | `23`                     | `"0o27"`         | [`prefixOct`]                      |            |
|                      `Integral a => a` | `23`                     | `"17"`           | [`hex`]                            | [`x`]      |
|                      `Integral a => a` | `23`                     | `"0x17"`         | [`prefixHex`]                      |            |
|                      `Integral a => a` | `23`                     | `"13"`           | [`base`] `20`                      |            |
|                     `Buildable a => a` | `10`                     | `"  10"`         | [`left`] `4 ' '`                   | [`l`]      |
|                     `Buildable a => a` | `10`                     | `"10  "`         | [`right`] `4 ' '`                  | [`r`]      |
|                     `Buildable a => a` | `10`                     | `" 10 "`         | [`center`] `4 ' '`                 |            |
|                     `Buildable a => a` |  `123456`                | `"123"`          | [`fitLeft`] `3`                    |            |
|                     `Buildable a => a` |  `123456`                | `"456"`          | [`fitRight`] `3`                   |            |
|                     `Buildable a => a` |  `True`                  | `"True"`         | [`build`]                          |            |
|                                    `a` | `undefined`              | `"gronk!"`       | [`fconst`] `"gronk!"`              |            |

### Formatter Combinator Quick Reference

Formatter combinators take a formatter and modify it somehow, e.g. by using it to format elements of a list, or changing its output.

Built-in formatter combinators:


| To format a                              | e.g.                      | as                                  | use                                      |
| ----------------------------------------:| ------------------------  | ----------------------------------- | ---------------------------------------- |
| `Maybe a`                                | `Nothing`                 | `"Goodbye"`                         | [`maybed`] `"Goodbye" text`              |
| `Maybe a`                                | `Just "Hello"`            | `"Hello"`                           | [`maybed`] `"Goodbye" text`              |
| `Maybe a`                                | `Nothing`                 | `""`                                | [`optioned`] `text`                      |
| `Maybe a`                                | `Just "Hello"`            | `"Hello"`                           | [`optioned`] `text`                      |
| `Either a b`                             | `Left "Error!"`           | `"Error!"`                          | [`eithered`] `text int`                  |
| `Either a b`                             | `Right 69`                | `"69"`                              | [`eithered`] `text int`                  |
| `Either a x`                             | `Left "bingo"`            | `"bingo"`                           | [`lefted`] `text`                        |
| `Either a x`                             | `Right 16`                | `""`                                | [`lefted`] `text`                        |
| `Either x a`                             | `Right "bingo"`           | `"bingo"`                           | [`righted`] `text`                       |
| `Either x a`                             | `Left 16`                 | `""`                                | [`righted`] `text`                       |
| `Foldable t => t a`                      | `[1, 2, 3]`               | `"1st2nd3rd"`                       | [`concatenated`] `ords`                  |
| `Foldable t => t a`                      | `[123, 456, 789]`         | `"789456123"`                       | [`joinedWith`] `(mconcat . reverse) int` |
| `Foldable t => t a`                      | `[1, 2, 3]`               | `"1\|\|2\|\|3"`                     | [`intercalated`] `"\|\|" int`            |
| `Foldable t => t a`                      | `[1, 2, 3]`               | `"1 2 3"`                           | [`unworded`] `int`                       |
| `Foldable t => t a`                      | `[1, 2, 3]`               | `"1\n2\n3"`                         | [`unlined`] `d`                          |
| `Foldable t => t a`                      | `[1, 2, 3]`               | `"1 2 3"`                           | [`spaced`] `int`                         |
| `Foldable t => t a`                      | `[1, 2, 3]`               | `"1,2,3"`                           | [`commaSep`] `int`                       |
| `Foldable t => t a`                      | `[1, 2, 3]`               | `"1st, 2nd, 3rd"`                   | [`commaSpaceSep`] `ords`                 |
| `Foldable t => t a`                      | `["one", "two", "three"]` | `"[one, two, three]"`               | [`list`] `t`                             |
| `Foldable t => t a`                      | `["one", "two", "three"]` | `"[\"one\", \"two\", \"three\"]"`   | [`qlist`] `t`                            |
| `[a]`                                    | `[1..]`                   | `"[1, 10, 11, 100]"`                | [`took`] `4 (list bin)`                  |
| `[a]`                                    | `[1..6]`                  | `"[4, 5, 6]"`                       | [`dropped`] `3 (list int)`               |
| `a`                                      | `"one two\tthree\nfour`   | `"one, two, three, four"`           | [`splat`] `isSpace commaSpaceSep stext`  |
| `a`                                      | `1234567890`              | `"[123, 456, 789, 0]"`              | [`splatWith`] `(chunksOf 3) list int`    |
| `a`                                      | `"one,two,three"`         | `"one\ntwo\nthree\n"`               | [`splatOn`] `"," unlined t`              |
| `a`                                      | `"one  two three  "`      | `"[one, two, three]"`               | [`worded`] `list text`                   |
| `a`                                      | `"one\n\ntwo\nthree\n\n`  | `"["one", "", "two", "three", ""]"` | [`lined`] `qlist text`                   |
| `a`                                      | `123456`                  | `"654321"`                          | [`alteredWith`] `TL.reverse int`         |
| `a`                                      | `"Data.Char.isUpper`      | `"DCU"`                             | [`charsKeptIf`] `isUpper string`         |
| `a`                                      | `"Data.Char.isUpper`      | `"ata.har.ispper"`                  | [`charsRemovedIf`] `isUpper string`      |
| `a`                                      | `"look and boot"`         | `"leek and beet"`                   | [`replaced`] `"oo" "ee" text`            |
| `a`                                      | `"look and boot"`         | `"LOOK AND BOOT"`                   | [`uppercased`]                           |
| `a`                                      | `"Look and Boot"`         | `"look and boot"`                   | [`lowercased`]                           |
| `a`                                      | `"look and boot"`         | `"Look And Boot"`                   | [`titlecased`]                           |
| `a`                                      | `"hellos"`                | `"he..."`                           | [`ltruncated`] `5 text`                  |
| `a`                                      | `"hellos"`                | `"h...s"`                           | [`ctruncated`]                           |
| `a`                                      | `"hellos"`                | `"...os"`                           | [`rtruncated`] `5 text`                  |
| `a`                                      | `1`                       | `"  1"`                             | [`lpadded`] `3 int`                      |
| `a`                                      | `1`                       | `"1  "`                             | [`rpadded`] `3 int`                      |
| `a`                                      | `1`                       | `" 1 "`                             | [`cpadded`] `3 int`                      |
| `a`                                      | `123`                     | `"123 "`                            | [`lfixed`] `4 int`                       |
| `a`                                      | `123456`                  | `"1..."`                            | [`lfixed`] `4 int`                       |
| `a`                                      | `123`                     | `" 123"`                            | [`rfixed`] `4 int`                       |
| `a`                                      | `123456`                  | `"...6"`                            | [`rfixed`] `4 int`                       |
| `a`                                      | `123`                     | `"  123 "`                          | [`cfixed`] `2 1 ' ' int`                 |
| `a`                                      | `1234567`                 | `"12...7"`                          | [`cfixed`] `2 1 ' ' int`                 |
| `a`                                      | `"Goo"`                   | `"McGoo"`                           | [`prefixed`] `"Mc" t`                    |
| `a`                                      | `"Goo"`                   | `"Goosen"`                          | [`suffixed`] `"sen" t`                   |
| `a`                                      | `"Goo"`                   | `"McGooMc"`                         | [`surrounded`] `"Mc" t`                  |
| `a`                                      | `"Goo"`                   | `"McGoosen"`                        | [`enclosed`] `"Mc" "sen" t`              |
| `a`                                      | `"Goo"`                   | `"'Goo'"`                           | [`squoted`] `t`                          |
| `a`                                      | `"Goo"`                   | `"\"Goo\""`                         | [`dquoted`] `t`                          |
| `a`                                      | `"Goo"`                   | `"(Goo)"`                           | [`parenthesised`] `t`                    |
| `a`                                      | `"Goo"`                   | `"[Goo]"`                           | [`squared`] `t`                          |
| `a`                                      | `"Goo"`                   | `"{Goo}"`                           | [`braced`] `t`                           |
| `a`                                      | `"Goo"`                   | `"<Goo>"`                           | [`angled`] `t`                           |
| `a`                                      | `"Goo"`                   | ``"`Goo`"``                         | [`backticked`] `t`                       |
| `a`                                      | `"Goo"`                   | `"   Goo"`                          | [`indented`] `3 t`                       |
| `Foldable t => t a`                      | `[1, 2, 3]`               | `"  1\n  2\n  3"`                   | [`indentedLines`] `2 d`                  |
| `a`                                      | `"1\n2\n3"`               | `"  1\n  2\n  3"`                   | [`reindented`] `2 t`                     |
| `Integral i, RealFrac d => d`            | `6.66`                    | `"7"`                               | [`roundedTo`] `int`                      |
| `Integral i, RealFrac d => d`            | `6.66`                    | `"6"`                               | [`truncatedTo`] `int`                    |
| `Integral i, RealFrac d => d`            | `6.66`                    | `"7"`                               | [`ceilingedTo`] `int`                    |
| `Integral i, RealFrac d => d`            | `6.66`                    | `"6"`                               | [`flooredTo`] `int`                      |
| field through a `Lens' s a`              | `(1, "goo")`              | `"goo"`                             | [`viewed`] `_2 t`                        |
| field through a record accessor `s -> a` | `(1, "goo")`              | `"1"`                               | [`accessed`] `fst d`                     |
| `Integral a => a`                        | `4097`                    | `"0b0001000000000001"`              | [`binPrefix`] `16`                       |
| `Integral a => a`                        | `4097`                    | `"0o0000000000010001"`              | [`octPrefix`] `16`                       |
| `Integral a => a`                        | `4097`                    | `"0x0000000000001001"`              | [`hexPrefix`] `16`                       |
| `Ord f, Integral a, Fractional f => a`   | `1024`                    | `"1KB"`                             | [`bytes`] `shortest`                     |
| `Ord f, Integral a, Fractional f => a`   | `1234567890`              | `"1.15GB"`                          | [`bytes`] `(fixed 2)`                    |

## Composing formatters

`%.` is like `%` but feeds one formatter into another:

``` haskell
λ> format (left 2 '0' %. hex) 10
"0a"
```

## Using more than one formatter on the same argument

``` haskell
λ> now <- getCurrentTime
λ> format (year % "/" <> month <> "/" % dayOfMonth) now
"2015/01/27"
```

## The Buildable Typeclass

One of the great things about `formatting` is that it doesn't rely on typeclasses: you can define one or more formatters for each of your types.
But you also have the option of defining a 'default' formatter for a type, by implementing the `Buildable` typeclass, which has one method: `build :: p -> Builder`.
Once this is defined for a type, you can use the `build` formatter (which is distinct from the `build` method of `Buildable`!):

```haskell
> format ("Int: " % build % ", Text: " % build) 23 "hello"
"Int: 23, Text: hello"
```

Note that while this can be convenient, it also sacrifices some type-safety: there's nothing preventing you from putting the arguments in the wrong order, because both `Int` and `Text` have a `Buildable` instance.
Note also that if a type already has a `Show` instance then you can use this instead, by using the `shown` formatter.

## Understanding the Types

Formatters generally have a type like this:

```haskell
Format r (a -> r)
```

This describes a formatter that will eventually produce some string type `r`, and takes an `a` as an argument.
For example:

```haskell
int :: Integral a => Format r (a -> r)
```

This takes an `Integral a` argument, and eventually produces an `r`.
Let's work through using this with `format`:

```haskell
-- format has this type:
format :: Format TL.Text a -> a

-- so in 'format int', called with an 'Int', 'int's type specialises to:
int :: Format TL.Text (Int -> TL.Text)

-- and 'format's 'a' parameter specialises to 'Int -> TL.Text':
format :: Format TL.Text (Int -> TL.Text) -> Int -> TL.Text

-- so 'format int' takes an Int and produces text:
format int :: Int -> TL.Text
```

What can be confusing in the above is that `int`'s `a` parameter expands to `Int`, but `format`'s `a` parameter expands to `Int -> TL.Text`.

Now let's look at what happens when we use the `%` operator to append formatters:

```haskell
-- Here are the types of the functions we will use:
(%) :: Format r a -> Format r' r -> Format r' a
int :: Format r (Int -> r) -- simplified for this use
stext :: Format r (T.Text -> r)

-- Within the call to '%', in the expression 'int % stext', the type parameters expand like this:
-- r = T.Text -> r'
-- a = Int -> T.Text -> r'
-- and so we have these types:
int :: Format (T.Text -> r') (Int -> T.Text -> r')
stext :: Format r' (T.Text -> r')
int % stext :: Format r' (Int -> T.Text -> r')

-- And so when we use 'format' we get a function that takes two arguments and produces text:
format (int % stext) :: Int -> T.Text -> TL.Text
```

## Comparison with Other Languages

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

## Formatter Examples

### "Hello, World!": Texts

``` haskell
> format (text % "!") "Hi!"
"Hi!!"
> format (string % "!") "Hi!"
"Hi!!"
```

### 123: Integers

``` haskell
> format int 23
"23"
```

### 23.4: Decimals

``` haskell
> format (fixed 0) 23.3
"23"
> format (fixed 2) 23.3333
"23.33"
> format shortest 23.3333
"23.3333"
> format shortest 0.0
"0.0"
> format sci 2.3
"2.3"
> format (scifmt Fixed (Just 0)) 2.3
"2"
```

### 1,242: Commas

``` haskell
> format commas 123456778
"123,456,778"
> format commas 1234
"1,234"
```

### 1st: Ordinals

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

### 3F: Hex

``` haskell
> format hex 15
"f"
> format hex 25
"19"
```

### Monday 1st June: Dates & times

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

### 3 years ago: Time spans

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

### File sizes

``` haskell
> format (bytes shortest) 1024
"1KB"
> format (bytes (fixed 2 % " ")) (1024*1024*5)
"5.00 MB"
```

### Scientific

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

## Writing your own Formatters

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

## Hacking

### Building with Nix

See [README-nix.md](./README-nix.md).

### Running the Tests

From within your `nix-shell`, run `cabal test`.

The tests are in `test/Spec.hs`.

### Running the Benchmarks

From within your `nix-shell`, run `cabal bench`.

To build the html benchmarking reports, run `cabal bench --benchmark-option=-obench/reports/7.2.0.html > bench/reports/7.2.0.txt`, replacing '7.2.0' with the current version.
This will output the file `bench.html` which you can open in a browser.

The benchmarks are in `bench/bench.hs`.

[`format`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting.html#v:format
[`sformat`]:        https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting.html#v:sformat
[`bformat`]:        https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting.html#v:bformat
[`formatToString`]: https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting.html#v:formatToString
[`fprint`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting.html#v:fprint
[`fprintLn`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting.html#v:fprintLn
[`hprint`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting.html#v:hprint
[`hprintLn`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting.html#v:hprintLn

[`text`]:           https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:text
[`stext`]:          https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:stext
[`string`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:string
[`builder`]:        https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:builder
[`shown`]:          https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:shown
[`char`]:           https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:char
[`int`]:            https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:int
[`float`]:          https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:float
[`fixed`]:          https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:fixed
[`sci`]:            https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:sci
[`scifmt`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:scifmt
[`groupInt`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:groupInt
[`commas`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:commas
[`ords`]:           https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:ords
[`plural`]:            https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:plural
[`asInt`]:          https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:asInt
[`bin`]:            https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:bin
[`prefixBin`]:      https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:prefixBin
[`oct`]:            https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:oct
[`prefixOct`]:      https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:prefixOct
[`hex`]:            https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:hex
[`prefixHex`]:      https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:prefixHex
[`base`]:           https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:base
[`left`]:           https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:left
[`right`]:          https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:right
[`center`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:center
[`fitLeft`]:        https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:fitLeft
[`fitRight`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:fitRight
[`build`]:          https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:build
[`fconst`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:fconst
[`bytes`]:          https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Formatters.html#v:bytes

[`t`]:              https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-ShortFormatters.html#v:t
[`st`]:             https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-ShortFormatters.html#v:st
[`s`]:              https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-ShortFormatters.html#v:s
[`sh`]:             https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-ShortFormatters.html#v:sh
[`c`]:              https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-ShortFormatters.html#v:c
[`d`]:              https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-ShortFormatters.html#v:d
[`sf`]:             https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-ShortFormatters.html#v:sf
[`f`]:              https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-ShortFormatters.html#v:f
[`b`]:              https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-ShortFormatters.html#v:b
[`o`]:              https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-ShortFormatters.html#v:o
[`x`]:              https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-ShortFormatters.html#v:x
[`l`]:              https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-ShortFormatters.html#v:l
[`r`]:              https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-ShortFormatters.html#v:r

[`maybed`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting.html#v:maybed
[`optioned`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting.html#v:optioned
[`eithered`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting.html#v:eithered
[`lefted`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting.html#v:lefted
[`righted`]:        https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting.html#v:righted
[`concatenated`]:   https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting.html#v:concatenated
[`joinedWith`]:     https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:joinedWith
[`intercalated`]:   https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:intercalated
[`unworded`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:unworded
[`unlined`]:        https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:unlined
[`spaced`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:spaced
[`commaSep`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:commaSep
[`commaSpaceSep`]:  https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:commaSpaceSep
[`list`]:           https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:list
[`qlist`]:          https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:qlist
[`took`]:           https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:took
[`dropped`]:        https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:dropped
[`splat`]:          https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:splat
[`splatWith`]:      https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:splatWith
[`splatOn`]:        https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:splatOn
[`worded`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:worded
[`lined`]:          https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:lined
[`alteredWith`]:    https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:alteredWith
[`charsKeptIf`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:charsKeptIf
[`charsRemovedIf`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#vata.har.isppercharsRemovedIf
[`replaced`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:replaced
[`uppercased`]:     https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:uppercased
[`lowercased`]:     https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:lowercased
[`titlecased`]:     https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:titlecased
[`ltruncated`]:     https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:ltruncated
[`ctruncated`]:     https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:ctruncated
[`rtruncated`]:     https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:rtruncated
[`lpadded`]:        https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:lpadded
[`rpadded`]:        https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:rpadded
[`cpadded`]:        https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:cpadded
[`lfixed`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:lfixed
[`rfixed`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:rfixed
[`cfixed`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:cfixed
[`prefixed`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:prefixed
[`suffixed`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:suffixed
[`surrounded`]:     https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:surrounded
[`enclosed`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:enclosed
[`squoted`]:        https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:squoted
[`dquoted`]:        https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:dquoted
[`parenthesised`]:  https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:parenthesised
[`squared`]:        https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:squared
[`braced`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:braced
[`angled`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:angled
[`backticked`]:     https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:backticked
[`indented`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:indented
[`indentedLines`]:  https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:indentedLines
[`reindented`]:     https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:reindented
[`roundedTo`]:      https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:roundedTo
[`truncatedTo`]:    https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:truncatedTo
[`ceilingedTo`]:    https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:ceilingedTo
[`flooredTo`]:      https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:flooredTo
[`viewed`]:         https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:viewed
[`accessed`]:       https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:accessed
[`binPrefix`]:      https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:binPrefix
[`octPrefix`]:      https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:octPrefix
[`hexPrefix`]:      https://hackage.haskell.org/package/formatting-7.1.1/docs/Formatting-Combinators.html#v:hexPrefix
