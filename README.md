Formatting is a type-safe and flexible library for formatting text from built-in or custom data types.

- [Hackage Documentation](https://hackage.haskell.org/package/formatting)
- [The original blog post introducing the library](https://chrisdone.com/posts/formatting/), but note that some of the types have changed: `Holey`

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

| Function         | Outputs       | Also              |
| ---------------- | ------------- | ----------------- |
| `format`         | lazy `Text`   |                   |
| `sformat`        | strict `Text` |                   |
| `bformat`        | `Builder`     |                   |
| `bprint`         | `Builder`     | to be deprecated  |
| `formatToString` | `String`      |                   |
| `fprint`         | to `stdout`   |                   |
| `fprintLn`       | to `stdout`   | appends a newline |
| `hprint`         | to a handle   |                   |
| `hprintLn`       | to a handle   | appends a newline |

Apart from the `%` operator, formatters can also be joined using the monoid append operator (`<>`) to avoid repeating the same argument, they can be chained using `%.`, and there are also formatter combinators for composing more advanced combinators.
More on this below.

### Formatter Quick Reference

Built-in formatters:

| To format a                            | e.g.                     | as               | use                          | short form  | which has type                                       |
| --------------------------------------:| ------------------------ | ---------------- | ---------------------------- | ----------  | ----------------------------------------------------:|
|                            lazy `Text` | `"Hello"`                | `"Hello"`        | `text`                       | `t`         |                            `Format r (TL.Text -> r)` |
|                          strict `Text` | `"World!"`               | `"World!"`       | `stext`                      | `st`        |                             `Format r (T.Text -> r)` | 
|                               `String` | `"Goodbye"`              | `"Goodbye"`      | `string`                     | `s`         |                             `Format r (String -> r)` |
|                              `Builder` | `"Bathtub"`              | `"Bathtub"`      | `builder`                    |             |                        `Format r (TLB.Builder -> r)` |
|                          `Show a => a` | `[1, 2, 3]`              | `"[1, 2, 3]"`    | `shown`                      | `sh`        |                                  `Format r (a -> r)` |
|                                 `Char` | `'!'`                    | `"!"`            | `char`                       | `c`         |                               `Format r (Char -> r)` |
|                      `Integral a => a` | `23`                     | `"23"`           | `int`                        | `d`         |                                  `Format r (a -> r)` |
|                          `Real a => a` | `123.32`                 | `"123.32"`       | `float`                      | `sf`        |                                  `Format r (a -> r)` |
|                          `Real a => a` | `123.32`                 | `"123.320"`      | `fixed 3`                    | `f`         |                           `Int -> Format r (a -> r)` |
|                           `Scientific` | `scientific 60221409 16` | `"6.0221409e23"` | `sci`                        |             |                         `Format r (Scientific -> r)` |
|                           `Scientific` | `scientific 60221409 16` | `"6.022e23"`     | `scifmt Exponent (Just 3)`   |             |                         `Format r (Scientific -> r)` |
|         `Buildable n, Integral n => n` | `123456`                 | `"12.34.56"`     | `groupInt 2 '.'`             |             |                   `Int -> Char -> Format r (n -> r)` |
|         `Buildable n, Integral n => n` | `12000`                  | `"12,000"`       | `commas`                     |             |                                  `Format r (n -> r)` |
|                      `Integral n => n` | `32`                     | `"32nd"`         | `ords`                       |             |                                  `Format r (n -> r)` |
|                     `Num a, Eq a => a` | `1`                      | `"1 ant"`        | `int <> plural "ant" "ants"` |             |                                  `Format r (a -> r)` |
|                     `Num a, Eq a => a` | `2`                      | `"2 ants"`       | `int <> plural "ant" "ants"` |             |                                  `Format r (a -> r)` |
|                          `Enum a => a` | `a`                      | `"97"`           | `asInt`                      |             |                                  `Format r (a -> r)` |
|                      `Integral a => a` | `23`                     | `"10111"`        | `bin`                        | `b`         |                                  `Format r (a -> r)` |
|                      `Integral a => a` | `23`                     | `"0b10111"`      | `prefixBin`                  |             |                                  `Format r (a -> r)` |
|                      `Integral a => a` | `23`                     | `"27"`           | `oct`                        | `o`         |                                  `Format r (a -> r)` |
|                      `Integral a => a` | `23`                     | `"0o27"`         | `prefixOct`                  |             |                                  `Format r (a -> r)` |
|                      `Integral a => a` | `23`                     | `"17"`           | `hex`                        | `x`         |                                  `Format r (a -> r)` |
|                      `Integral a => a` | `23`                     | `"0x17"`         | `prefixHex`                  |             |                                  `Format r (a -> r)` |
|                      `Integral a => a` | `23`                     | `"13"`           | `base 20`                    |             |                           `Int -> Format r (a -> r)` |
|                     `Buildable a => a` | `10`                     | `"  10"`         | `left 4 ' '`                 | `l`         |                   `Int -> Char -> Format r (a -> r)` |
|                     `Buildable a => a` | `10`                     | `"10  "`         | `right 4 ' '`                | `r`         |                   `Int -> Char -> Format r (a -> r)` |
|                     `Buildable a => a` | `10`                     | `" 10 "`         | `center 4 ' '`               |             |                   `Int -> Char -> Format r (a -> r)` |
|                     `Buildable a => a` |  `123456`                | `"123"`          | `fitLeft 3`                  |             |                           `Int -> Format r (a -> r)` |
|                     `Buildable a => a` |  `123456`                | `"456"`          | `fitRight 3`                 |             |                           `Int -> Format r (a -> r)` |
|                     `Buildable a => a` |  `True`                  | `"True"`         | `build`                      |             |                                  `Format r (a -> r)` |
|                                    `a` | `undefined`              | `"gronk!"`       | `fconst "gronk!"`            |             |                       `Builder -> Format r (a -> r)` |

### Formatter Combinator Quick Reference

Formatter combinators take a formatter and modify it somehow, e.g. by using it to format elements of a list, or changing its output.

Built-in formatter combinators:


| To format a                              | e.g.                      | as                                | use                                  | the combinator of which has type                                                                                       |
| ----------------------------------------:| ------------------------  | --------------------------------- | -------------------------------------| ----------------------------------------------------------------------------------------------------------------------:|
| `Foldable t => t a`                      | `[1, 2, 3]`               | "1st2nd3rd"                       | `concatenated ords`                  | `Format Builder (a -> Builder) -> Format r (t a -> r)`                                                                 |
| `Foldable t => t a`                      | `[123, 456, 789]`         | "789456123"                       | `joinedWith (mconcat . reverse) int` | `([Text] -> Text) -> Format Builder (a -> Builder) -> Format r (t a -> r)`                                             |
| `Foldable t => t a`                      | `[1, 2, 3]`               | "1||2||3"                         | `intercalated "||" int`              | `Text -> Format Builder (a -> Builder) -> Format r (t a -> r)`                                                         |
| `Foldable t => t a`                      | `[1, 2, 3]`               | "1 2 3"                           | `unworded int`                       | `Format Builder (a -> Builder) -> Format r (t a -> r)`                                                                 |
| `Foldable t => t a`                      | `[1, 2, 3]`               | "1\n2\n3"                         | `unlined d`                          | `Format Builder (a -> Builder) -> Format r (t a -> r)`                                                                 |
| `Foldable t => t a`                      | `[1, 2, 3]`               | "1 2 3"                           | `spaced int`                         | `Format Builder (a -> Builder) -> Format r (t a -> r)`                                                                 |
| `Foldable t => t a`                      | `[1, 2, 3]`               | "1,2,3"                           | `commaSep int`                       | `Format Builder (a -> Builder) ->  Format r (t a -> r)`                                                                |
| `Foldable t => t a`                      | `[1, 2, 3]`               | "1st, 2nd, 3rd"                   | `commaSpaceSep ords`                 | `Format Builder (a -> Builder) ->  Format r (t a -> r`)                                                                |
| `Foldable t => t a`                      | `["one", "two", "three"]` | "[one, two, three]"               | `list t`                             | `Format Builder (a -> Builder) -> Format r (a -> r)`                                                                   |
| `Foldable t => t a`                      | `["one", "two", "three"]` | "[\"one\", \"two\", \"three\"]"   | `qlist t`                            | `Format Builder (a -> Builder) -> Format r (a -> r)`                                                                   |
| `[a]`                                    | `[1..]`                   | "[1, 10, 11, 100]"                | `took 4 (list bin)`                  | `Int -> Format r ([a] -> r) -> Format r ([a] -> r)`                                                                    |
| `[a]`                                    | `[1..6]`                  | "[4, 5, 6]"                       | `dropped 3 (list int)`               | `Int -> Format r ([a] -> r) -> Format r ([a] -> r)`                                                                    |
| `a`                                      | `"one two\tthree\nfour`   | "one, two, three, four"           | `splat isSpace commaSpaceSep stext`  | `(Char -> Bool) -> (Format r' (Builder -> r') -> Format Builder ([Builder] -> Builder)) -> Format r a -> Format r a`   |
| `a`                                      | `1234567890`              | "[123, 456, 789, 0]"              | `splatWith`                          | `(Text -> [Text]) -> (Format r' (Builder -> r') -> Format Builder ([Builder] -> Builder)) -> Format r a -> Format r a` |
| `a`                                      | `"one,two,three"`         | "one\ntwo\nthree\n"               | `splatOn`                            | `Text -> (Format r' (Builder -> r') -> Format Builder ([Builder] -> Builder)) -> Format r a -> Format r a`             |
| `a`                                      | `"one  two three  "`      | "[one, two, three]"               | `worded`                             | `(Format r' (Builder -> r') -> Format Builder ([Builder] -> Builder)) -> Format r a -> Format r a`                     |
| `a`                                      | `"one\n\ntwo\nthree\n\n`  | "["one", "", "two", "three", ""]" | `lined`                              | `(Format Builder (Builder -> Builder) -> Format Builder ([Builder] -> Builder)) -> Format r a -> Format r a`           |
| `a`                                      | `123456`                  | "654321"                          | `alteredWith`                        | `(Text -> Text) -> Format r a -> Format r a`                                                                           |
| `a`                                      | `"look and boot"`         | "leek and beet"                   | `replaced "oo" "ee" text`            | `Text -> Text -> Format r a -> Format r a`                                                                             |
| `a`                                      | `"look and boot"`         | "LOOK AND BOOT"                   | `uppercased`                         | `Format r a -> Format r a`                                                                                             |
| `a`                                      | `"Look and Boot"`         | "look and boot"                   | `lowercased`                         | `Format r a -> Format r a`                                                                                             |
| `a`                                      | `"look and boot"`         | "Look And Boot"                   | `titlecased`                         | `Format r a -> Format r a`                                                                                             |
| `a`                                      | `"hellos"`                | "he..."                           | `ltruncated 5 text`                  | `Int64 -> Format r a -> Format r a`                                                                                    |
| `a`                                      | `"hellos"`                | "h...s"                           | `ctruncated`                         | `Int64 -> Int64 -> Format r a -> Format r a`                                                                           |
| `a`                                      | `"hellos"`                | "...os"                           | `rtruncated 5 text`                  | `Int64 -> Format r (a -> r)`                                                                                           |
| `a`                                      | `1`                       | "  1"                             | `lpadded 3 int`                      | `Int64 -> Char -> Format r (a -> r) -> Format r (a -> r)`                                                              |
| `a`                                      | `1`                       | "1  "                             | `rpadded 3 int`                      | `Int64 -> Char -> Format r (a -> r) -> Format r (a -> r)`                                                              |
| `a`                                      | `1`                       | " 1 "                             | `cpadded 3 int`                      | `Int64 -> Char -> Format r (a -> r) -> Format r (a -> r)`                                                              |
| `a`                                      | `123`                     | "123 "                            | `lfixed 4 int`                       | `Int64 -> Char -> Format r (a -> r) -> Format r (a -> r)`                                                              |
| `a`                                      | `123456`                  | "1..."                            | `lfixed 4 int`                       | `Int64 -> Char -> Format r (a -> r) -> Format r (a -> r)`                                                              |
| `a`                                      | `123`                     | " 123"                            | `rfixed 4 int`                       | `Int64 -> Char -> Format r (a -> r) -> Format r (a -> r)`                                                              |
| `a`                                      | `123456`                  | "...6"                            | `rfixed 4 int`                       | `Int64 -> Char -> Format r (a -> r) -> Format r (a -> r)`                                                              |
| `a`                                      | `123`                     | "  123 "                          | `cfixed 2 1 ' ' int`                 | `Int64 -> Int64 -> Char -> Format r (a -> r) -> Format r (a -> r)`                                                     |
| `a`                                      | `1234567`                 | "12...7"                          | `cfixed 2 1 ' ' int`                 | `Int64 -> Int64 -> Char -> Format r (a -> r) -> Format r (a -> r)`                                                     |
| `a`                                      | `"Goo"`                   | "McGoo"                           | `prefixed "Mc" t`                    | `Builder -> Format r a -> Format r a`                                                                                  |
| `a`                                      | `"Goo"`                   | "Goosen"                          | `suffixed "sen" t`                   | `Builder -> Format r a -> Format r a`                                                                                  |
| `a`                                      | `"Goo"`                   | "McGooMc"                         | `surrounded "Mc" t`                  | `Builder -> Format r a -> Format r a`                                                                                  |
| `a`                                      | `"Goo"`                   | "McGoosen"                        | `enclosed "Mc" "sen" t`              | `Builder -> Builder -> Format r a -> Format r a`                                                                       |
| `a`                                      | `"Goo"`                   | "'Goo'"                           | `squoted t`                          | `Format r a -> Format r a`                                                                                             |
| `a`                                      | `"Goo"`                   | "\"Goo\""                         | `dquoted t`                          | `Format r a -> Format r a`                                                                                             |
| `a`                                      | `"Goo"`                   | "(Goo)"                           | `parenthesised t`                    | `Format r a -> Format r a`                                                                                             |
| `a`                                      | `"Goo"`                   | "[Goo]"                           | `squared t`                          | `Format r a -> Format r a`                                                                                             |
| `a`                                      | `"Goo"`                   | "{Goo}"                           | `braced t`                           | `Format r a -> Format r a`                                                                                             |
| `a`                                      | `"Goo"`                   | "<Goo>"                           | `angled t`                           | `Format r a -> Format r a`                                                                                             |
| `a`                                      | `"Goo"`                   | "`Goo`"                           | `backticked t`                       | `Format r a -> Format r a`                                                                                             |
| `a`                                      | `"Goo"`                   | "   Goo"                          | `indented 3 t`                       | `Int -> Format r a -> Format r a`                                                                                      |
| `Foldable t => t a`                      | `[1, 2, 3]`               | "  1\n  2\n  3"                   | `indentedLines 2 d`                  | `Format r (a -> r)`                                                                                                    |
| `a`                                      | `"1\n2\n3"`               | "  1\n  2\n  3"                   | `reindented 2 t`                     | `Format r (a -> r)`                                                                                                    |
| `Integral i, RealFrac d => d`            | `6.66`                    | "7"                               | `roundedTo int`                      | `Format r (i -> r) -> Format r (d -> r)`                                                                               |
| `Integral i, RealFrac d => d`            | `6.66`                    | "6"                               | `truncatedTo int`                    | `Format r (i -> r) -> Format r (d -> r)`                                                                               |
| `Integral i, RealFrac d => d`            | `6.66`                    | "7"                               | `ceilingedTo int`                    | `Format r (i -> r) -> Format r (d -> r)`                                                                               |
| `Integral i, RealFrac d => d`            | `6.66`                    | "6"                               | `flooredTo int`                      | `Format r (i -> r) -> Format r (d -> r)`                                                                               |
| field through a `Lens' s a`              | `(1, "goo")`              | "goo"                             | `viewed _2 t`                        | `Lens' s a -> Format r (a -> r) -> Format r (s -> r)`                                                                  |
| field through a record accessor `s -> a` | `(1, "goo")`              | "1"                               | `accessed fst d`                     | `(s -> a) -> Format r (a -> r) -> Format r (s -> r)`                                                                   |
| `Integral a => a`                        | `4097`                    | "0b0001000000000001"              | `binPrefix 16`                       | `Int64 -> Format r (a -> r)`                                                                                           |
| `Integral a => a`                        | `4097`                    | "0o0000000000010001"              | `octPrefix 16`                       | `Int64 -> Format r (a -> r)`                                                                                           |
| `Integral a => a`                        | `4097`                    | "0x0000000000001001"              | `hexPrefix 16`                       | `Int64 -> Format r (a -> r)`                                                                                           |
| `Ord f, Integral a, Fractional f => a`   | `1024`                    | "1.0KB"                           | `bytes shortest`            | `Format Builder (f -> Builder) -> Format r (a -> r)` |
| `Ord f, Integral a, Fractional f => a`   | `1234567890`              | "1.15GB"                          | `bytes (fixed 2)`            | `Format Builder (f -> Builder) -> Format r (a -> r)` |

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
"1.0KB"
> format (bytes (fixed 2 % " ")) (1024*1024*5)
"5.00 MB"
```

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
