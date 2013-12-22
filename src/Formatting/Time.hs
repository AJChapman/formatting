{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Formatters for time.

module Formatting.Time
    where

import Data.Time
import System.Locale
import Data.Text (Text)
import qualified Data.Text as T

-- * For 'TimeZone' (and 'ZonedTime' and 'UTCTime'):

-- | Timezone offset on the format @-HHMM@.
tz :: FormatTime t => t -> Text
tz = fmt "%z"

-- | Timezone name.
tzName :: FormatTime t => t -> Text
tzName = fmt "%Z"

-- | As 'dateTimeFmt' @locale@ (e.g. @%a %b %e %H:%M:%S %Z %Y@).
datetime :: FormatTime t => t -> Text
datetime = fmt "%c"

-- * For 'TimeOfDay' (and 'LocalTime' and 'ZonedTime' and 'UTCTime'):

-- | Same as @%H:%M@.
hm :: FormatTime t => t -> Text
hm = fmt "%R"

-- | Same as @%H:%M:%S@.
hms :: FormatTime t => t -> Text
hms = fmt "%T"

-- | As 'timeFmt' @locale@ (e.g. @%H:%M:%S@).
hmsL :: FormatTime t => t -> Text
hmsL = fmt "%X"

-- | As 'time12Fmt' @locale@ (e.g. @%I:%M:%S %p@).
hmsPL :: FormatTime t => t -> Text
hmsPL = fmt "%r"

-- | Day half from ('amPm' @locale@), converted to lowercase, @am@,
-- @pm@.
dayHalf :: FormatTime t => t -> Text
dayHalf = fmt "%P"

-- | Day half from ('amPm' @locale@), @AM@, @PM@.
dayHalfU :: FormatTime t => t -> Text
dayHalfU = fmt "%p"

-- | Hour, 24-hour, leading 0 as needed, @00@ - @23@.
hour24 :: FormatTime t => t -> Text
hour24 = fmt "%H"

-- | Hour, 12-hour, leading 0 as needed, @01@ - @12@.
hour12 :: FormatTime t => t -> Text
hour12 = fmt "%I"

-- | Hour, 24-hour, leading space as needed, @ 0@ - @23@.
hour24S :: FormatTime t => t -> Text
hour24S = fmt "%k"

-- | Hour, 12-hour, leading space as needed, @ 1@ - @12@.
hour12S :: FormatTime t => t -> Text
hour12S = fmt "%l"

-- | Minute, @00@ - @59@.
minute :: FormatTime t => t -> Text
minute = fmt "%M"

-- | Second, without decimal part, @00@ - @60@.
second :: FormatTime t => t -> Text
second = fmt "%S"

-- | Picosecond, including trailing zeros, @000000000000@ -
-- @999999999999@.
pico :: FormatTime t => t -> Text
pico = fmt "%q"

-- | Decimal point and up to 12 second decimals, without trailing
-- zeros. For a whole number of seconds, this produces the empty
-- string.
decimals :: FormatTime t => t -> Text
decimals = fmt "%Q"

-- * For 'UTCTime' and 'ZonedTime'
--
-- Number of whole seconds since the Unix epoch. For times before
-- the Unix epoch, this is a negative number. Note that in @%s.%q@ and @%s%Q@
-- the decimals are positive, not negative. For example, 0.9 seconds
-- before the Unix epoch is formatted as @-1.1@ with @%s%Q@.
epoch :: FormatTime t => t -> Text
epoch = fmt "%s"

-- * For 'Day' (and 'LocalTime' and 'ZonedTime' and 'UTCTime'):

-- | Same as @%m\/%d\/%y@.
dateSlash :: FormatTime t => t -> Text
dateSlash = fmt "%D"

-- | Same as @%Y-%m-%d@.
dateDash :: FormatTime t => t -> Text
dateDash = fmt "%F"

-- | As 'dateFmt' @locale@ (e.g. @%m\/%d\/%y@).
dateSlashL :: FormatTime t => t -> Text
dateSlashL = fmt "%x"

-- | Year.
year :: FormatTime t => t -> Text
year = fmt "%Y"

-- | Last two digits of year, @00@ - @99@.
yy :: FormatTime t => t -> Text
yy = fmt "%y"

-- | Century (being the first two digits of the year), @00@ - @99@.
century :: FormatTime t => t -> Text
century = fmt "%C"

-- | Month name, long form ('fst' from 'months' @locale@), @January@ -
-- @December@.
monthName :: FormatTime t => t -> Text
monthName = fmt "%B"

-- | @ %H] month name, short form ('snd' from 'months' @locale@),
-- @Jan@ - @Dec@.
monthNameShort :: FormatTime t => t -> Text
monthNameShort = fmt "%b"

-- | Month of year, leading 0 as needed, @01@ - @12@.
month :: FormatTime t => t -> Text
month = fmt "%m"

-- | Day of month, leading 0 as needed, @01@ - @31@.
dayOfMonth :: FormatTime t => t -> Text
dayOfMonth = fmt "%d"

-- | Day of month, leading space as needed, @ 1@ - @31@.
dayOfMonthS :: FormatTime t => t -> Text
dayOfMonthS = fmt "%e"

-- | Day of year for Ordinal Date format, @001@ - @366@.
day :: FormatTime t => t -> Text
day = fmt "%j"

-- | Year for Week Date format e.g. @2013@.
weekYear :: FormatTime t => t -> Text
weekYear = fmt "%G"

-- | Last two digits of year for Week Date format, @00@ - @99@.
weekYY :: FormatTime t => t -> Text
weekYY = fmt "%g"

-- | Century (first two digits of year) for Week Date format, @00@ -
-- @99@.
weekCentury :: FormatTime t => t -> Text
weekCentury = fmt "%f"

-- | Week for Week Date format, @01@ - @53@.
week :: FormatTime t => t -> Text
week = fmt "%V"

-- | Day for Week Date format, @1@ - @7@.
dayOfWeek :: FormatTime t => t -> Text
dayOfWeek = fmt "%u"

-- | Day of week, short form ('snd' from 'wDays' @locale@), @Sun@ -
-- @Sat@.
dayNameShort :: FormatTime t => t -> Text
dayNameShort = fmt "%a"

-- | Day of week, long form ('fst' from 'wDays' @locale@), @Sunday@ -
-- @Saturday@.
dayName :: FormatTime t => t -> Text
dayName = fmt "%A"

-- | Week number of year, where weeks start on Sunday (as
-- 'sundayStartWeek'), @00@ - @53@.
weekFromZero :: FormatTime t => t -> Text
weekFromZero = fmt "%U"

-- | Day of week number, @0@ (= Sunday) - @6@ (= Saturday).
dayOfWeekFromZero :: FormatTime t => t -> Text
dayOfWeekFromZero = fmt "%w"

-- | Week number of year, where weeks start on Monday (as
-- 'mondayStartWeek'), @00@ - @53@.
weekOfYearMon :: FormatTime t => t -> Text
weekOfYearMon = fmt "%W"

-- * Internal.

-- | Formatter call. Probably don't want to use this.
fmt :: FormatTime t => Text -> t -> Text
fmt f = T.pack . formatTime defaultTimeLocale (T.unpack f)
