{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Formatters for time.

module Formatting.Time where

import           Data.List
import           Data.Text.Lazy.Builder
import           Formatting.Formatters  hiding (build)
import           Formatting.Holey
import           Formatting.Internal

import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Buildable
import           Data.Time
import           System.Locale

-- * For 'TimeZone' (and 'ZonedTime' and 'UTCTime'):

-- | Timezone offset on the format @-HHMM@.
tz :: FormatTime a => Format a
tz = later (build . fmt "%z")

-- | Timezone name.
tzName :: FormatTime a => Format a
tzName = later (build . fmt "%Z")

-- | As 'dateTimeFmt' @locale@ (e.g. @%a %b %e %H:%M:%S %Z %Y@).
datetime :: FormatTime a => Format a
datetime = later (build . fmt "%c")

-- * For 'TimeOfDay' (and 'LocalTime' and 'ZonedTime' and 'UTCTime'):

-- | Same as @%H:%M@.
hm :: FormatTime a => Format a
hm = later (build . fmt "%R")

-- | Same as @%H:%M:%S@.
hms :: FormatTime a => Format a
hms = later (build . fmt "%T")

-- | As 'timeFmt' @locale@ (e.g. @%H:%M:%S@).
hmsL :: FormatTime a => Format a
hmsL = later (build . fmt "%X")

-- | As 'time12Fmt' @locale@ (e.g. @%I:%M:%S %p@).
hmsPL :: FormatTime a => Format a
hmsPL = later (build . fmt "%r")

-- | Day half from ('amPm' @locale@), converted to lowercase, @am@,
-- @pm@.
dayHalf :: FormatTime a => Format a
dayHalf = later (build . fmt "%P")

-- | Day half from ('amPm' @locale@), @AM@, @PM@.
dayHalfU :: FormatTime a => Format a
dayHalfU = later (build . fmt "%p")

-- | Hour, 24-hour, leading 0 as needed, @00@ - @23@.
hour24 :: FormatTime a => Format a
hour24 = later (build . fmt "%H")

-- | Hour, 12-hour, leading 0 as needed, @01@ - @12@.
hour12 :: FormatTime a => Format a
hour12 = later (build . fmt "%I")

-- | Hour, 24-hour, leading space as needed, @ 0@ - @23@.
hour24S :: FormatTime a => Format a
hour24S = later (build . fmt "%k")

-- | Hour, 12-hour, leading space as needed, @ 1@ - @12@.
hour12S :: FormatTime a => Format a
hour12S = later (build . fmt "%l")

-- | Minute, @00@ - @59@.
minute :: FormatTime a => Format a
minute = later (build . fmt "%M")

-- | Second, without decimal part, @00@ - @60@.
second :: FormatTime a => Format a
second = later (build . fmt "%S")

-- | Picosecond, including trailing zeros, @000000000000@ -
-- @999999999999@.
pico :: FormatTime a => Format a
pico = later (build . fmt "%q")

-- | Decimal point and up to 12 second decimals, without trailing
-- zeros. For a whole number of seconds, this produces the empty
-- string.
decimals :: FormatTime a => Format a
decimals = later (build . fmt "%Q")

-- * For 'UTCTime' and 'ZonedTime'
--
-- Number of whole seconds since the Unix epoch. For times before
-- the Unix epoch, this is a negative number. Note that in @%s.%q@ and @%s%Q@
-- the decimals are positive, not negative. For example, 0.9 seconds
-- before the Unix epoch is formatted as @-1.1@ with @%s%Q@.
epoch :: FormatTime a => Format a
epoch = later (build . fmt "%s")

-- * For 'Day' (and 'LocalTime' and 'ZonedTime' and 'UTCTime'):

-- | Same as @%m\/%d\/%y@.
dateSlash :: FormatTime a => Format a
dateSlash = later (build . fmt "%D")

-- | Same as @%Y-%m-%d@.
dateDash :: FormatTime a => Format a
dateDash = later (build . fmt "%F")

-- | As 'dateFmt' @locale@ (e.g. @%m\/%d\/%y@).
dateSlashL :: FormatTime a => Format a
dateSlashL = later (build . fmt "%x")

-- | Year.
year :: FormatTime a => Format a
year = later (build . fmt "%Y")

-- | Last two digits of year, @00@ - @99@.
yy :: FormatTime a => Format a
yy = later (build . fmt "%y")

-- | Century (being the first two digits of the year), @00@ - @99@.
century :: FormatTime a => Format a
century = later (build . fmt "%C")

-- | Month name, long form ('fst' from 'months' @locale@), @January@ -
-- @December@.
monthName :: FormatTime a => Format a
monthName = later (build . fmt "%B")

-- | @ %H] month name, short form ('snd' from 'months' @locale@),
-- @Jan@ - @Dec@.
monthNameShort :: FormatTime a => Format a
monthNameShort = later (build . fmt "%b")

-- | Month of year, leading 0 as needed, @01@ - @12@.
month :: FormatTime a => Format a
month = later (build . fmt "%m")

-- | Day of month, leading 0 as needed, @01@ - @31@.
dayOfMonth :: FormatTime a => Format a
dayOfMonth = later (build . fmt "%d")

-- | Day of month, @1st@, @2nd@, @25th@, etc.
dayOfMonthOrd :: FormatTime a => Format a
dayOfMonthOrd = later (bprint ords . toInt)
  where toInt :: FormatTime a => a -> Int
        toInt = read . formatTime defaultTimeLocale "%d"

-- | Day of month, leading space as needed, @ 1@ - @31@.
dayOfMonthS :: FormatTime a => Format a
dayOfMonthS = later (build . fmt "%e")

-- | Day of year for Ordinal Date format, @001@ - @366@.
day :: FormatTime a => Format a
day = later (build . fmt "%j")

-- | Year for Week Date format e.g. @2013@.
weekYear :: FormatTime a => Format a
weekYear = later (build . fmt "%G")

-- | Last two digits of year for Week Date format, @00@ - @99@.
weekYY :: FormatTime a => Format a
weekYY = later (build . fmt "%g")

-- | Century (first two digits of year) for Week Date format, @00@ -
-- @99@.
weekCentury :: FormatTime a => Format a
weekCentury = later (build . fmt "%f")

-- | Week for Week Date format, @01@ - @53@.
week :: FormatTime a => Format a
week = later (build . fmt "%V")

-- | Day for Week Date format, @1@ - @7@.
dayOfWeek :: FormatTime a => Format a
dayOfWeek = later (build . fmt "%u")

-- | Day of week, short form ('snd' from 'wDays' @locale@), @Sun@ -
-- @Sat@.
dayNameShort :: FormatTime a => Format a
dayNameShort = later (build . fmt "%a")

-- | Day of week, long form ('fst' from 'wDays' @locale@), @Sunday@ -
-- @Saturday@.
dayName :: FormatTime a => Format a
dayName = later (build . fmt "%A")

-- | Week number of year, where weeks start on Sunday (as
-- 'sundayStartWeek'), @00@ - @53@.
weekFromZero :: FormatTime a => Format a
weekFromZero = later (build . fmt "%U")

-- | Day of week number, @0@ (= Sunday) - @6@ (= Saturday).
dayOfWeekFromZero :: FormatTime a => Format a
dayOfWeekFromZero = later (build . fmt "%w")

-- | Week number of year, where weeks start on Monday (as
-- 'mondayStartWeek'), @00@ - @53@.
weekOfYearMon :: FormatTime a => Format a
weekOfYearMon = later (build . fmt "%W")

-- * Time spans, diffs, 'NominalDiffTime', 'DiffTime', etc.

-- | Display a time span as one time relative to another. Input is
-- assumed to be seconds. Typical inputs are 'NominalDiffTime' and
-- 'DiffTime'.
diff :: (RealFrac n)
     => Bool     -- ^ Display 'in/ago'?
     -> Format n -- ^ Example: '3 seconds ago', 'in three days'.
diff fix =
  later diffed
  where
    diffed ts =
      case find (\(s,_,_) -> abs ts >= s) (reverse ranges) of
        Nothing -> "unknown"
        Just (_,f,base) -> bprint (prefix % f % suffix) (toInt ts base)
      where prefix = now (if fix && ts > 0 then "in " else "")
            suffix = now (if fix && ts < 0 then " ago" else "")
    toInt ts base = abs (round (ts / base))
    ranges =
      [(0,int % " seconds",1)
      ,(minute,fconst "a minute",0)
      ,(minute*2,int % " minutes",minute)
      ,(minute*30,fconst "half an hour",0)
      ,(minute*31,int % " minutes",minute)
      ,(hour,fconst "an hour",0)
      ,(hour*2,int % " hours",hour)
      ,(hour*3,fconst "a few hours",0)
      ,(hour*4,int % " hours",hour)
      ,(day,fconst "a day",0)
      ,(day*2,int % " days",day)
      ,(week,fconst "a week",0)
      ,(week*2,int % " weeks",week)
      ,(month,fconst "a month",0)
      ,(month*2,int % " months",month)
      ,(year,fconst "a year",0)
      ,(year*2,int % " years",year)]
      where year = month * 12
            month = day * 30
            week = day * 7
            day = hour * 24
            hour = minute * 60
            minute = 60

-- | Display the absolute value time span in years.
years :: (RealFrac n)
      => Int -- ^ Decimal places.
      -> Format n
years n = later (bprint (fixed n) . abs . count)
  where count n = n / 365 / 24 / 60 / 60

-- | Display the absolute value time span in days.
days :: (RealFrac n)
      => Int -- ^ Decimal places.
      -> Format n
days n = later (bprint (fixed n) . abs . count)
  where count n = n / 24 / 60 / 60

-- | Display the absolute value time span in hours.
hours :: (RealFrac n)
      => Int -- ^ Decimal places.
      -> Format n
hours n = later (bprint (fixed n) . abs . count)
  where count n = n / 60 / 60

-- | Display the absolute value time span in minutes.
minutes :: (RealFrac n)
      => Int -- ^ Decimal places.
      -> Format n
minutes n = later (bprint (fixed n) . abs . count)
  where count n = n / 60

-- | Display the absolute value time span in seconds.
seconds :: (RealFrac n)
      => Int -- ^ Decimal places.
      -> Format n
seconds n = later (bprint (fixed n) . abs . count)
  where count n = n

-- * Internal.

-- | Formatter call. Probably don't want to use this.
fmt :: FormatTime a => Text -> a -> Text
fmt f = T.pack . formatTime defaultTimeLocale (T.unpack f)
