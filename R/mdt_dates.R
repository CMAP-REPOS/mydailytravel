################################################################################
# Dates for MDT survey period
################################################################################

library(lubridate)

# We want to keep all weekdays from September 4th, 2018 to May 9th, 2019. This
# represents the time period during which the main phase of the travel survey
# was being actively completed. However, several weeks were excluded that need
# to be excluded here as well: the week of Thanksgiving 2018 (November 19th to
# November 23rd); the period between December 24th, 2018 and January 4th, 2019;
# and the week of Spring Break (April 15th to 19th, 2019).
mdt_int <-  interval(ymd_hms("2018-09-04 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2019-05-10 02:59:59",tz = "America/Chicago"))

xgiving <-  interval(ymd_hms("2018-11-19 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2018-11-24 02:59:59",tz = "America/Chicago"))
xmas <-     interval(ymd_hms("2018-12-24 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2019-01-05 02:59:59",tz = "America/Chicago"))
springb <-  interval(ymd_hms("2019-04-15 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2019-04-20 02:59:59",tz = "America/Chicago"))

# We also exclude federal public holidays that fall within this time range that
# were not excluded through the date ranges above: Columbus Day, Veterans Day,
# MLK Day, and President's Day,.
columbus <- interval(ymd_hms("2018-10-08 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2018-10-09 02:59:59",tz = "America/Chicago"))
vets <-     interval(ymd_hms("2018-11-12 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2018-11-13 02:59:59",tz = "America/Chicago"))
mlk <-      interval(ymd_hms("2019-01-21 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2019-01-22 02:59:59",tz = "America/Chicago"))
pres <-     interval(ymd_hms("2019-02-18 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2019-02-19 02:59:59",tz = "America/Chicago"))

# Join all holidays into one list
holidays <- c(mlk,pres,columbus,vets)

# Identify number of non-excluded weekdays in the sample
number_of_weekdays <-
  sum(!weekdays(seq(ymd("2018-09-04"),
                    ymd("2019-05-09"),
                    by = "days")) %in% c("Saturday", "Sunday")) -
  length(holidays) -
  5 - # for Thanksgiving week
  10 - # for Christmas/New Year's weeks
  5   # for Spring Break week


####### ARCHIVE OF ALL 2019 HOLIDAYS

# mlk <-      interval(ymd_hms("2019-01-21 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-01-22 02:59:59",tz = "America/Chicago"))
# pres <-     interval(ymd_hms("2019-02-18 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-02-19 02:59:59",tz = "America/Chicago"))
# memorial <- interval(ymd_hms("2019-05-27 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-05-28 02:59:59",tz = "America/Chicago"))
# july4 <-    interval(ymd_hms("2019-07-04 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-07-05 02:59:59",tz = "America/Chicago"))
# labor <-    interval(ymd_hms("2019-09-02 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-09-03 02:59:59",tz = "America/Chicago"))
# columbus <- interval(ymd_hms("2019-10-14 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-10-15 02:59:59",tz = "America/Chicago"))
# vets <-     interval(ymd_hms("2019-11-11 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-11-12 02:59:59",tz = "America/Chicago"))
# xgiving <-  interval(ymd_hms("2019-11-28 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-11-29 02:59:59",tz = "America/Chicago"))
# blackfri <- interval(ymd_hms("2019-11-29 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-11-30 02:59:59",tz = "America/Chicago"))
# xmas <-     interval(ymd_hms("2019-12-25 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-12-26 02:59:59",tz = "America/Chicago"))
