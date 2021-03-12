library(matrixStats)
library(data.table)


# setwd("~/Documents/GitHub/LEMMA-Forecasts/")

source('Code/GetCountyData.R')
source('Code/RunCountiesFromBeginning.R')

ConvertNegative <- function(value) {
  #-999999 in admits data means 1 2 or 3 => use 2
  stopifnot(all(value >= 0 | is.na(value) | value == -999999))
  value[value == -999999] <- 2
  return(value)
}

ReadCsvAWS <- function(object) {
  filestr <- tempfile()
  aws.s3::save_object(object, bucket = "js-lemma-bucket1", file = filestr, region = "us-west-1")
  csv <- fread(filestr)
  value <- unlink(filestr)
  if (!identical(value, 0L)) stop("failed to delete temp file")
  return(csv)
}

county.dt <- readRDS("~/Documents/Temp/countydt.rds"); cat("temp - using saved county.dt\n")
doses.dt <- readRDS("~/Documents/Temp/dosesdt.rds")
#TODO: move this to GetCountyData
if (!exists("county.dt")) {
  seroprev.dt <- ReadCsvAWS("countySP_ts.csv")
  seroprev.dt[, county := sub(" County", "", county)]
  seroprev.dt[, year := substr(as.character(end_4wk), 1, 4)]
  seroprev.dt[, week := substr(as.character(end_4wk), 5, 6)]
  seroprev.dt[, date := as.Date(paste0(year, "/1/1")) + as.numeric(week) * 7 - 14] #subtract two weeks to get middle of 4 week period
  seroprev.dt <- seroprev.dt[, .(date, county, seroprev.conf = reweightedSP, seroprev.pui = NA_real_)]

  doses.dt <- ReadCsvAWS("VaccinesByCounty.csv")
  doses.dt[, date := as.Date(date)]
  doses.dt[, count := as.numeric(count)]

  county.dt <- GetCountyData(include.regions = F)
  county.dt[, deaths.pui := NA_real_]

  admits.dt <- fread("https://healthdata.gov/node/3651441/download")[state == "CA"]
  admits.dt[, previous_day_admission_adult_covid_confirmed_7_day_sum := ConvertNegative(previous_day_admission_adult_covid_confirmed_7_day_sum)]
  admits.dt[, previous_day_admission_pediatric_covid_confirmed_7_day_sum := ConvertNegative(previous_day_admission_pediatric_covid_confirmed_7_day_sum)]
  admits.dt[, previous_day_admission_adult_covid_suspected_7_day_sum := ConvertNegative(previous_day_admission_adult_covid_suspected_7_day_sum)]
  admits.dt[, previous_day_admission_pediatric_covid_suspected_7_day_sum := ConvertNegative(previous_day_admission_pediatric_covid_suspected_7_day_sum)]
  admits.dt2 <- admits.dt[, .(admits.conf = sum(previous_day_admission_adult_covid_confirmed_7_day_sum + previous_day_admission_pediatric_covid_confirmed_7_day_sum) / 7,
                              admits.pui = sum(previous_day_admission_adult_covid_suspected_7_day_sum + previous_day_admission_pediatric_covid_suspected_7_day_sum) / 7),
                          by = c("fips_code", "collection_week")]
  admits.dt2[, date := as.Date(collection_week) + 3] #add 3 for midpoint of week
  setnames(admits.dt2, "fips_code", "fips")
  admits.dt2 <- merge(admits.dt2, fread("Inputs/CountyFips.csv"))
  admits.dt2[is.na(admits.conf), admits.pui := NA_real_]
  admits.dt2[is.na(admits.pui), admits.conf := NA_real_]

  case.dt <- fread("https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv")
  case.dt[, date := as.Date(date)]
  stopifnot(uniqueN(case.dt[, max(date), by = county]$V1) == 1)
  max.date <- case.dt[, max(date)] - 4 #assume last 4 days are not reliable
  case.dt2 <- case.dt[date >= as.Date("2020/9/25") & date <= max.date, .(county, cases.conf = as.numeric(newcountconfirmed), cases.pui = NA_real_), keyby = c("county", "date")]
  case.dt2[, cases.conf := frollmean(cases.conf, 7), by = county]

  county.dt <- merge(county.dt, case.dt2[, .(date, county, cases.conf, cases.pui)], by = c("date", "county"), all = T)
  county.dt <- merge(county.dt, admits.dt2[, .(date, county, admits.conf, admits.pui)], by = c("date", "county"), all = T)
  county.dt <- merge(county.dt, seroprev.dt, by = c("date", "county"), all = T)
  county.dt <- county.dt[!(county %in% c("Out Of Country", "Unassigned", "Unknown"))]


}

library(parallel)

options(warn = 1)
setkey(county.dt, county, date)
county.by.pop <- unique(county.dt[!is.na(population), .(county, population)]) #NA population if no hospitalizations
setorder(county.by.pop, -population)
county.set <- county.by.pop[, county]

county.dt <- county.dt[date <= as.Date("2021/3/8")] #to compare with LEMMA1

print(county.set)
# include.seroprev <- F
# s=system.time(
#   z.nosero <- mclapply(county.set, RunOneCounty, county.dt, doses.dt, mc.cores = 15)
# )
include.seroprev <- T
s=system.time(
  z.withsero <- mclapply(county.set, RunOneCounty, county.dt, doses.dt, mc.cores = 15)
)


