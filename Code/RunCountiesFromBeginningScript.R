library(matrixStats)
library(data.table)

setwd("~/Documents/GitHub/LEMMA-Forecasts/")

source('Code/GetCountyData.R')
source('Code/RunCountiesFromBeginning.R')

ReadCsvAWS <- function(object) {
  filestr <- tempfile()
  aws.s3::save_object(object, bucket = "js-lemma-bucket1", file = filestr, region = "us-west-1")
  csv <- fread(filestr)
  value <- unlink(filestr)
  if (!identical(value, 0L)) stop("failed to delete temp file")
  return(csv)
}

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

  county.dt <- GetCountyData(include.regions = F, remove.holidays = T)
  county.dt <- merge(county.dt, seroprev.dt, by = c("date", "county"), all = T)

  county.dt <- county.dt[!(county %in% c("Out Of Country", "Unassigned", "Unknown"))]
}

library(parallel)

setkey(county.dt, county, date)
county.by.pop <- unique(county.dt[!is.na(population), .(county, population)]) #NA population if no hospitalizations
setorder(county.by.pop, -population)
county.set <- county.by.pop[, county]

county.set <- setdiff(county.set, c("Siskiyou", "Humboldt", "El Dorado")) #not currently working
# county.set <- "San Francisco"

print(county.set)
print(system.time(
lemma.set <- mclapply(county.set, RunOneCounty, county.dt, doses.dt, mc.cores = 15)
))


