# --------------------------------------------------------------------------------
#   Functions to get doses data:
#   1. GetDosesData.old
#   2. GetDosesData
#   3. ReadSFDoses.old
#   4. SFDosesToAWS
#   5. ReadSFDoses
# --------------------------------------------------------------------------------


#data sources for doses
#VaccinesByCounty.csv ; dose_num age_bin count; all counties up to 2/24, emailed from Tomas Leon
#~/Documents/MissionCovid/CAIR Summary Report.xlsx; ADMIN_DATE VAX_TYPE DOSE_NUM COUNT_65_AND_OLDER COUNT_UNDER_65; SF only, emailed daily
#https://data.chhs.ca.gov/dataset/e283ee5a-cf18-4f20-a92c-ee94a2866ccd/resource/130d7ba2-b6eb-438d-a412-741bde207e1c/download/covid19vaccinesbycounty.csv ;
# c("county", "administered_date", "total_doses", "cumulative_total_doses",
#   "pfizer_doses", "cumulative_pfizer_doses", "moderna_doses", "cumulative_moderna_doses",
#   "jj_doses", "cumulative_jj_doses", "partially_vaccinated", "total_partially_vaccinated",
#   "fully_vaccinated", "cumulative_fully_vaccinated", "at_least_one_dose",
#   "cumulative_at_least_one_dose", "california_flag")
#all counties, updated daily

# GetDosesData.old <- function() {
#   doses.dt <- ReadCsvAWS("VaccinesByCounty.csv")
#   doses.dt[, date := as.Date(date)]
#   doses.dt[, count := as.numeric(count)]
#
#   doses.sf.under <- cbind(ReadSFDoses("Under 65 SF Residents"), age_bin = "<65")
#   doses.sf.over <- cbind(ReadSFDoses("Over 65 SF Residents"), age_bin = "65+")
#   doses.sf <- rbind(doses.sf.under, doses.sf.over)
#   doses.sf[, county := "San Francisco"]
#   doses.sf <- doses.sf[date <= (max(date) - 2)] #last few days incomplete
#
#   doses.dt <- rbind(doses.dt[county != "San Francisco"], doses.sf)
#   setkey(doses.dt, county, date)
#   return(doses.dt)
# }


#' @title Get doses data
#' @param states
#' @param remote a logical value, if \code{TRUE} download all data from remotes, otherwise use local data
#' @export
GetDosesData <- function(states = FALSE, remote = FALSE) {
  if (states) {
    if (remote) {
      state.dt <- fread("https://raw.githubusercontent.com/LocalEpi/LEMMA-Forecasts/master/Inputs/state%20abbreviations.csv")
    } else {
      state.dt <- fread("Inputs/state abbreviations.csv")
    }

    setnames(state.dt, c("StateName", "state"))

    x <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv")
    x[location == "New York State", location := "New York"]
    x <- x[location %in% state.dt$StateName]
    x[, .(date, doses = daily_vaccinations)]
    base_vac <- x[date == as.Date("2021/1/15"), .(location, total_vaccinations)]
    base_dates <- seq(as.Date("2020/12/15"), as.Date("2021/1/15"), by = "day")
    num_days <- length(base_dates)
    base_vac[, per_day := total_vaccinations / num_days]
    x[, people_vaccinated := na.approx(people_vaccinated, na.rm=F), by = "location"]
    x[, people_fully_vaccinated := na.approx(people_fully_vaccinated, na.rm=F), by = "location"]
    x[, frac_2_or_jj := c(NA, diff(people_fully_vaccinated)) / daily_vaccinations, by = "location"]

    y <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations-by-manufacturer.csv")[location == "United States"]
    y[, date := as.Date(date)]
    jj <- y[vaccine == "Johnson&Johnson"]
    setkey(jj, date)
    jj <- rbind(data.table(total_vaccinations = 0), jj, fill=T)
    jj[, new_jj := c(NA, diff(total_vaccinations))]

    z <- y[, .(all_vac = sum(total_vaccinations)), by = "date"]
    z[, new_vac := c(NA, diff(all_vac))]
    z <- merge(z, jj, by = "date", all.x = T)
    z[date < as.Date("2021/3/8"), jj_frac := 0]
    z[date >= as.Date("2021/3/8"), jj_frac := pmin(1, pmax(0, new_jj / new_vac))]
    z <- z[, .(date, jj_frac)]

    doses.dt <- merge(x[, .(date, StateName = location, frac_2_or_jj, doses = daily_vaccinations)], z, by = "date")
    doses.dt[, date := as.Date(date)]
    doses.dt[, frac_2 := pmin(1, pmax(0, frac_2_or_jj - jj_frac))]
    doses.dt[, frac_1 := pmin(1, pmax(0, 1 - (frac_2 + jj_frac)))]
    doses.dt[, dose1 := doses * frac_1]
    doses.dt[, dose2 := doses * frac_2]
    doses.dt[, doseJ := doses * jj_frac]
    doses.dt <- rbind(doses.dt[date > as.Date("2021/1/15"), .(StateName, date, dose1, dose2, doseJ)], merge(data.frame(date = base_dates), as.data.frame(base_vac[, .(StateName = location, dose1 = per_day, dose2 = 0, doseJ = 0)]), by = NULL, all = T)) #merge.data.frame allows by=NULL
    setkey(doses.dt, StateName, date)
    doses.dt <- merge(doses.dt, state.dt, by = "StateName")
    doses.dt <- doses.dt[, .(state, date, dose1, dose2, doseJ)]
  } else {
    x <- fread("https://data.chhs.ca.gov/dataset/e283ee5a-cf18-4f20-a92c-ee94a2866ccd/resource/130d7ba2-b6eb-438d-a412-741bde207e1c/download/covid19vaccinesbycounty.csv")
    x[, doseJ := jj_doses]
    x[, dose2 := fully_vaccinated - jj_doses]
    x[, dose1 := total_doses - (dose2 + doseJ)]
    x[, date := as.Date(administered_date)]
    x <- x[county != "San Francisco", .(county, date, dose1, dose2, doseJ)]
    sf <- ReadSFDoses()
    doses.dt <- rbind(x, sf)
  }
  doses.dt <- doses.dt[date <= (max(date) - 2)] #last few days incomplete
  doses.dt$dose1 <- as.numeric(doses.dt$dose1)
  doses.dt$dose2 <- as.numeric(doses.dt$dose2)
  doses.dt$doseJ <- as.numeric(doses.dt$doseJ)
  return(doses.dt)
}


# ReadSFDoses.old <- function(sheet) {
#   d <- as.data.table(readxl::read_excel("~/Documents/MissionCovid/VaccineData_OverandUnder65_3.19.21.xlsx", sheet = sheet))
#   setnames(d, c("date", "vax_type", "dose", "count"))
#   d[, date := as.Date(date)]
#   d <- d[!(dose %in% c("INV", "UNK"))]
#   d <- d[vax_type %in% c("Moderna", "Pfizer", "J&J")]
#   d[vax_type %in% c("Moderna", "Pfizer"), dose_num := dose]
#   d[vax_type == "J&J", dose_num := "J"]
#   d <- d[, .(count = sum(count)), keyby = c("date", "dose_num")]
#   return(d)
# }


#' @title Upload SF doses to AWS
#' @export
SFDosesToAWS <- function() {
  suppressWarnings(d <- as.data.table(readxl::read_excel("~/Documents/MissionCovid/CAIR Summary Report.xlsx", col_types = c("date", "text", "text", "numeric", "numeric")))) #suppress numeric to date warnings
  setnames(d, c("date", "vax_type", "dose", "count_65plus", "count_under65"))
  write.csv(d, "~/Documents/MissionCovid/CAIR Summary Report.csv", row.names = F)
  aws.s3::put_object("~/Documents/MissionCovid/CAIR Summary Report.csv", bucket = "js-lemma-bucket1", region = "us-west-1")
}


#' @title Read SF doses from AWS
#' @export
ReadSFDoses <- function() {
  #suppressWarnings(d <- as.data.table(readxl::read_excel("~/Documents/MissionCovid/CAIR Summary Report.xlsx", col_types = c("date", "text", "text", "numeric", "numeric")))) #suppress numeric to date warnings
  #setnames(d, c("date", "vax_type", "dose", "count_65plus", "count_under65"))
  d <- ReadCsvAWS("CAIR Summary Report.csv")
  d[, date := as.Date(date)]
  d <- d[!(dose %in% c("INV", "UNK"))]
  d <- d[vax_type %in% c("Moderna", "Pfizer", "Johnson & Johnson")]
  d[vax_type %in% c("Moderna", "Pfizer"), dose_num := dose]
  d[vax_type == "Johnson & Johnson", dose_num := "J"]
  d[, count := count_65plus + count_under65]
  d <- d[, .(count = sum(count)), keyby = c("date", "dose_num")]

  d <- d[, .(dose1 = sum(count * (dose_num == "1")), dose2 = sum(count * (dose_num == "2")), doseJ = sum(count * (dose_num == "J"))), by = "date"]
  d[, county := "San Francisco"]
  return(d)
}
