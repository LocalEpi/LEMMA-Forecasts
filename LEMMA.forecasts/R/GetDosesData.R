# --------------------------------------------------------------------------------
#   Functions to get doses data:
#   1. GetDosesData
#   2. SFDosesToAWS
#   3. ReadSFDoses
# --------------------------------------------------------------------------------


#' @title Get doses data
#' @param states get data for states or counties
#' @param remote a logical value, if \code{TRUE} download all data from remotes, otherwise use local data
#' @export
GetDosesData <- function(states = FALSE, remote = FALSE) {
  if (states) {
    if (remote) {
      state.dt <- data.table::fread("https://raw.githubusercontent.com/LocalEpi/LEMMA-Forecasts/master/Inputs/state%20abbreviations.csv")
    } else {
      state.dt <- data.table::fread("Inputs/state abbreviations.csv")
    }

    setnames(state.dt, c("StateName", "state"))

    x <- data.table::fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv")
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

    y <- data.table::fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations-by-manufacturer.csv")[location == "United States"]
    y[, date := as.Date(date)]
    jj <- y[vaccine == "Johnson&Johnson"]
    data.table::setkey(jj, date)
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
    data.table::setkey(doses.dt, StateName, date)
    doses.dt <- merge(doses.dt, state.dt, by = "StateName")
    doses.dt <- doses.dt[, .(state, date, dose1, dose2, doseJ)]
  } else {
    x <- data.table::fread("https://data.chhs.ca.gov/dataset/e283ee5a-cf18-4f20-a92c-ee94a2866ccd/resource/130d7ba2-b6eb-438d-a412-741bde207e1c/download/covid19vaccinesbycounty.csv")
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

#' @title Upload SF doses to AWS
#' @export
SFDosesToAWS <- function() {
  suppressWarnings(d <- as.data.table(readxl::read_excel("~/Documents/MissionCovid/CAIR Summary Report.xlsx", col_types = c("date", "text", "text", "numeric", "numeric")))) #suppress numeric to date warnings
  setnames(d, c("date", "vax_type", "dose", "count_65plus", "count_under65"))
  write.csv(d, "~/Documents/MissionCovid/CAIR Summary Report.csv", row.names = F)
  aws.s3::put_object("~/Documents/MissionCovid/CAIR Summary Report.csv", bucket = "js-lemma-bucket1", region = "us-west-1")
}


#' @title Read SF doses from AWS
#' @description This function requires the following environment variables to be set:
#' "AWS_ACCESS_KEY_ID","AWS_SECRET_ACCESS_KEY", and "AWS_DEFAULT_REGION" (see \code{\link{EnvVar}}).
#' @param print_outdate write dose number to console or not
#' @export
ReadSFDoses <- function(print_outdate = FALSE) {
  #suppressWarnings(d <- as.data.table(readxl::read_excel("~/Documents/MissionCovid/CAIR Summary Report.xlsx", col_types = c("date", "text", "text", "numeric", "numeric")))) #suppress numeric to date warnings
  #setnames(d, c("date", "vax_type", "dose", "count_65plus", "count_under65"))
  d <- ReadCsvAWS("CAIR Summary Report.csv")
  d[, date := as.Date(date)]
  d <- d[!(dose %in% c("INV", "UNK"))]
  d <- d[vax_type %in% c("Moderna", "Pfizer", "Johnson & Johnson")]
  d[vax_type %in% c("Moderna", "Pfizer"), dose_num := dose]
  d[vax_type == "Johnson & Johnson", dose_num := "J"]
  d[, count := count_65plus + count_under65]

  if (print_outdate) {
    cat("at least one dose:\n")
    under65 <- d[dose_num %in% c("1", "J"), sum(count_under65)]
    over65 <- d[dose_num %in% c("1", "J"), sum(count_65plus)]
    total <- under65 + over65

    dt <- data.table(name = c("all", "12+", "12-64", "~16-64", "65+"),
                     numer = c(total, total, under65, under65, over65),
                     denom = c(833401, 759567, 629430, 606341, 132892)) #for 65+ my file uses 130137 but Ted has 132892
    dt[, frac_vax := numer / denom]
    print(dt, digits = 3)
  }

  d <- d[, .(count = sum(count)), keyby = c("date", "dose_num")]
  d <- d[, .(dose1 = sum(count * (dose_num == "1")), dose2 = sum(count * (dose_num == "2")), doseJ = sum(count * (dose_num == "J"))), by = "date"]
  d[, county := "San Francisco"]
  return(d)
}
