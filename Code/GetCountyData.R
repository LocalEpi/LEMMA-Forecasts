GetCountyData <- function(include.regions = FALSE, remove.holidays = TRUE, states = FALSE) {
  if (states) return(GetStateData(remove.holidays))
  dt <- fread("https://data.chhs.ca.gov/dataset/2df3e19e-9ee4-42a6-a087-9761f82033f6/resource/47af979d-8685-4981-bced-96a6b79d3ed5/download/covid19hospitalbycounty.csv")
  dt <- dt[, .(county, date = as.Date(todays_date), hosp.conf = hospitalized_covid_confirmed_patients, hosp.pui = hospitalized_suspected_covid_patients, icu.conf = icu_covid_confirmed_patients, icu.pui = icu_suspected_covid_patients)]

  if (T) {
    deaths.cases <- fread("https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv")
    deaths.cases[, date := as.Date(date)]
    deaths.cases[, county := area]
    deaths.cases <- deaths.cases[!(county %in% c("Unknown", "Out of state", "California")) & !is.na(date)]
    setkey(deaths.cases, county, date)

    #assume last 3 days of cases, 30 days of deaths are not reliable
    max.date <- deaths.cases[, max(date)]
    case.dt <- deaths.cases[date >= as.Date("2020/9/25") & date < (max.date - 3), .(date, county, cases.conf = cases, cases.pui = NA_real_)]

    if (remove.holidays) {
      case.dt[date %in% as.Date(c("2020/11/26", "2020/11/27", "2020/12/25", "2021/1/1")), cases.conf := NA_real_] #remove major holidays before and after frollmean
    }

    case.dt[, cases.conf := frollmean(cases.conf, 7, na.rm = T), by = county]

    if (remove.holidays) {
      case.dt[date %in% as.Date(c("2020/11/26", "2020/11/27", "2020/12/25", "2021/1/1")), cases.conf := NA_real_] #remove major holidays before and after frollmean
    }

    deaths.dt <- deaths.cases[date <= (max.date - 30), .(date, deaths.conf = cumsum(deaths), deaths.pui = NA_real_), by = "county"]
  } else {
    #CHHS server is down
    cat("Using saved cases and deaths!\n")
    prev <- readRDS("Inputs/savedCountyData.rds")
    case.dt <- prev[, .(date, county, cases.conf, cases.pui)]
    sf.cases <- c(32, 33, 34, 36, 39, 38, 36, 36, 38, 37)
    case.dt[county == "San Francisco" & date >= as.Date("2021-03-21") & date <= (as.Date("2021-03-21") + length(sf.cases) - 1), cases.conf := sf.cases]
    deaths.dt <- prev[, .(date, county, deaths.conf, deaths.pui)]
  }
  county.dt <- merge(dt, deaths.dt, all = T, by = c("county", "date"))
  county.dt <- merge(county.dt, case.dt, all = T, by = c("county", "date"))

  admits.dt <- GetAdmits()
  county.dt <- merge(county.dt, admits.dt, all = T, by = c("county", "date"))

  seroprev.dt <- ReadCsvAWS("countySP_ts.csv")
  seroprev.dt[, county := sub(" County", "", county)]
  seroprev.dt[, year := substr(as.character(end_4wk), 1, 4)]
  seroprev.dt[, week := substr(as.character(end_4wk), 5, 6)]
  seroprev.dt[, date := as.Date(paste0(year, "/1/1")) + as.numeric(week) * 7 - 14] #subtract two weeks to get middle of 4 week period
  seroprev.dt <- seroprev.dt[, .(date, county, seroprev.conf = reweightedSP, seroprev.pui = NA_real_)]

  county.dt <- merge(county.dt, seroprev.dt, by = c("date", "county"), all = T)

  county.dt <- county.dt[date >= as.Date("2020/3/1")]
  county.dt[, mean10 := mean(hosp.conf[date >= (Sys.Date() - 10)], na.rm=T), by = "county"]
  county.dt <- county.dt[mean10 > 1] #exclude if average hosp over last 10 days < 1
  county.dt$mean10 <- NULL

  county.pop <- data.table::fread("Inputs/county population.csv")
  county.dt <- merge(county.dt, county.pop, by = "county")

  county.dt <- county.dt[!(county %in% c("Out Of Country", "Unassigned", "Unknown"))]


  #Imperial has weird hosp data for a few days late March
  county.dt[date < as.Date("2020/4/1") & county == "Imperial", hosp.conf := NA]
  county.dt[date < as.Date("2020/4/1") & county == "Imperial", hosp.pui := NA]
  #Imperial has early deaths that may be correct but cause lots of problems in LEMMA
  county.dt[date < as.Date("2020/4/1") & county == "Imperial", deaths.conf := NA]

  #San Bernardino has weird admit data for 3 weeks
  county.dt[date < as.Date("2020-08-24") & county == "San Bernardino", admits.conf := NA]
  county.dt[date < as.Date("2020-08-24") & county == "San Bernardino", admits.pui := NA]

  #Misc data problems
  county.dt[county == "Nevada" & date=="2020-03-29", hosp.conf := NA_real_]
  county.dt[county == "Inyo" & date=="2020-03-31", hosp.pui := NA_real_]

  county.dt[, is.region := F]
  if (include.regions) {
    regions <- list(BayArea = c("Alameda", "Contra Costa", "Marin", "Monterey", "Napa", "San Francisco", "San Mateo", "Santa Clara", "Santa Cruz", "Solano", "Sonoma"),
                    GreaterSacramento = c("Alpine", "Amador", "Butte", "Colusa", "El Dorado", "Nevada",
                                          "Placer", "Plumas", "Sacramento", "Sierra", "Sutter", "Yolo",
                                          "Yuba"),
                    SanJoaquinValley = c("Calaveras", "Fresno", "Kern", "Kings", "Madera", "Mariposa",
                                         "Merced", "San Benito", "San Joaquin", "Stanislaus", "Tulare",
                                         "Tuolumne"),
                    SouthernCalifornia = c("Imperial", "Inyo", "Los Angeles", "Mono", "Orange", "Riverside",
                                           "San Bernardino", "San Diego", "San Luis Obispo", "Santa Barbara",
                                           "Ventura"))

    for (reg in names(regions)) {
      region.dt <- county.dt[county %in% regions[[reg]], lapply(.SD, sum), by = "date", .SDcols = -c("county", "is.region")]
      region.dt[, county := reg]
      region.dt[, is.region := T]
      county.dt <- rbind(county.dt, region.dt)
    }
  }

  setkey(county.dt, county, date)
  return(county.dt)
}


GetStateData <- function(remove.holidays = TRUE) {
  GetHospData <- function(f) {
    x <- fread(f)
    if ("reporting_cutoff_start" %in% names(x)) {
      #used in daily updates, not in time series
      x[, date := as.Date(reporting_cutoff_start) + 3]
    } else {
      x[, date := as.Date(date)]
    }
    setkey(x, state, date)
    hosp.dt <- x[, .(state, date,
                     hosp.conf = total_adult_patients_hospitalized_confirmed_covid + total_pediatric_patients_hospitalized_confirmed_covid,
                     hosp.pui = pmax(0, total_adult_patients_hospitalized_confirmed_and_suspected_covid +
                                       total_pediatric_patients_hospitalized_confirmed_and_suspected_covid -
                                       total_adult_patients_hospitalized_confirmed_covid - total_pediatric_patients_hospitalized_confirmed_covid),
                     icu.conf = staffed_icu_adult_patients_confirmed_covid,
                     icu.pui = pmax(0, staffed_icu_adult_patients_confirmed_and_suspected_covid - staffed_icu_adult_patients_confirmed_covid),
                     admits.conf = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed,
                     admits.pui = previous_day_admission_adult_covid_suspected + previous_day_admission_pediatric_covid_suspected)]
    return(hosp.dt)
  }

  hosp.dt <- GetHospData("https://healthdata.gov/api/views/6xf2-c3ie/rows.csv?accessType=DOWNLOAD") #daily, by state
  if (F) {
    hosp.dt.prev <- GetHospData("https://healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD")[date > as.Date("2020-08-01") & date != as.Date("2021-03-27")] #time series #2021-03-27 is duplicate of 3-26
    ar <- fread("https://healthdata.gov/api/views/4cnb-m4rz/rows.csv?accessType=DOWNLOAD")
    ar[, date := as.Date(ar$`Update Date`, format = "%m/%d/%Y")]
    for (i in ar[date > as.Date("2021-03-15"), `Archive Link`]) {
      hosp.dt.temp <- GetHospData(i)
      stopifnot(uniqueN(hosp.dt.temp$date) == 1)
      new.date <- unique(hosp.dt.temp$date)
      if (!(new.date %in% hosp.dt.prev$date)) {
        print(new.date)
        hosp.dt.prev <- rbind(hosp.dt.prev, hosp.dt.temp)
      }
    }
    ctp.dt <- fread("https://covidtracking.com/data/download/all-states-history.csv")
    ctp.dt[, date := as.Date(date)]
    ctp.dt <- ctp.dt[date <= as.Date("2020-08-01"), .(state, date, hosp.conf = hospitalizedCurrently, hosp.pui = ifelse(is.na(hospitalizedCurrently), NA_real_, 0), icu.conf = inIcuCurrently, icu.pui = ifelse(is.na(inIcuCurrently), NA_real_, 0), admits.conf = hospitalizedIncrease, admits.pui = ifelse(is.na(hospitalizedIncrease), NA_real_, 0))]
    ctp.dt[, index := admits.conf == 0]
    ctp.dt[index == T, admits.conf := NA_real_]
    ctp.dt[index == T, admits.pui := NA_real_]
    ctp.dt$index <- NULL
    hosp.dt.prev <- rbind(ctp.dt, hosp.dt.prev)
    setkey(hosp.dt.prev, state, date)
    saveRDS(hosp.dt.prev, "Inputs/StateHosp.rds")
  }
  hosp.dt.prev <- readRDS("Inputs/StateHosp.rds")
  stopifnot(uniqueN(hosp.dt$date) == 1)
  new.date <- unique(hosp.dt$date)

  if (new.date %in% hosp.dt.prev$date) {
    hosp.dt <- hosp.dt.prev
  } else {
    hosp.dt <- rbind(hosp.dt.prev, hosp.dt)
    setkey(hosp.dt, state, date)
    saveRDS(hosp.dt, "Inputs/StateHosp.rds")
  }


  state.abbr.dt <- fread("Inputs/state abbreviations.csv")
  setnames(state.abbr.dt, c("StateName", "state"))

  if (F) {
    #these seem unreliable
    s=fread("https://data.cdc.gov/api/views/d2tw-32xv/rows.csv?accessType=DOWNLOAD")
    #seroprev - may be mix of infected only and infected or vaccinated - only use up to Dec 2020
    z <- s[, strsplit(`Date Range of Specimen Collection`, "-")]
    s$date <- as.Date(sapply(z, function (z1) z1[[2]]), format = " %b %d, %Y")
    s <- s[date <= as.Date("2020/12/31") & `Catchment Area Description` == "Statewide", .(state = Site, date, seroprev.conf = `Rate (%) [Cumulative Prevalence]` / 100)]
  }


  #deaths (with date of death - NCHS data - see https://covidtracking.com/analysis-updates/federal-covid-data-101-working-with-death-numbers)
  x <- fread("https://data.cdc.gov/api/views/r8kw-7aab/rows.csv?accessType=DOWNLOAD")
  x <- x[Group == "By Week", .(StateName = State, date = as.Date(`End Date`, format = "%m/%d/%Y"), newdeaths = `COVID-19 Deaths`)]
  x[is.na(newdeaths), newdeaths := 5] #1-9 deaths is NA, estimate with 5
  x <- merge(x, state.abbr.dt)
  x[, deaths.conf := cumsum(newdeaths), by = "state"]
  death.dt <- x[, .(state, date, deaths.conf, deaths.pui = NA_real_)]
  death.dt[date > (max(date) - 30), deaths.conf := NA_real_] #assume last 30 days not reliable
  setkey(death.dt, state, date)

  #cases
  x <- fread("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD")
  x[, date := as.Date(submission_date, "%m/%d/%Y")]
  setkey(x, state, date)
  x[, cases.conf := as.numeric(new_case - ifelse(is.na(pnew_case), 0, pnew_case))]
  x[, cases.pui := as.numeric(pnew_case)]

  if (remove.holidays) {
    x[date %in% as.Date(c("2020/11/26", "2020/11/27", "2020/12/25", "2021/1/1")), cases.conf := NA_real_] #remove major holidays before and after frollmean
    x[date %in% as.Date(c("2020/11/26", "2020/11/27", "2020/12/25", "2021/1/1")), cases.pui := NA_real_]
  }

  x[, cases.conf := frollmean(cases.conf, 7, na.rm = T), by = state]
  x[, cases.pui := frollmean(cases.pui, 7, na.rm = T), by = state]

  if (remove.holidays) {
    x[date %in% as.Date(c("2020/11/26", "2020/11/27", "2020/12/25", "2021/1/1")), cases.conf := NA_real_] #remove major holidays before and after frollmean
    x[date %in% as.Date(c("2020/11/26", "2020/11/27", "2020/12/25", "2021/1/1")), cases.pui := NA_real_]
  }
  case.dt <- x[date >= as.Date("2020/9/25"), .(state, date, cases.conf, cases.pui, seroprev.conf = NA_real_, seroprev.pui= NA_real_)]
  state.dt <- merge(hosp.dt, case.dt, all = T)
  state.dt <- merge(state.dt, death.dt, all = T)
  state.dt <- state.dt[state %in% state.abbr.dt$state]
  return(state.dt)
}

ReadCsvAWS <- function(object) {
  filestr <- tempfile()
  aws.s3::save_object(object, bucket = "js-lemma-bucket1", file = filestr, region = "us-west-1")
  csv <- fread(filestr)
  value <- unlink(filestr)
  if (!identical(value, 0L)) stop("failed to delete temp file")
  return(csv)
}

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

ReadSFDoses.old <- function(sheet) {
  d <- as.data.table(readxl::read_excel("~/Documents/MissionCovid/VaccineData_OverandUnder65_3.19.21.xlsx", sheet = sheet))
  setnames(d, c("date", "vax_type", "dose", "count"))
  d[, date := as.Date(date)]
  d <- d[!(dose %in% c("INV", "UNK"))]
  d <- d[vax_type %in% c("Moderna", "Pfizer", "J&J")]
  d[vax_type %in% c("Moderna", "Pfizer"), dose_num := dose]
  d[vax_type == "J&J", dose_num := "J"]
  d <- d[, .(count = sum(count)), keyby = c("date", "dose_num")]
  return(d)
}

SFDosesToAWS <- function() {
  suppressWarnings(d <- as.data.table(readxl::read_excel("~/Documents/MissionCovid/CAIR Summary Report.xlsx", col_types = c("date", "text", "text", "numeric", "numeric")))) #suppress numeric to date warnings
  setnames(d, c("date", "vax_type", "dose", "count_65plus", "count_under65"))
  write.csv(d, "~/Documents/MissionCovid/CAIR Summary Report.csv", row.names = F)
  aws.s3::put_object("~/Documents/MissionCovid/CAIR Summary Report.csv", bucket = "js-lemma-bucket1", region = "us-west-1")
}

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

GetDosesData.old <- function() {
  doses.dt <- ReadCsvAWS("VaccinesByCounty.csv")
  doses.dt[, date := as.Date(date)]
  doses.dt[, count := as.numeric(count)]

  doses.sf.under <- cbind(ReadSFDoses("Under 65 SF Residents"), age_bin = "<65")
  doses.sf.over <- cbind(ReadSFDoses("Over 65 SF Residents"), age_bin = "65+")
  doses.sf <- rbind(doses.sf.under, doses.sf.over)
  doses.sf[, county := "San Francisco"]
  doses.sf <- doses.sf[date <= (max(date) - 2)] #last few days incomplete

  doses.dt <- rbind(doses.dt[county != "San Francisco"], doses.sf)
  setkey(doses.dt, county, date)
  return(doses.dt)
}

GetDosesData <- function(states = FALSE) {
  if (states) {
    state.dt <- fread("Inputs/state abbreviations.csv")
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
  return(doses.dt)
}

ConvertNegative <- function(value) {
  #-999999 in admits data means 1 2 or 3 => use 2
  stopifnot(all(value >= 0 | is.na(value) | value == -999999))
  value[value == -999999] <- 2
  return(value)
}

GetOldAdmits <- function() {
  #also in JS code branch
  admits.dt <- fread("~/Documents/MissionCovid/reported_hospital_capacity_admissions_facility_level_weekly_average_timeseries_20210228.csv")[state == "CA"]

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
  saveRDS(admits.dt2, "Inputs/AdmitsUntilFeb19.rds")
}

GetAdmits <- function() {
  admits.dt <- fread("https://healthdata.gov/api/views/anag-cw7u/rows.csv?accessType=DOWNLOAD")[state == "CA"]
  admits.dt[, previous_day_admission_adult_covid_confirmed_7_day_sum := ConvertNegative(previous_day_admission_adult_covid_confirmed_7_day_sum)]
  admits.dt[, previous_day_admission_pediatric_covid_confirmed_7_day_sum := ConvertNegative(previous_day_admission_pediatric_covid_confirmed_7_day_sum)]
  admits.dt[, previous_day_admission_adult_covid_suspected_7_day_sum := ConvertNegative(previous_day_admission_adult_covid_suspected_7_day_sum)]
  admits.dt[, previous_day_admission_pediatric_covid_suspected_7_day_sum := ConvertNegative(previous_day_admission_pediatric_covid_suspected_7_day_sum)]
  admits.dt2 <- admits.dt[, .(admits.conf = sum(previous_day_admission_adult_covid_confirmed_7_day_sum + previous_day_admission_pediatric_covid_confirmed_7_day_sum) / 7,
                              admits.pui = sum(previous_day_admission_adult_covid_suspected_7_day_sum + previous_day_admission_pediatric_covid_suspected_7_day_sum) / 7),
                          by = c("fips_code", "collection_week")]
  admits.dt2[, date := as.Date(collection_week) + 3] #add 3 for midpoint of week (collection_week is start of week)
  admits.dt2 <- admits.dt2[date > as.Date("2021-2-1")] #data errors
  setnames(admits.dt2, "fips_code", "fips")
  admits.dt2 <- merge(admits.dt2, fread("Inputs/CountyFips.csv"))
  admits.dt2[is.na(admits.conf), admits.pui := NA_real_]
  admits.dt2[is.na(admits.pui), admits.conf := NA_real_]

  admits.old <- readRDS("Inputs/AdmitsUntilFeb19.rds")
  admits.dt2 <- rbind(admits.old[, .(county, date, admits.conf, admits.pui)], admits.dt2[date > as.Date("2021/2/22"), .(county, date, admits.conf, admits.pui)]) #some overlap between sources
  setkey(admits.dt2, county, date)
  return(admits.dt2)
}

GetAdmits_notused <- function() {
  #this data source hasn't updated since late feb
  admits.dt <- fread("https://opendata.arcgis.com/datasets/cebaea39dc3b4a4d858105a170318731_0.csv")[state == "CA"]
  admits.dt[, previous_day_admission_adult_covid_confirmed_7_day_sum := ConvertNegative(prevadmit_adult_conf_7d_sum)]
  admits.dt[, previous_day_admission_pediatric_covid_confirmed_7_day_sum := ConvertNegative(prevadmit_pedi_conf_7d_sum)]
  admits.dt[, previous_day_admission_adult_covid_suspected_7_day_sum := ConvertNegative(prevadmit_adult_susp_7d_sum)]
  admits.dt[, previous_day_admission_pediatric_covid_suspected_7_day_sum := ConvertNegative(prevadmit_pedi_susp_7d_sum)]
  admits.dt2 <- admits.dt[, .(admits.conf = sum(previous_day_admission_adult_covid_confirmed_7_day_sum + previous_day_admission_pediatric_covid_confirmed_7_day_sum) / 7,
                              admits.pui = sum(previous_day_admission_adult_covid_suspected_7_day_sum + previous_day_admission_pediatric_covid_suspected_7_day_sum) / 7),
                          by = c("fips_code", "collection_week")]
  admits.dt2[, date := as.Date(collection_week) + 3] #add 3 for midpoint of week (collection_week is start of week)
  admits.dt2 <- admits.dt2[date > as.Date("2021-2-1")] #data errors
  setnames(admits.dt2, "fips_code", "fips")
  admits.dt2 <- merge(admits.dt2, fread("Inputs/CountyFips.csv"))
  admits.dt2[is.na(admits.conf), admits.pui := NA_real_]
  admits.dt2[is.na(admits.pui), admits.conf := NA_real_]

  admits.old <- readRDS("Inputs/AdmitsUntilFeb19.rds")
  admits.dt2 <- rbind(admits.old[, .(county, date, admits.conf, admits.pui)], admits.dt2[, .(county, date, admits.conf, admits.pui)])
  setkey(admits.dt2, county, date)
  return(admits.dt2)
}

#not currently used
GetSantaClaraData <- function() {
  sc.deaths <- fread("https://data.sccgov.org/api/views/tg4j-23y2/rows.csv?accessType=DOWNLOAD")
  sc.deaths[, date := as.Date(Date)]
  sc.deaths <- sc.deaths[date <= Sys.Date()] #remove data errors
  sc.hosp <- fread("https://data.sccgov.org/api/views/5xkz-6esm/rows.csv?accessType=DOWNLOAD")
  sc.hosp <- sc.hosp[, .(date = as.Date(Date), icu_covid, icu_pui, non_icu_covid, non_icu_pui)]
  sc <- merge(sc.deaths[, .(Cumulative, date)], sc.hosp, all = T, by = "date")
  sc <- sc[date >= as.Date("2020/3/27"), .(county = "Santa Clara", date, hosp.conf = icu_covid + non_icu_covid, hosp.pui = icu_pui + non_icu_pui, icu.conf = icu_covid, icu.pui = icu_pui, deaths.conf = Cumulative)]
  stopifnot(all(sc$date <= Sys.Date()))
  sc[, is.region := F]
  pop <- data.table::fread("Inputs/county population.csv")[county == "Santa Clara", population]
  sc[, population := pop]
  return(sc)
}

