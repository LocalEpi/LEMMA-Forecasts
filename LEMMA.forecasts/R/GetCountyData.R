# --------------------------------------------------------------------------------
#   Functions to get county data:
#   1. GetCountyData
#   2. GetStateData
#   3. ReadCsvAWS
#   4. ConvertNegative
#   5. GetAdmits
# --------------------------------------------------------------------------------


#' @title Get data for counties
#' @param include.regions include regions in addition to counties
#' @param remove.holidays remove major holidays
#' @param states get data for states
#' @param remote a logical value, if \code{TRUE} download all data from remotes, otherwise use local data
#' @return a \code{\link[data.table]{data.table}} object \code{county.dt}
#' @export
GetCountyData <- function(include.regions = FALSE, remove.holidays = TRUE, states = FALSE, remote = FALSE) {
  if (states) return(GetStateData(remove.holidays))
  dt <- data.table::fread("https://data.chhs.ca.gov/dataset/2df3e19e-9ee4-42a6-a087-9761f82033f6/resource/47af979d-8685-4981-bced-96a6b79d3ed5/download/covid19hospitalbycounty.csv")
  dt <- dt[, .(county, date = as.Date(todays_date), hosp.conf = hospitalized_covid_confirmed_patients, hosp.pui = hospitalized_suspected_covid_patients, icu.conf = icu_covid_confirmed_patients, icu.pui = icu_suspected_covid_patients)]

  if (T) {
    deaths.cases <- data.table::fread("https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv")
    deaths.cases[, date := as.Date(date)]
    deaths.cases[, county := area]
    deaths.cases <- deaths.cases[!(county %in% c("Unknown", "Out of state", "California")) & !is.na(date)]
    data.table::setkey(deaths.cases, county, date)

    #assume last 3 days of cases, 30 days of deaths are not reliable
    max.date <- deaths.cases[, max(date)]
    case.dt <- deaths.cases[date >= as.Date("2020/9/25") & date < (max.date - 3), .(date, county, cases.conf = cases, cases.pui = NA_real_)]

    if (remove.holidays) {
      case.dt[date %in% as.Date(c("2020/11/26", "2020/11/27", "2020/12/25", "2021/1/1")), cases.conf := NA_real_] #remove major holidays before and after data.table::frollmean
    }

    case.dt[, cases.conf := data.table::frollmean(cases.conf, 7, na.rm = T), by = county]

    if (remove.holidays) {
      case.dt[date %in% as.Date(c("2020/11/26", "2020/11/27", "2020/12/25", "2021/1/1")), cases.conf := NA_real_] #remove major holidays before and after data.table::frollmean
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

  admits.dt <- GetAdmits(remote = remote)
  county.dt <- merge(county.dt, admits.dt, all = T, by = c("county", "date"))

  seroprev.dt <- ReadCsvAWS("countySP_ts.csv")
  seroprev.dt[, county := sub(" County", "", county)]
  seroprev.dt[, year := substr(as.character(end_4wk), 1, 4)]
  seroprev.dt[, week := substr(as.character(end_4wk), 5, 6)]
  seroprev.dt[, date := as.Date(paste0(year, "/1/1")) + as.numeric(week) * 7 - 14] #subtract two weeks to get middle of 4 week period
  seroprev.dt <- seroprev.dt[, .(date, county, seroprev.conf = reweightedSP, seroprev.pui = NA_real_)]

  county.dt <- merge(county.dt, seroprev.dt, by = c("date", "county"), all = T)

  always.include <- c("Marin") #run these even if hosp < 1
  county.dt <- county.dt[date >= as.Date("2020/3/1")]
  county.dt[, mean10 := mean(hosp.conf[date >= (Sys.Date() - 10)], na.rm=T), by = "county"]
  county.dt <- county.dt[(mean10 > 1) | (county %in% always.include)] #exclude if average hosp over last 10 days < 1
  county.dt$mean10 <- NULL

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

  data.table::setkey(county.dt, county, date)
  return(county.dt)
}


#' @title Get state data
#' @param remove.holidays remove major holidays?
GetStateData <- function(remove.holidays = TRUE) {
  GetHospData <- function(f) {
    x <- data.table::fread(f)
    if ("reporting_cutoff_start" %in% names(x)) {
      #used in daily updates, not in time series
      x[, date := as.Date(reporting_cutoff_start) + 3]
    } else {
      x[, date := as.Date(date)]
    }
    data.table::setkey(x, state, date)
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
    hosp.dt.prev <- GetHospData("https://healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD")[date > as.Date("2020-08-01")] #time series
    cat("last date may be duplicated - check\n")
    print(tail(hosp.dt.prev[state == "MI"]))
    ar <- data.table::fread("https://healthdata.gov/api/views/4cnb-m4rz/rows.csv?accessType=DOWNLOAD")
    ar[, date := as.Date(ar$`Update Date`, format = "%m/%d/%Y")]
    for (i in ar[date > as.Date("2021-05-01"), `Archive Link`]) {
      hosp.dt.temp <- GetHospData(i)
      stopifnot(uniqueN(hosp.dt.temp$date) == 1)
      new.date <- unique(hosp.dt.temp$date)
      if (!(new.date %in% hosp.dt.prev$date)) {
        print(new.date)
        hosp.dt.prev <- rbind(hosp.dt.prev, hosp.dt.temp)
      }
    }
    ctp.dt <- data.table::fread("https://covidtracking.com/data/download/all-states-history.csv")
    ctp.dt[, date := as.Date(date)]
    ctp.dt <- ctp.dt[date <= as.Date("2020-08-01"), .(state, date, hosp.conf = hospitalizedCurrently, hosp.pui = ifelse(is.na(hospitalizedCurrently), NA_real_, 0), icu.conf = inIcuCurrently, icu.pui = ifelse(is.na(inIcuCurrently), NA_real_, 0), admits.conf = hospitalizedIncrease, admits.pui = ifelse(is.na(hospitalizedIncrease), NA_real_, 0))]
    ctp.dt[, index := admits.conf == 0]
    ctp.dt[index == T, admits.conf := NA_real_]
    ctp.dt[index == T, admits.pui := NA_real_]
    ctp.dt$index <- NULL
    hosp.dt.prev <- rbind(ctp.dt, hosp.dt.prev)
    data.table::setkey(hosp.dt.prev, state, date)
    saveRDS(hosp.dt.prev, "Inputs/StateHosp.rds")
  }
  hosp.dt.prev <- readRDS("Inputs/StateHosp.rds")
  stopifnot(uniqueN(hosp.dt$date) == 1)
  new.date <- unique(hosp.dt$date)

  if (new.date %in% hosp.dt.prev$date) {
    hosp.dt <- hosp.dt.prev
  } else {
    hosp.dt <- rbind(hosp.dt.prev, hosp.dt)
    data.table::setkey(hosp.dt, state, date)
    saveRDS(hosp.dt, "Inputs/StateHosp.rds")
  }


  state.abbr.dt <- data.table::fread("Inputs/state abbreviations.csv")
  setnames(state.abbr.dt, c("StateName", "state"))

  if (F) {
    #these seem unreliable
    s=data.table::fread("https://data.cdc.gov/api/views/d2tw-32xv/rows.csv?accessType=DOWNLOAD")
    #seroprev - may be mix of infected only and infected or vaccinated - only use up to Dec 2020
    z <- s[, strsplit(`Date Range of Specimen Collection`, "-")]
    s$date <- as.Date(sapply(z, function (z1) z1[[2]]), format = " %b %d, %Y")
    s <- s[date <= as.Date("2020/12/31") & `Catchment Area Description` == "Statewide", .(state = Site, date, seroprev.conf = `Rate (%) [Cumulative Prevalence]` / 100)]
  }


  #deaths (with date of death - NCHS data - see https://covidtracking.com/analysis-updates/federal-covid-data-101-working-with-death-numbers)
  x <- data.table::fread("https://data.cdc.gov/api/views/r8kw-7aab/rows.csv?accessType=DOWNLOAD")
  x <- x[Group == "By Week", .(StateName = State, date = as.Date(`End Date`, format = "%m/%d/%Y"), newdeaths = `COVID-19 Deaths`)]
  x[is.na(newdeaths), newdeaths := 5] #1-9 deaths is NA, estimate with 5
  x <- merge(x, state.abbr.dt)
  x[, deaths.conf := cumsum(newdeaths), by = "state"]
  death.dt <- x[, .(state, date, deaths.conf, deaths.pui = NA_real_)]
  death.dt[date > (max(date) - 30), deaths.conf := NA_real_] #assume last 30 days not reliable
  data.table::setkey(death.dt, state, date)

  #cases
  x <- data.table::fread("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD")
  x[, date := as.Date(submission_date, "%m/%d/%Y")]
  data.table::setkey(x, state, date)
  x[, cases.conf := as.numeric(new_case - ifelse(is.na(pnew_case), 0, pnew_case))]
  x[, cases.pui := as.numeric(pnew_case)]

  x[, cases.invalid := (cases.conf < 0) | (cases.pui < 0)]
  x[cases.invalid == T, cases.conf := NA_real_]
  x[cases.invalid == T, cases.pui := NA_real_]

  if (remove.holidays) {
    x[date %in% as.Date(c("2020/11/26", "2020/11/27", "2020/12/25", "2021/1/1")), cases.conf := NA_real_] #remove major holidays before and after data.table::frollmean
    x[date %in% as.Date(c("2020/11/26", "2020/11/27", "2020/12/25", "2021/1/1")), cases.pui := NA_real_]
  }

  x[, cases.conf := data.table::frollmean(cases.conf, 7, na.rm = T), by = state]
  x[, cases.pui := data.table::frollmean(cases.pui, 7, na.rm = T), by = state]

  if (remove.holidays) {
    x[date %in% as.Date(c("2020/11/26", "2020/11/27", "2020/12/25", "2021/1/1")), cases.conf := NA_real_] #remove major holidays before and after data.table::frollmean
    x[date %in% as.Date(c("2020/11/26", "2020/11/27", "2020/12/25", "2021/1/1")), cases.pui := NA_real_]
  }
  case.dt <- x[date >= as.Date("2020/9/25"), .(state, date, cases.conf, cases.pui, seroprev.conf = NA_real_, seroprev.pui= NA_real_)]
  state.dt <- merge(hosp.dt, case.dt, all = T)
  state.dt <- merge(state.dt, death.dt, all = T)
  state.dt <- state.dt[state %in% state.abbr.dt$state]

  state.dt[admits.conf < 0, admits.pui := NA_real_]
  state.dt[admits.conf < 0, admits.conf := NA_real_]
  return(state.dt)
}


#' @title Get data from AWS
#' @param object name of the object to retrieve
ReadCsvAWS <- function(object) {
  filestr <- tempfile()
  aws.s3::save_object(object, bucket = "js-lemma-bucket1", file = filestr, region = "us-west-1")
  csv <- data.table::fread(filestr)
  value <- unlink(filestr)
  if (!identical(value, 0L)) stop("failed to delete temp file")
  return(csv)
}


#' @title Set negative values to 2
#' @param value a numeric value
ConvertNegative <- function(value) {
  #-999999 in admits data means 1 2 or 3 => use 2
  stopifnot(all(value >= 0 | is.na(value) | value == -999999))
  value[value == -999999] <- 2
  return(value)
}

#' @title Get information on hospital admissions
#' @param remote a logical value, if \code{TRUE} download all data from remotes, otherwise use local data
GetAdmits <- function(remote = FALSE) {
  admits.dt <- data.table::fread("https://healthdata.gov/api/views/anag-cw7u/rows.csv?accessType=DOWNLOAD")[state == "CA"]
  max.week <- admits.dt[, max(as.Date(collection_week))]
  admits.dt <- admits.dt[collection_week != max.week] #starting with collection_week = 2021-05-07, last update seems incomplete? (this should be week starting May 7 and including data through May 16 but was posted May 9) - for now omit last week but need to check this after next update - will collection_week = 2021-05-07 be corrected?

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
  if (remote) {
    admits.dt2 <- merge(admits.dt2, data.table::fread("https://raw.githubusercontent.com/LocalEpi/LEMMA-Forecasts/master/Inputs/CountyFips.csv"))
  } else {
    admits.dt2 <- merge(admits.dt2, data.table::fread("Inputs/CountyFips.csv"))
  }
  admits.dt2[is.na(admits.conf), admits.pui := NA_real_]
  admits.dt2[is.na(admits.pui), admits.conf := NA_real_]

  if (remote) {
    admits.old <- readRDS(file = url("https://github.com/LocalEpi/LEMMA-Forecasts/raw/master/Inputs/AdmitsUntilFeb19.rds"))
  } else {
    admits.old <- readRDS("Inputs/AdmitsUntilFeb19.rds")
  }

  admits.dt2 <- rbind(admits.old[, .(county, date, admits.conf, admits.pui)], admits.dt2[date > as.Date("2021/2/22"), .(county, date, admits.conf, admits.pui)]) #some overlap between sources
  data.table::setkey(admits.dt2, county, date)
  return(admits.dt2)
}

