GetCountyData <- function(include.regions = FALSE, remove.holidays = TRUE) {
  dt <- fread("https://data.chhs.ca.gov/dataset/2df3e19e-9ee4-42a6-a087-9761f82033f6/resource/47af979d-8685-4981-bced-96a6b79d3ed5/download/covid19hospitalbycounty.csv")
  dt <- dt[, .(county, date = as.Date(todays_date), hosp.conf = hospitalized_covid_confirmed_patients, hosp.pui = hospitalized_suspected_covid_patients, icu.conf = icu_covid_confirmed_patients, icu.pui = icu_suspected_covid_patients)]

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
  county.dt <- merge(dt, deaths.dt, all = T, by = c("county", "date"))
  county.dt <- merge(county.dt, case.dt, all = T, by = c("county", "date"))

  admits.dt <- GetAdmits()
  county.dt <- merge(county.dt, admits.dt, all = T, by = c("county", "date"))


  county.dt <- county.dt[date >= as.Date("2020/3/1")]
  county.dt[, mean10 := mean(hosp.conf[date >= (Sys.Date() - 10)], na.rm=T), by = "county"]
  county.dt <- county.dt[mean10 > 1] #exclude if average hosp over last 10 days < 1
  county.dt$mean10 <- NULL

  county.pop <- data.table::fread("Inputs/county population.csv")
  county.dt <- merge(county.dt, county.pop, by = "county")

  #Imperial has weird hosp data for a few days late March
  county.dt[date < as.Date("2020/4/1") & county == "Imperial", hosp.conf := NA]
  county.dt[date < as.Date("2020/4/1") & county == "Imperial", hosp.pui := NA]
  #Imperial has early deaths that may be correct but cause lots of problems in LEMMA
  county.dt[date < as.Date("2020/4/1") & county == "Imperial", deaths.conf := NA]

  #San Bernardino has weird admit data for 3 weeks
  county.dt[date < as.Date("2020-08-24") & county == "San Bernardino", admits.conf := NA]
  county.dt[date < as.Date("2020-08-24") & county == "San Bernardino", admits.pui := NA]


  county.dt[, is.region := F]

  county.dt[county == "Nevada" & date=="2020-03-29", hosp.conf := NA_real_]
  county.dt[county == "Inyo" & date=="2020-03-31", hosp.pui := NA_real_]
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
  admits.dt <- fread("https://opendata.arcgis.com/datasets/cebaea39dc3b4a4d858105a170318731_0.csv")[state == "CA"]
  admits.dt[, previous_day_admission_adult_covid_confirmed_7_day_sum := ConvertNegative(prevadmit_adult_conf_7d_sum)]
  admits.dt[, previous_day_admission_pediatric_covid_confirmed_7_day_sum := ConvertNegative(prevadmit_pedi_conf_7d_sum)]
  admits.dt[, previous_day_admission_adult_covid_suspected_7_day_sum := ConvertNegative(prevadmit_adult_susp_7d_sum)]
  admits.dt[, previous_day_admission_pediatric_covid_suspected_7_day_sum := ConvertNegative(prevadmit_pedi_susp_7d_sum)]
  admits.dt2 <- admits.dt[, .(admits.conf = sum(previous_day_admission_adult_covid_confirmed_7_day_sum + previous_day_admission_pediatric_covid_confirmed_7_day_sum) / 7,
                              admits.pui = sum(previous_day_admission_adult_covid_suspected_7_day_sum + previous_day_admission_pediatric_covid_suspected_7_day_sum) / 7),
                          by = c("fips_code", "collection_week")]
  admits.dt2[, date := as.Date(collection_week) + 3] #add 3 for midpoint of week
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

GetRunTime <- function(county1) {
  filestr <- paste0("Logs/progress-", county1, ".txt")
  time.num <- NA
  if (file.exists(filestr)) {
    log.str <- readLines(filestr)
    index <- grep("elapsed", log.str)
    if (length(index) >= 1) {
      index <- min(index) + 1
      time.num <- as.numeric(strsplit(trimws(log.str[index]), " ")[[1]])
      time.num <- time.num[length(time.num)]
    }
  }
  if (is.na(time.num)) {
    time.num <- 100000
  }
  if (county1 %in% c("Yuba")) { #temp
    time.num <- time.num + 1
  }
  return(time.num)
}


IsBad <- function(w) {
  if (w %in% c("Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable. Running the chains for more iterations may help. See http://mc-stan.org/misc/warnings.html#bulk-ess",
               "Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable. Running the chains for more iterations may help. See http://mc-stan.org/misc/warnings.html#tail-ess",
               "Examine the pairs() plot to diagnose sampling problems")) return(F)
  if (grepl("There were [[:digit:]]+ divergent transitions after warmup.", w)) {
    x <- as.numeric(strsplit(sub("There were ", "", w), split = " divergent")[[1]][1])
    return(x > 50)
  }
  if (grepl("There were [[:digit:]]+ transitions after warmup that exceeded the maximum treedepth.", w)) {
    x <- as.numeric(strsplit(sub("There were ", "", w), split = " transitions")[[1]][1])
    return(x > 50)
  }
  return(T)
}


