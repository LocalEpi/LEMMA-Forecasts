GetCountyData <- function(exclude.set) {
  dt <- fread("https://data.ca.gov/dataset/529ac907-6ba1-4cb7-9aae-8966fc96aeef/resource/42d33765-20fd-44b8-a978-b083b7542225/download/hospitals_by_county.csv")
  #dt <- dt[todays_date != "", .(county, date = as.Date(todays_date), hosp.conf = hospitalized_covid_confirmed_patients, hosp.pui = hospitalized_suspected_covid_patients, icu.conf = icu_covid_confirmed_patients, icu.pui = icu_suspected_covid_patients)]
  dt <- dt[, .(county, date = as.Date(todays_date), hosp.conf = hospitalized_covid_confirmed_patients, hosp.pui = hospitalized_suspected_covid_patients, icu.conf = icu_covid_confirmed_patients, icu.pui = icu_suspected_covid_patients)]

  dt <- dt[date >= as.Date("2020/4/1"), .(county, date, hosp.conf, hosp.pui, icu.conf, icu.pui)]

  deaths <- fread("https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv")
  deaths <- deaths[, .(county, date = as.Date(date), deaths.conf = as.numeric(totalcountdeaths))]
  county.dt <- merge(dt, deaths, all = T, keyby = c("county", "date"))

  county.dt[, mean10 := mean(hosp.conf[date >= (Sys.Date() - 10)], na.rm=T), by = "county"]
  county.dt <- county.dt[mean10 > 1] #exclude if average hosp over last 10 days < 1
  county.dt <- county.dt[!(county %in% exclude.set)]
  county.dt$mean10 <- NULL

  #make deaths NA for outliers that imply cumulative deaths decrease
  county.dt[county == 'Alameda' & date == '2020-08-22', deaths.conf := NA_real_]
  county.dt[county == 'Alameda' & date == '2020-10-04', deaths.conf := NA_real_]
  county.dt[county == 'Alameda' & date == '2020-10-11', deaths.conf := NA_real_]
  county.dt[county == 'Alameda' & date == '2020-10-12', deaths.conf := NA_real_]
  county.dt[county == 'Alameda' & date == '2020-10-13', deaths.conf := NA_real_]
  county.dt[county == 'Alameda' & date == '2020-10-15', deaths.conf := NA_real_]
  county.dt[county == 'Alameda' & date == '2020-10-18', deaths.conf := NA_real_]
  county.dt[county == 'Alameda' & date == '2020-10-20', deaths.conf := NA_real_]
  county.dt[county == 'Amador' & date == '2020-08-31', deaths.conf := NA_real_]
  county.dt[county == 'Amador' & date == '2020-09-06', deaths.conf := NA_real_]
  county.dt[county == 'Contra Costa' & date == '2020-03-24', deaths.conf := NA_real_]
  county.dt[county == 'Contra Costa' & date == '2020-03-25', deaths.conf := NA_real_]
  county.dt[county == 'Contra Costa' & date == '2020-05-10', deaths.conf := NA_real_]
  county.dt[county == 'Kern' & date == '2020-09-03', deaths.conf := NA_real_]
  county.dt[county == 'Los Angeles' & date == '2020-03-25', deaths.conf := NA_real_]
  county.dt[county == 'Marin' & date == '2020-08-27', deaths.conf := NA_real_]
  county.dt[county == 'Marin' & date == '2020-10-27', deaths.conf := NA_real_]
  county.dt[county == 'Mendocino' & date == '2020-09-11', deaths.conf := NA_real_]
  county.dt[county == 'Monterey' & date == '2020-10-26', deaths.conf := NA_real_]
  county.dt[county == 'Orange' & date == '2020-04-10', deaths.conf := NA_real_]
  county.dt[county == 'Orange' & date == '2020-07-19', deaths.conf := NA_real_]
  county.dt[county == 'Orange' & date == '2020-08-10', deaths.conf := NA_real_]
  county.dt[county == 'Sacramento' & date == '2020-03-21', deaths.conf := NA_real_]
  county.dt[county == 'Sacramento' & date == '2020-06-17', deaths.conf := NA_real_]
  county.dt[county == 'Sacramento' & date == '2020-06-18', deaths.conf := NA_real_]
  county.dt[county == 'Sacramento' & date == '2020-06-19', deaths.conf := NA_real_]
  county.dt[county == 'Sacramento' & date == '2020-06-20', deaths.conf := NA_real_]
  county.dt[county == 'Sacramento' & date == '2020-06-21', deaths.conf := NA_real_]
  county.dt[county == 'San Bernardino' & date == '2020-04-10', deaths.conf := NA_real_]
  county.dt[county == 'San Bernardino' & date == '2020-07-30', deaths.conf := NA_real_]
  county.dt[county == 'San Joaquin' & date == '2020-09-28', deaths.conf := NA_real_]
  county.dt[county == 'San Luis Obispo' & date == '2020-08-31', deaths.conf := NA_real_]
  county.dt[county == 'San Francisco' & date == '2020-03-26', deaths.conf := NA_real_]
  county.dt[county == 'San Mateo' & date == '2020-03-30', deaths.conf := NA_real_]
  county.dt[county == 'San Mateo' & date == '2020-04-10', deaths.conf := NA_real_]
  county.dt[county == 'Santa Barbara' & date == '2020-04-10', deaths.conf := NA_real_]
  county.dt[county == 'Santa Barbara' & date == '2020-07-08', deaths.conf := NA_real_]
  county.dt[county == 'Santa Clara' & date == '2020-03-31', deaths.conf := NA_real_]
  county.dt[county == 'Santa Clara' & date == '2020-08-29', deaths.conf := NA_real_]
  county.dt[county == 'Santa Cruz' & date == '2020-06-18', deaths.conf := NA_real_]
  county.dt[county == 'Shasta' & date == '2020-06-11', deaths.conf := NA_real_]
  county.dt[county == 'Shasta' & date == '2020-06-12', deaths.conf := NA_real_]
  county.dt[county == 'Shasta' & date == '2020-06-13', deaths.conf := NA_real_]
  county.dt[county == 'Shasta' & date == '2020-06-14', deaths.conf := NA_real_]
  county.dt[county == 'Stanislaus' & date == '2020-06-20', deaths.conf := NA_real_]
  county.dt[county == 'Stanislaus' & date == '2020-06-21', deaths.conf := NA_real_]
  county.dt[county == 'Sonoma' & date == '2020-05-12', deaths.conf := NA_real_]
  county.dt[county == 'Yolo' & date == '2020-04-06', deaths.conf := NA_real_]
  county.dt[county == 'Yolo' & date == '2020-04-08', deaths.conf := NA_real_]
  county.dt[county == 'Yolo' & date == '2020-04-09', deaths.conf := NA_real_]
  county.dt[county == 'Yuba' & date == '2020-06-16', deaths.conf := NA_real_]
  county.dt[county == 'Yuba' & date == '2020-06-17', deaths.conf := NA_real_]
  county.dt[county == 'Yuba' & date == '2020-06-18', deaths.conf := NA_real_]

  #data errors
  county.dt[county == 'Merced' & date == '2020-08-16', hosp.conf := NA_real_]
  county.dt[county == 'Merced' & date == '2020-08-16', hosp.pui := NA_real_]
  county.dt[county == 'Merced' & date == '2020-08-16', icu.conf := NA_real_]
  county.dt[county == 'Merced' & date == '2020-08-16', icu.pui := NA_real_]

  #check for new problems with death reporting
  for (county1 in unique(county.dt$county)) {
    dt <- county.dt[county == county1]
    dt[, deaths.inc := c(NA, diff(deaths.conf))]
    index <- which(dt[, deaths.inc < 0])
    for (index1 in index) {
      index2 <- seq(pmax(1, index1-5),pmin(nrow(dt), index1+5))
      print(dt[index2, .(county, date, deaths.conf, deaths.inc)])
      d <- dt[index1, date]
      cat("county.dt[county == '", county1, "' & date == '", as.character(d), "', deaths.conf := NA_real_]\n", sep = "")
    }
  }
  return(county.dt)
}
