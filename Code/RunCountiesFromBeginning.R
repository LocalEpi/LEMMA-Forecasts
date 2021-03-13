#also output median and sd other posteriors, should we use these (e.g. frac_mort?); I think depends in part on if we want to fit to recent death rate or overall
#email Paul, Chris
#think about how to generate full fit if needed (e.g. save Rt quantiles up to restart date for plotting)
#think about when to move restart date and update cache - if restart is after vaccines start would need more categories
#document this change + sigmaobs fix + variants/vaccines - set a deadline and priorities
#clean up LEMMA code (GetStanInputs, LEMMA.stan) - check for any fixme/todo/temp
#plan for new spreadsheet format (caching + variants + vaccines)
#update RunCACounties-SF - have something to just add one line with Wayne's latest (use nonICU+ICU as input and convert to total) to the state data and UeS to cases


library(matrixStats)
library(data.table)
# setwd("~/Documents/GitHub/LEMMA-Forecasts/")

source('Code/GetInputsVaxRestart.R')
source('Code/GetVaccineParams.R')


GetVaccineParamsForCounty <- function(county1, doses.dt) {
  pop <- readRDS("Inputs/county population by age.rds")[county1, ]
  population <- data.table(pop)
  #convert census age categories to CDC age categories
  population$age <- c(0, 5, 5, 5, 18, 18, 18, 18, 18, 30, 30, 40, 40, 50, 50, 50, 50, 65, 65, 65, 75, 75, 85)
  population <- population[, .(pop = sum(pop)), by = "age"]

  age = c(0, 5, 18, 30, 40, 50, 65, 75, 85)
  vax_prop_sf = c(0, 0, 7.3, 10.4, 10.8, 18.5, 24.6, 18.4, 10.0)/100 #as of 2/16 (assumed 85+ is 35% of 75+ same as SF population)

  doses_actual <- doses.dt[county == county1, .(date, dose_num, age_bin, count)]
  frac_65plus <- doses_actual[dose_num == 1, sum(count * (age_bin == "65+")) / sum(count)]

  #scale SF vax props to county level above and below 65
  dose_proportion <- data.table(age, vax_prop_sf)
  dose_proportion[age >= 65, vax_prop := vax_prop_sf * frac_65plus / sum(vax_prop_sf)]
  dose_proportion[age < 65, vax_prop := vax_prop_sf * (1 - frac_65plus) / sum(vax_prop_sf)]
  dose_proportion <- dose_proportion[, vax_prop]

  doses_actual[, date := as.Date(date)]
  doses_actual <- doses_actual[, .(dose1 = sum(count * (dose_num == 1)), dose2 = sum(count * (dose_num == 2))), by = "date"]

  variants <- data.table(name = c("Wild", "UK", "SA", "CA", "BR"),
                         vaccine_efficacy_for_susceptibility_1 =  c(0.81, 0.81, 0.7, 0.81, 0.81),
                         vaccine_efficacy_for_susceptibility_2 = c(0.89, 0.89, 0.8, 0.89, 0.89),
                         vaccine_efficacy_against_progression_1 = c(0.90, 0.90, 0.80, 0.90, 0.90),
                         vaccine_efficacy_against_progression_2 = c(0.99, 0.99, 0.90, 0.99, 0.90),
                         transmisson_mult = c(1, 1.5, 1, 1.25, 1.8),
                         duration_vaccinated_12 = 365 * c(3, 3, 1, 3, 2),
                         duration_natural_12 =    365 * c(3, 3, 1, 3, 2),
                         hosp_mult = c(1, 1.3, 1, 1, 1.3),
                         mort_mult = c(1, 1.5, 1, 1, 1.5),
                         daily_growth = c(1, 1, 1, 1.034, 1),
                         frac_on_day0 = c(0.25, 0, 0, 0.75, 0))
  variant_day0 <- as.Date("2021/2/24")

  #generate extra doses/variants, will subset
  start_date <- as.Date("2020/1/1")
  end_date <- as.Date("2022/1/1")

  scale <- population[, sum(pop)] / 883305  #scale to SF

  #Youyang says ~1.6% daily increase = 80 per day increase from 5000 (this might be off - not fully vaccinated until end June, 10000 per day May 10)
  doses <- GetDoses(doses_actual, doses_per_day_base = 5000 * scale, doses_per_day_increase  = 80 * scale, doses_per_day_maximum = 10000 * scale, start_increase_day = as.Date("2021/3/1"), start_date, end_date,  dose_proportion, population, vax_uptake = 0.85, max_second_dose_frac = rep(0.7, length(start_date:end_date)))
  v <- GetVaccineParams(doses, variants, start_date, end_date, variant_day0, population, dose_proportion)
  return(v)
}


Get1 <- function(zz) {
  stopifnot(uniqueN(zz) == 1)
  zz[1]
}

GetCountyInputs <- function(county1, county.dt, doses.dt) {
  county.pop1 <- county.dt[county == county1, Get1(population)]
  if (county1 == "San Francisco") {
    cat("SF is using state data, not SF data\n")
  }
  input.file <- "Inputs/CAcounties_sigmaobs.xlsx"
  sheets <- LEMMA:::ReadInputs(input.file)
  county.dt1 <- county.dt[county == county1, .(date, hosp.conf, hosp.pui, icu.conf, icu.pui,  deaths.conf, admits.conf, admits.pui, cases.conf, cases.pui, seroprev.conf, seroprev.pui)]
  county.dt1[!is.na(deaths.conf), deaths.pui := 0]
  sheets$Data <- county.dt1

  sheets$`Model Inputs`[internal.name == "total.population", value := county.pop1]

  sheets$Data[is.na(deaths.pui) & !is.na(deaths.conf), deaths.pui := 0]
  inputs <- LEMMA:::ProcessSheets(sheets, input.file)
  inputs$vaccines <- GetVaccineParamsForCounty(county1, doses.dt)
  inputs$internal.args$output.filestr <- paste0("Restart/Forecast/test point est_", county1)

  mean.ini <- 1e-5 * county.pop1
  inputs$internal.args$lambda_ini_exposed <- 1 / mean.ini
  return(inputs)
}

RunOneCounty <- function(county1, county.dt, doses.dt) {

  inputs <- GetCountyInputs(county1, county.dt, doses.dt)
  #county1 is used for printing and filename, could be removed/put in inputs

  if (include.seroprev) {
    inputs$internal.args$output.filestr <- paste0("~/Documents/tempForecasts/march12-withSP/test point est_", county1)
  } else {
    #also new deaths, removed case holidays
    inputs$internal.args$output.filestr <- paste0("~/Documents/tempForecasts/march13-noSP-newint-noAdmits/test point est_", county1)
    # inputs$internal.args$output.filestr <- paste0("Forecasts/", county1)
    ### temp
    inputs$obs.data[, seroprev.conf := NA_real_]
    inputs$obs.data[, seroprev.pui := NA_real_]
    cat("----- temp -- no seroprev\n\n")
  }
  lemma <- LEMMA:::CredibilityInterval(inputs)
  return(lemma)
}


