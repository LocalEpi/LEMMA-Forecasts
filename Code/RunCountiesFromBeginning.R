library(matrixStats)
library(data.table)
library(ggplot2)

source('Code/GetVaccineParams.R')

GetVaccineParamsForCounty <- function(county1, doses.dt, start_date, end_date) {
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
                         vaccine_efficacy_for_susceptibility_1 =  c(0.60, 0.60, 0.50, 0.60, 0.50),
                         vaccine_efficacy_for_susceptibility_2 = c(0.80, 0.80, 0.70, 0.80, 0.70),
                         vaccine_efficacy_against_progression_1 = c(0.90, 0.90, 0.80, 0.90, 0.80),
                         vaccine_efficacy_against_progression_2 = c(0.99, 0.99, 0.90, 0.99, 0.90),
                         transmisson_mult = c(1, 1.5, 1, 1.25, 1.8),
                         duration_vaccinated_12 = 365 * c(5, 5, 1, 5, 2),
                         duration_natural_12 =    365 * c(3, 3, 1, 3, 2),
                         hosp_mult = c(1, 1.3, 1, 1, 1.3),
                         mort_mult = c(1, 1.5, 1, 1, 1.5),
                         daily_growth_prior = c(1, 1, 1, 1.034, 1),
                         daily_growth_future = c(1, 1, 1, 1, 1),
                         frac_on_day0 = c(0.45, 0, 0, 0.55, 0))
  variant_day0 <- as.Date("2021/1/25")

  scale <- population[, sum(pop)] / 883305  #scale to SF

  doses <- GetDoses(doses_actual, doses_per_day_base = 6000 * scale, doses_per_day_increase  = 70 * scale, doses_per_day_maximum = 12000 * scale, start_increase_day = as.Date("2021/3/1"), start_date, end_date,  dose_proportion, population, vax_uptake = 0.85, max_second_dose_frac = rep(0.7, length(start_date:end_date)))
  v <- GetVaccineParams(doses, variants, start_date, end_date, variant_day0, population, dose_proportion)
  return(v)
}


Get1 <- function(zz) {
  stopifnot(uniqueN(zz) == 1)
  zz[1]
}

GetCountyInputs <- function(county1, county.dt, doses.dt) {
  county.pop1 <- county.dt[county == county1, Get1(population)]
  county.dt1 <- county.dt[county == county1, .(date, hosp.conf, hosp.pui, icu.conf, icu.pui,  deaths.conf, deaths.pui, admits.conf, admits.pui, cases.conf, cases.pui, seroprev.conf, seroprev.pui)]
  input.file <- "Inputs/CAcounties.xlsx"
  sheets <- LEMMA:::ReadInputs(input.file)
  sheets$Data <- county.dt1
  if (county1 == "San Francisco") {
    # #use local data for intervention dates and hospital/ICU census
    # sf.input.file <- "Inputs/SF.xlsx"
    # sf.sheets <- list(LEMMA:::ReadExcel(sf.input.file, sheet = "Interventions", skip = 2),
    #                   LEMMA:::ReadExcel(sf.input.file, sheet = "Data", skip = 3))
    # names(sf.sheets) <- sapply(sf.sheets, function (z) attr(z, "sheetname"))
    # sf.sheets <- rapply(sf.sheets, as.Date, classes = "POSIXt", how = "replace") #convert dates
    # sheets$Interventions <- sf.sheets$Interventions
    # sheets$Data$hosp.conf <- sheets$Data$hosp.pui <- sheets$Data$icu.conf <- sheets$Data$icu.pui <- NULL
    # sheets$Data <- merge(sheets$Data, sf.sheets$Data[, .(date, hosp.conf, hosp.pui, icu.conf, icu.pui)], by = "date", all = T)

    #add UeS cases
    ues <- c(34, 39, 43, 45, 0, 50, 0, 57, 53, 44, 36, 0, 37, 0, 35, 43, 31, 23, 0, 38, 0, 0, 0, 0, 20, 0, 0, 0, 14, 16, 11, 19, 0, 0, 0, 12, 9, 10, 7, 0, 0, 0, 7, 12, 4, 7, 0, 0, 0, 5, 9, 2, 7, 0, 0, 0, 5, 1, 1, 2, 0, 0, 0, 7, 7, 3, 3)
    ues.dt <- data.table(date = as.Date("2021/1/10") + (1:length(ues)) - 1, ues)
    print(ues.dt)
    sheets$Data <- merge(sheets$Data, ues.dt, by = "date", all.x = T)
    sheets$Data[, ues := frollmean(ues, 7)]
    sheets$Data[!is.na(ues), cases.conf := ues + cases.conf]
    sheets$Data$ues <- NULL
  }

  sheets$`Model Inputs`[internal.name == "total.population", value := county.pop1]

  inputs <- LEMMA:::ProcessSheets(sheets, input.file)

  #need different initial conditions to converge
  if (county1 == "Siskiyou") {
    inputs$internal.args$init_frac_mort_nonhosp <- 0.00001
  }
  if (county1 == "Humboldt") {
    inputs$internal.args$init_frac_mort_nonhosp <- 0.001
  }
  if (county1 == "El Dorado") {
    inputs$internal.args$init_frac_mort_nonhosp <- 0.001
  }

  inputs <- c(inputs, GetVaccineParamsForCounty(county1, doses.dt, start_date = inputs$internal.args$simulation.start.date + 1, end_date = inputs$model.inputs$end.date))

  mean.ini <- 1e-5 * county.pop1
  inputs$internal.args$lambda_ini_exposed <- 1 / mean.ini


  date1 <- as.Date("2020/10/1")
  inputs$obs.data[date <= date1, icu.conf := NA]
  inputs$obs.data[date <= date1, icu.pui := NA]
  inputs$obs.data[date <= date1, deaths.conf := NA]
  inputs$obs.data[date <= date1, deaths.pui := NA]

  inputs$internal.args$weights <- c(1, 1, 1, 1, 0.5, 1)
  inputs$internal.args$output.filestr <- paste0("Forecasts/", county1)
  return(inputs)
}

RunOneCounty <- function(county1, county.dt, doses.dt) {
  inputs <- GetCountyInputs(county1, county.dt, doses.dt)
  lemma <- LEMMA:::CredibilityInterval(inputs)
  return(lemma)
}


