library(matrixStats)
library(data.table)
library(ggplot2)

#SF: CA variant 55% January, 15% end November
# vax_prop_sf = c(0, 0, 7.3, 10.4, 10.8, 18.5, 24.6, 18.4, 10.0)/100 #as of 2/16 (assumed 85+ is 35% of 75+ same as SF population) #in CAcounties.xlsx

#100M J&J by end of May
#Pfizer+Moderna "another 100 million people by the end of July" = 200M doses [article March 8, ~0 J&J], so J&J should be 1/3 of doses to come
#https://www.statnews.com/2021/03/08/will-the-u-s-have-covid-vaccine-doses-for-everyone-by-the-end-of-may-probably/

Get1 <- function(zz) {
  stopifnot(uniqueN(zz) == 1)
  zz[1]
}

GetCountySheets <- function(county1, county.dt, doses.dt) {
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
    ues <- c(34, 39, 43, 45, 0, 50, 0, 57, 53, 44, 36, 0, 37, 0, 35, 43, 31, 23, 0, 38, 0, 0, 0, 0, 20, 0, 0, 0, 14, 16, 11, 19, 0, 0, 0, 12, 9, 10, 7, 0, 0, 0, 7, 12, 4, 7, 0, 0, 0, 5, 9, 2, 7, 0, 0, 0, 5, 1, 1, 2, 0, 0, 0, 7, 7, 3, 3, 0, 0, 0, 3, 1, 1)
    ues.dt <- data.table(date = as.Date("2021/1/10") + (1:length(ues)) - 1, ues)
    print(tail(ues.dt))
    sheets$Data <- merge(sheets$Data, ues.dt, by = "date", all.x = T)
    sheets$Data[, ues := frollmean(ues, 7)]
    sheets$Data[!is.na(ues), cases.conf := ues + cases.conf]
    sheets$Data$ues <- NULL
  }

  # sheets$`Model Inputs`[internal.name == "total.population", value := county.pop1]


  pop <- readRDS("Inputs/county population by age.rds")[county1, ]
  population <- data.table(pop)
  #convert census age categories to CDC/vaccine eligibility age categories - for simplicity use CDC 12-15 = census 10-14, CDC 16-30 = census 15-30
  #CDC:    0  5 12 16                30    40    50          65       75    85
  #census: 0  5 10 15 18 20 21 22 25 30 35 40 45 50 55 60 62 65 67 70 75 80 85
  population$age <- c(0, 5, 12, 16, 16, 16, 16, 16, 16, 30, 30, 40, 40, 50, 50, 50, 50, 65, 65, 65, 75, 75, 85)
  sheets$`Vaccine Distribution`$pop <- population[, .(pop = sum(pop)), by = "age"]$pop


  doses_actual <- doses.dt[county == county1, .(date, dose_num, age_bin, count)]
  frac_65plus <- doses_actual[dose_num == 1, sum(count * (age_bin == "65+")) / sum(count)]

  #scale SF vax props to county level above and below 65
  sheets$`Vaccine Distribution`[age >= 65, dose_proportion := dose_proportion * frac_65plus / sum(dose_proportion)]
  sheets$`Vaccine Distribution`[age < 65, dose_proportion := dose_proportion * (1 - frac_65plus) / sum(dose_proportion)]

  doses_actual[, date := as.Date(date)]
  doses_actual <- doses_actual[, .(dose1 = sum(count * (dose_num == "1")), dose2 = sum(count * (dose_num == "2")), doseJ = sum(count * (dose_num == "J"))), by = "date"]

  sheets$`Vaccine Doses - Observed` <- doses_actual

  scale <- county.pop1 / 883305  #scale to SF
  #rescale doses_per_day_base, doses_per_day_increase, doses_per_day_maximum - this is clunky because value is a list
  for (i in c("doses_per_day_base", "doses_per_day_increase", "doses_per_day_maximum")) {
    index <- sheets$`Vaccine Doses - Future`[, which(internal.name == i)]
    rescaled.mrna <- unlist(sheets$`Vaccine Doses - Future`[index, mrna]) * scale
    rescaled.jj <- unlist(sheets$`Vaccine Doses - Future`[index, jj]) * scale
    sheets$`Vaccine Doses - Future`[index, mrna := rescaled.mrna]
    sheets$`Vaccine Doses - Future`[index, jj := rescaled.jj]
  }

  date1 <- as.Date("2020/10/1")
  sheets$Data[date <= date1, icu.conf := NA]
  sheets$Data[date <= date1, icu.pui := NA]
  sheets$Data[date <= date1, deaths.conf := NA]
  sheets$Data[date <= date1, deaths.pui := NA]

  return(sheets)
}


GetCountyInputs <- function(county1, county.dt, doses.dt) {
  sheets <- GetCountySheets(county1, county.dt, doses.dt)

  inputs <- LEMMA:::ProcessSheets(sheets)

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

  inputs$internal.args$weights <- c(1, 1, 1, 1, 0.5, 1)
  inputs$internal.args$output.filestr <- paste0("Forecasts/", county1)
  return(inputs)
}

RunOneCounty <- function(county1, county.dt, doses.dt) {
  inputs <- GetCountyInputs(county1, county.dt, doses.dt)
  lemma <- LEMMA:::CredibilityInterval(inputs)
  return(lemma)
}


