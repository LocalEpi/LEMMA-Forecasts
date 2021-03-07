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

RunLemma <- function(inputs) {
  lemma <- LEMMA:::CredibilityInterval(inputs)

  #TODO: move this to LEMMA package
  x <- rstan::extract(lemma$fit.to.data, pars = "x")[[1]]
  cases <- rstan::extract(lemma$fit.to.data, pars = "total_cases")[[1]]

  index <- dim(x)[3] - inputs$internal.args$overlap_days
  mu <- colMedians(x[, , index])
  sigma <- colSds(x[, , index])
  mu_cases <- median(cases[, index])
  sigma_cases <- sd(cases[, index])

  # int Su = 1;
  # int Sv = 2;
  # int Eu = 3;
  # int Ev = 4;
  # int Imildu = 5;
  # int Imildv = 6;
  # int Iprehu = 7;
  # int Iprehv = 8;
  # int Hmodu  = 9;
  # int Hmodv  = 10;
  # int Hicuu  = 11;
  # int Hicuv  = 12;
  # int Rliveu = 13;
  # int Rlivev = 14;
  # int Rmort = 15;

  #TODO: add stopifnot to check that initial state shouldnt have any vaccines yet - not sure how to do this
  #assumes vaccinations haven't started
  state <- list(mu_iniE = mu[3], mu_ini_Imild = mu[5], mu_ini_Ipreh = mu[7], mu_ini_Rlive = mu[13], mu_ini_cases = mu_cases,
                sigma_iniE = sigma[3], sigma_ini_Imild = sigma[5], sigma_ini_Ipreh = sigma[7], sigma_ini_Rlive = sigma[13], sigma_ini_cases = sigma_cases, from_beginning = 0)

  pars <- c("r0", "duration_latent", "duration_rec_mild", "duration_pre_hosp", "duration_hosp_mod",
            "duration_hosp_icu", "frac_hosp", "frac_icu", "frac_mort",
            "beta_multiplier", "t_inter", "sigma_obs", "ini_E", "ini_Imild", "ini_Ipreh", "ini_Rlive", "frac_tested")
  posteriors <- lapply(rstan::extract(lemma$fit.to.data, pars = pars), function (z) colQuantiles(as.matrix(z), probs = seq(0, 1, by = 0.05)))
  state <- c(state, posteriors = list(posteriors), inputs = inputs)
  return(list(lemma = lemma, state = state))
}


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

#Get start date and state if hosp goes to zero
GetStartDate <- function(county1, county.dt) {
  dt <- county.dt[county == county1]
  dt <- dt[frollsum(hosp.conf, 5) <= 1]
  if (nrow(dt) == 0) {
    return(list(start.date = as.Date("2020/2/17"), state = NULL))
  } else {
    dt <- dt[date == max(date)]
    start.date <- dt[, date] - 7 #-5 worked for all but Lake, El Dorado, Madera, Siskiyou, not sure why these crashed
    deaths <- dt[, deaths.conf]
    pop <- dt[, population]

    cases.est <- pmax(deaths, 1) / 0.006 #0.006 = estimated IFR

    state <- list(mu_iniE = pop * 1e-5, mu_ini_Imild = 0.1, mu_ini_Ipreh = 0.1, mu_ini_Rlive = cases.est, mu_ini_cases = cases.est,
                  sigma_iniE = pop * 1e-3, sigma_ini_Imild = 10, sigma_ini_Ipreh = 10, sigma_ini_Rlive = cases.est / 2, sigma_ini_cases = cases.est / 2,
                  from_beginning = 0)
    return(list(start.date = start.date, state = state))
  }
}

#county1 is used for printing and filename, could be removed/put in inputs
RunFromBeginning <- function(inputs.orig, county1) {
  restart.date.set <- inputs.orig$internal.args$restart.date.set
  for (i in seq_along(restart.date.set)) {
    restart.date <- restart.date.set[i]
    if (i == 1) {
      prev.state <- inputs.orig$internal.args$initial.state
    } else {
      prev.state <- state
    }
    if (i == length(restart.date.set)) {
      end.date <- as.Date("2021/7/1")
    } else {
      end.date <- restart.date.set[i + 1] + inputs$internal.args$overlap_days
    }
    print(county1)
    print(c(restart.date, end.date))
    inputs <- GetInputsRestart(inputs.orig, restart.date, end.date, initial.state = prev.state)
    obj <- RunLemma(inputs)
    state <- obj$state
    warning(paste("-----------", county1, i, restart.date, end.date, "-----------"))
    print(warnings())
    saveRDS(prev.state, paste0("Restart/State/state_", county1, "_", restart.date, ".rds"))
    if (i  == length(restart.date.set)) {
      #useful for diagnostics
      saveRDS(state, paste0("Restart/State/state_", county1, "_", end.date, ".rds"))
    }
  }
  return(obj)
}

Get1 <- function(zz) {
  stopifnot(uniqueN(zz) == 1)
  zz[1]
}

GetCountyInputs <- function(county1, county.dt, doses.dt) {
  start.list <- GetStartDate(county1, county.dt)
  restart.interval <- 60 #9999 #140 #60
  last.restart.date <- as.Date("2020/12/15")
  if (start.list$start.date > (last.restart.date - 30)) {
    stop("not enough data before vaccine roll-out in ", county1)
  }
  num.intervals <- pmax(2, ceiling(as.numeric(last.restart.date - start.list$start.date) / restart.interval))
  restart.date.set <- round(seq(start.list$start.date, as.Date("2020/12/15"), length.out = num.intervals))

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
  inputs$internal.args$output.filestr <- paste0("Restart/Forecast/test vax with cases and seroprev_", county1)

  #TODO: move these to another list element?
  inputs$internal.args$restart.date.set <- restart.date.set
  inputs$internal.args$initial.state <- start.list$state
  inputs$internal.args$overlap_days <- 21
  return(inputs)
}

RunOneCounty <- function(county1, county.dt, doses.dt) {
  try.result <- try({
    ParallelLogger::logInfo("starting county = ", county1)
    sink.file <- paste0("Restart/Logs/progress-", county1, ".txt")
    sink(sink.file)
    cat("county = ", county1, "\n")
    cat("start time = ", format(Sys.time(), usetz = T, tz = "America/Los_Angeles"), "\n")
    cat("max date = ", as.character(max(county.dt$date)), "\n")

    total.time <- system.time({
      inputs <- GetCountyInputs(county1, county.dt, doses.dt)
      RunFromBeginning(inputs, county1)
    })
    print(total.time)
    sink()
    ParallelLogger::logInfo("finished county = ", county1)
  })

  if (inherits(try.result, "try-error")) {
    ParallelLogger::logInfo("ERROR in = ", county1)
    ParallelLogger::logInfo(as.character(try.result))
  }

  return(NULL)
}


