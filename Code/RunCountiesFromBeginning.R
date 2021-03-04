#this looks good! no warnings, ran in ~90sec

#test on larger set of counties - need to output colMedians and colSds x[, , index] for some or all index for each county
#also output median and sd other posteriors, should we use these (e.g. frac_mort?); I think depends in part on if we want to fit to recent death rate or overall
#automate some process for starting from scratch and caching state/posteriors and also for starting from cache
#email Paul, Chris
#think about how to generate full fit if needed (e.g. save Rt quantiles up to restart date for plotting)
#think about when to move restart date and update cache - if restart is after vaccines start would need more categories
#how to get initial cache - will take a really long time using correct sigmaobs, maybe can run in stages - eg feb 2020-july-sept-dec
#compare feb2020-july2020 vs feb2020-may2020-july2020 for some counties
#document this change + sigmaobs fix + variants/vaccines - set a deadline and priorities
#clean up LEMMA code (GetStanInputs, LEMMA.stan)
#plan for new spreadsheet format (caching + variants + vaccines)
#think about fitting to cases, admits
#sort out priors (SF is different than counties)
#** is transmission blocking correct? are case calculations correct?
#update RunCACounties-SF


library(matrixStats)
library(data.table)
# setwd("~/Documents/GitHub/LEMMA-Forecasts/")

source('Code/GetInputsVaxRestart.R')
source('Code/GetVaccineParams.R')

RunLemma <- function(county1, county.dt, doses.dt, restart.date, end.date, prev.state) {
  vaccines <- GetVaccineParamsForCounty(county1, doses.dt)
  inputs <- GetInputsVaxRestart(county1, county.dt, restart.date, end.date, initial.state = prev.state, vaccines)

  inputs$internal.args$output.filestr <- paste0("Restart/Forecast/test vax with cases and seroprev_", county1, "_", restart.date)
  inputs$internal.args$refresh <- 100 #500
  inputs$internal.args$iter <- 1000
  lemma <- LEMMA:::CredibilityInterval(inputs)

  x <- rstan::extract(lemma$fit.to.data, pars = "x")[[1]]
  cases <- rstan::extract(lemma$fit.to.data, pars = "total_cases")[[1]]

  index <- dim(x)[3]
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

  stopifnot(restart.date <= as.Date("2020/12/15"))
  #assumes vaccinations haven't started
  state <- list(mu_iniE = mu[3], mu_ini_Imild = mu[5], mu_ini_Ipreh = mu[7], mu_ini_Rlive = mu[13], mu_ini_cases = mu_cases,
                sigma_iniE = sigma[3], sigma_ini_Imild = sigma[5], sigma_ini_Ipreh = sigma[7], sigma_ini_Rlive = sigma[13], sigma_ini_cases = sigma_cases, from_beginning = 0)

  pars <- c("r0", "duration_latent", "duration_rec_mild", "duration_pre_hosp", "duration_hosp_mod",
            "duration_hosp_icu", "frac_hosp", "frac_icu", "frac_mort",
            "beta_multiplier", "t_inter", "sigma_obs", "ini_E", "ini_Imild", "ini_Ipreh", "ini_Rlive", "frac_tested")
  posteriors <- lapply(rstan::extract(lemma$fit.to.data, pars = pars), function (z) colQuantiles(as.matrix(z), probs = seq(0, 1, by = 0.05)))
  state <- c(state, posteriors = list(posteriors))
  return(state)
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

  start_date <- as.Date("2020/12/1")
  end_date <- as.Date("2021/8/1")

  scale <- population[, sum(pop)] / 883305  #scale to SF

#Youyang says ~1.6% daily increase = 80 per day increase from 5000 (this might be off - not fully vaccinated until end June, 10000 per day May 10)
  doses <- GetDoses(doses_actual, doses_per_day_base = 5000, doses_per_day_increase  = 80, doses_per_day_maximum = 10000, start_increase_day = as.Date("2021/3/1"), start_date, end_date,  dose_proportion, population, vax_uptake = 0.85, max_second_dose_frac = rep(0.7, length(start_date:end_date)))
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
    start.date <- dt[, date] - 5
    deaths <- dt[, deaths.conf]
    pop <- dt[, population]

    cases.est <- pmax(deaths, 1) / 0.006 #0.006 = estimated IFR

    state <- list(mu_iniE = pop * 1e-5, mu_ini_Imild = 0.1, mu_ini_Ipreh = 0.1, mu_ini_Rlive = cases.est, mu_ini_cases = cases.est,
                  sigma_iniE = pop * 1e-3, sigma_ini_Imild = 10, sigma_ini_Ipreh = 10, sigma_ini_Rlive = cases.est / 2, sigma_ini_cases = cases.est / 2,
                  from_beginning = 0)
    return(list(start.date = start.date, state = state))
  }
}

RunOneCounty <- function(county1, county.dt, doses.dt) {
  try.result <- try({
    ParallelLogger::logInfo("starting county = ", county1)
    sink.file <- paste0("Restart/Logs/progress-", county1, ".txt")
    sink(sink.file)
    cat("county = ", county1, "\n")
    cat("start time = ", as.character(Sys.time() - 3600 * 8), "\n")
    cat("max date = ", as.character(max(county.dt$date)), "\n")

    start.list <- GetStartDate(county1, county.dt)
    restart.interval <- 60 #9999 #140 #60
    last.restart.date <- as.Date("2020/12/15")
    if (start.list$start.date > (last.restart.date - 30)) {
      stop("not enough data before vaccine roll-out in ", county1)
    }
    num.intervals <- pmax(2, ceiling(as.numeric(last.restart.date - start.list$start.date) / restart.interval))
    restart.date.set <- round(seq(start.list$start.date, as.Date("2020/12/15"), length.out = num.intervals))
    total.time <- system.time({
      for (i in seq_along(restart.date.set)) {
        restart.date <- restart.date.set[i]
        if (i == 1) {
          prev.state <- start.list$state
        } else {
          prev.state <- state
        }
        if (i == length(restart.date.set)) {
          end.date <- as.Date("2021/7/1")
        } else {
          end.date <- restart.date.set[i + 1]
        }
        print(county1)
        print(c(restart.date, end.date))
        state <- RunLemma(county1, county.dt, doses.dt, restart.date, end.date, prev.state)
        warning(paste("-----------", county1, i, restart.date, end.date, "-----------"))
        print(warnings())
        saveRDS(prev.state, paste0("Restart/State/state_", county1, "_", restart.date, ".rds"))
      }
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


