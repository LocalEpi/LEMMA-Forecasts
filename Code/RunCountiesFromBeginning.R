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

RunLemma <- function(county1, county.dt, restart.date, end.date, prev.state) {
  nt <- end.date - restart.date + 1
  vaccines <- list()
  vaccines$vaccinated_per_day <- vaccines$vaccine_efficacy_for_susceptibility <- vaccines$vaccine_efficacy_against_progression <- rep(0, nt)
  vaccines$duration_vaccinated <- vaccines$duration_natural <- rep(99999, nt)
  vaccines$frac_hosp_multiplier <- vaccines$frac_icu_multiplier <- vaccines$frac_mort_multiplier <-  rep(1, nt)

  inputs <- GetInputsVaxRestart(county1, county.dt, restart.date, end.date, initial.state = prev.state, vaccines)

  inputs$internal.args$output.filestr <- paste0("Restart/Forecast/test vax with restart_", county1, "_", restart.date)
  inputs$internal.args$refresh <- 500
  inputs$internal.args$iter <- 2000
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

  return(state)
}

RunOneCounty <- function(county1, county.dt) {
  try.result <- try({

    sink.file <- paste0("Restart/Logs/progress-", county1, ".txt")
    sink(sink.file)
    cat("county = ", county1, "\n")
    cat("start time = ", as.character(Sys.time() - 3600 * 8), "\n")
    cat("max date = ", as.character(max(county.dt$date)), "\n")

    restart.date.set <- round(seq(as.Date("2020/2/17"), as.Date("2020/12/15"), length.out = 6)) #3 works well for SF, 6 is fast but had some warnings

    total.time <- system.time({
      for (i in seq_along(restart.date.set)) {
        restart.date <- restart.date.set[i]
        if (i == 1) {
          prev.state <- NULL
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
        state <- RunLemma(county1, county.dt, restart.date, end.date, prev.state)
        warning(paste("-----------", county1, i, restart.date, end.date, "-----------"))
        print(warnings())
        saveRDS(prev.state, paste0("Restart/State/state_", county1, "_", restart.date, ".rds"))
      }
    })
    print(total.time)

    sink()

    sink()
    ParallelLogger::logInfo("county = ", county1)

  })

  if (inherits(try.result, "try-error")) {
    ParallelLogger::logInfo("ERROR in = ", county1)
    ParallelLogger::logInfo(as.character(try.result))
  }

  return(NULL)
}


