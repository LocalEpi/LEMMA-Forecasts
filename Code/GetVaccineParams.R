library(data.table)

#1M nationally = 2700 in SF
#1.5M nationally = 4000 in SF
#2M nationally = 5400 in SF
#2.5M nationally = 6750 in SF
#3M nationally = 8100 in SF

#2/1 471 uk+brazil+SA, 467 uk => SA ~0.01 of variants
GetVaccineParams <- function(vaccinated_per_day_max, vaccinated_per_day_increase, uk_growth, sa_growth, uk_now, sa_now, nt) {
  vax.update <- 0.85

  rate.dt <- data.table(age = c(0, 5, 18, 30, 40, 50, 65, 75, 85),
                        hosp = c(1/4, 1/9, 1, 2, 3, 4, 5, 8, 13),
                        death = c(1/9, 1/16, 1, 4, 10, 30, 90, 220, 630),
                        vax_prop = c(0, 0, 7.3, 10.4, 10.8, 18.5, 24.6, 18.4, 10.0)/100) #as of 2/16 (assumed 85+ is 35% of 75+ same as SF population)
  rate.dt[, icu_rate := sqrt(death/hosp)]
  rate.dt[, mort_rate := sqrt(death/hosp)]

  if (F) {
    #convert from census age categories to CDC vax age categories
    sf.pop <- as.data.table(colSums(readRDS("~/Documents/CensusData/SF pop by age and zip.rds")), keep.rownames = T)
    setnames(sf.pop, c("age1", "pop"))
    sf.pop$age <- c(0, 5, 5, 5, 18, 18, 18, 18, 18, 30, 30, 40, 40, 50, 50, 50, 50, 65, 65, 65, 75, 75, 85)
    sf.pop <- sf.pop[, .(pop = sum(pop)), by = "age"]

    sf.pop[, pop := pop * 883305 / sum(pop)] #fixme - change if not SF

  } else {
    sf.pop <- as.data.table(structure(list(age = c(0, 5, 18, 30, 40, 50, 65, 75, 85), pop = c(39906.3731406559,
                                                                                              78740.7974681953, 166750.621255458, 180965.550761221, 122148.668191433,
                                                                                              158523.262709589, 74677.0819111263, 39658.0686133239, 21934.5759489982
    )), row.names = c(NA, -9L), class = "data.frame"))
  }


  rate.dt <- merge(sf.pop, rate.dt, by = "age", all = T)
  rate.dt[, max.vax := vax.update * pop]
  rate.dt[vax_prop == 0, max.vax := 0]

  #day 345 is Jan 26
  vaccinated_per_day <- rep(0, nt)
  #start 12/17, 2800 as of 1/26 [move these 10-14 days for vaccine to take effect?]
  time_to_effect <- 14
  vaccinated_per_day[time_to_effect + 305:345] <- seq(0, 2800, length.out = length(305:345))
  vaccinated_per_day[time_to_effect + 346:nt] <- pmin(vaccinated_per_day_max, 2800 + seq(from = 0, by = vaccinated_per_day_increase, length.out = nt - 345))
  vaccinated_per_day <- vaccinated_per_day[1:nt]
  fully_vaccinated_per_day <- shift(vaccinated_per_day, n = 30, fill = 0)
  #frac_fully <- fully_vaccinated_per_day / (vaccinated_per_day + 1e-10) #avoid NaN
  frac_fully <- rep(1, nt) #frac_fully isn't working correctly - total vaccinated is not pop * uptake * 0.99, need to count people moving to fully vaccinated somewhere; for now assume immediate fully vaccinated

  vaccinated_per_day[cumsum(vaccinated_per_day) > sum(rate.dt$max.vax)] <- 0 #do this after calculating frac_fully, otherwise late people don't get second dose

  wild_growth <- 0.96
  n <- nt - 345
  wild_now <- 1 - (uk_now + sa_now)
  wild <- wild_now * wild_growth^(1:n)
  uk <- uk_now * uk_growth^(1:n)
  sa <- sa_now * sa_growth^(1:n)

  frac_prevariants <- matrix(c(1, 0, 0), nrow = 345, ncol = 3, byrow = T)
  frac <- rbind(frac_prevariants, cbind(wild / (wild + uk + sa), uk / (wild + uk + sa), sa / (wild + uk + sa)))
  efficacy_transmission_1 <- c(0.5, 0.5, 0.2) #if one dose
  efficacy_transmission_2 <- c(0.7, 0.7, 0.35) #if two doses
  efficacy_susceptible_1 <- c(0.90, 0.90, 0.80) #if one dose
  efficacy_susceptible_2 <- c(0.99, 0.99, 0.90) #if two doses

  variant_hosp_mult <- c(1, 1.3, 1) #wild (should be 1), uk, sa
  variant_mort_mult <- c(1, 1.5, 1) / variant_hosp_mult

  duration_vaccinated_ <- c(360, 360, 180)
  duration_natural_ <- c(360, 360, 90)

  efficacy_transmission <- efficacy_susceptible <- duration_vaccinated <- duration_natural  <- rep(NA_real_, nt)
  for (it in 1:nt) {
    efficacy_transmission[it] <- sum((efficacy_transmission_1 * (1 - frac_fully[it]) + efficacy_transmission_2 * frac_fully[it]) * frac[it, ])
    efficacy_susceptible[it] <- sum((efficacy_susceptible_1 * (1 - frac_fully[it]) + efficacy_susceptible_2 * frac_fully[it]) * frac[it, ])

    duration_vaccinated[it] <- sum(frac[it, ] * duration_vaccinated_)
    duration_natural[it] <- sum(frac[it, ] * duration_natural_)
  }

  frac_hosp_multiplier <- frac_icu_multiplier <- frac_mort_multiplier <- rep(NA_real_, nt)
  dt <- copy(rate.dt)
  # hosp_rate_unvax / hosp_rate_pop
  dt[, vax := 0]
  dt[, pop_frac := pop / sum(pop)]
  hosp_rate_pop <- dt[, sum(hosp * pop_frac)] #rate relative to 18-30
  icu_rate_pop <- dt[, sum(icu_rate * pop_frac)] #rate relative to 18-30
  mort_rate_pop <- dt[, sum(mort_rate * pop_frac)] #rate relative to 18-30

  for (it in 1:nt) {
    not.maxed.prop <- dt[vax < max.vax, sum(vax_prop)]
    dt[, new_doses := (vax < max.vax) * vax_prop / (1e-10 + not.maxed.prop) * vaccinated_per_day[it]]
    dt[, vax := vax + new_doses]
    stopifnot(!anyNA(dt))
    dt[, unvaccinated_pop := pmax(0, pop - vax)]
    dt[, unvaccinated_pop_frac := unvaccinated_pop / sum(unvaccinated_pop)] #num unvax/total unvax

    frac_hosp_multiplier[it] <- dt[, sum(hosp * unvaccinated_pop_frac)] / hosp_rate_pop * sum(variant_hosp_mult * frac[it, ])
    frac_icu_multiplier[it] <- dt[, sum(icu_rate * unvaccinated_pop_frac)] / icu_rate_pop
    frac_mort_multiplier[it] <- dt[, sum(mort_rate * unvaccinated_pop_frac)] / mort_rate_pop * sum(variant_mort_mult * frac[it, ])
  }


  #return number of successfully vaccinated per day
  return(list(vaccinated_per_day = efficacy_susceptible * vaccinated_per_day, efficacy_transmission = efficacy_transmission, duration_vaccinated = duration_vaccinated, duration_natural = duration_natural, frac_hosp_multiplier = frac_hosp_multiplier, frac_icu_multiplier = frac_icu_multiplier, frac_mort_multiplier = frac_mort_multiplier))
}


  input.file.sf <- "Inputs/SF.xlsx"
  sheets.sf <- LEMMA:::ReadInputs(input.file.sf)
  input.file <- "Inputs/CAcounties_sigmaobs.xlsx"
  sheets <- LEMMA:::ReadInputs(input.file)
  sheets$Data <- sheets.sf$Data
  sheets$`Model Inputs` <- sheets.sf$`Model Inputs`

  inputs <- LEMMA:::ProcessSheets(sheets, input.file)

  #inputs$internal.args$iter <- 1000; inputs$internal.args$warmup <- NULL #7.7 hours
  inputs$internal.args$iter <- 300; inputs$internal.args$warmup <- NULL  #temp #1.4 hours, rhat = 1.05

  inputs$model.inputs$end.date <- as.Date("2021/8/1")
  nt <- inputs$model.inputs$end.date - inputs$internal.args$simulation.start.date

  #uk now and growth is good for San Diego but uk now is too high for SF -- need estimates of uk now and sa now by county?
  inputs$vaccines <- GetVaccineParams(vaccinated_per_day_max = 6000, vaccinated_per_day_increase = 135, uk_growth = 1.05, sa_growth = 1.08, uk_now = 0.01, sa_now = 0.0001, nt = nt)
  inputs$internal.args$output.filestr <- "~/Dropbox/LEMMA_shared/JS code branch/lemma input and output/SF vax/vax-base"
