GetCountyInputs_scen <- function(county1, county.dt, doses.dt, k_uptake, k_ukgrowth, k_brgrowth, remote = FALSE, writedir = writedir) {
  sheets <- GetCountySheets(county1, county.dt, doses.dt,remote = remote)

  stopifnot(k_uptake %in% c("low", "high"))
  if (k_uptake == "low") {
    sheets$`Vaccine Distribution`[age < 65, vax_uptake := 0.70]
    sheets$`Vaccine Distribution`[age >= 65, vax_uptake := 0.85]
  } else {
    sheets$`Vaccine Distribution`[, vax_uptake := 0.85]
  }

  if (remote) {
    tmp <- tempfile(fileext = ".xlsx")
    download.file(url = "https://github.com/LocalEpi/LEMMA-Forecasts/raw/master/Inputs/variants.xlsx",destfile = tmp)
    x <- as.data.table(readxl::read_excel(path = tmp))
    unlink(x = tmp)
  } else {
    x <- as.data.table(readxl::read_excel("Inputs/variants.xlsx"))
  }

  if (county1 %in% x$county) {
    index <- x[, which(county == county1)]
  } else {
    index <- x[, which(county == "California")]
  }
  uk <- x[index, B.1.1.7] / 100
  ca <- x[index, B.1.427 + B.1.429] / 100
  br <- x[index, P.1] / 100
  sa <- x[index, B.1.351] / 100
  wild <- 1 - (uk + ca + br + sa)
  max.date <- sheets$Data[, max(date)]
  sheets$Variants[, variant_day0 := max.date]
  sheets$Variants[, frac_on_day0 := c(wild, uk, ca, br, sa)]
  sheets$Variants[, daily_growth_prior := c(1, 1.05, 1.034, 1.03, 1.03)]
  sheets$Variants[, daily_growth_future := 1]

  #these have to be positive or growth won't matter
  if (k_ukgrowth > 0) stopifnot(sheets$Variants[name == "UK", frac_on_day0 > 0])
  if (k_brgrowth > 0) stopifnot(sheets$Variants[name == "BR", frac_on_day0 > 0])

  sheets$Variants[name == "UK", daily_growth_future := k_ukgrowth]
  sheets$Variants[name == "BR", daily_growth_future := k_brgrowth]

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
  if (county1 == "Del Norte") {
    inputs$internal.args$init_frac_mort_nonhosp <- 0.001
  }
  if (county1 == "Imperial") {
    inputs$obs.data <- rbind(data.table(date = as.Date("2020/3/10"), hosp.conf = 0, hosp.pui = 0), inputs$obs.data, fill = T)
    inputs$obs.data[, admits.conf := NA_real_]
    inputs$obs.data[, admits.pui := NA_real_]
  }

  inputs$internal.args$weights <- c(1, 1, 1, 1, 0.5, 1)

  if (remote) {
    # inputs$internal.args$output.filestr <- tempfile(pattern = county1)
    forecast_path <- paste0(writedir, "/Forecasts")
    dir.create(path = forecast_path,showWarnings = FALSE)
    inputs$internal.args$output.filestr <- paste0(forecast_path, "/", county1)
  } else {
    inputs$internal.args$output.filestr <- paste0("Forecasts/", county1)
  }

  return(inputs)
}

Scenario <- function(filestr1, county1, k_mu_beta_inter, lemma_statusquo = NULL, k_uptake = "low", k_ukgrowth = 1, k_brgrowth = 1, k_max_open = 0.75, remote = FALSE, writedir = NULL) {
  inputs <- GetCountyInputs_scen(county1, county.dt, doses.dt, k_uptake, k_ukgrowth, k_brgrowth, remote = remote, writedir = writedir)
  if (filestr1 == "statusquo") {
    lemma <- LEMMA:::CredibilityInterval(inputs)
    return(lemma)
  }

  tier_date <- as.Date("2021/5/4")

  if (!is.null(writedir)) {
    # filestr <- normalizePath(path = paste0(writedir, "/", county1, "_", filestr1))
    scen_path <- paste0(writedir, "/Scenarios")
    dir.create(path = scen_path,showWarnings = FALSE)
    filestr <- paste0(scen_path, "/", county1, "_", filestr1)
  } else {
    filestr <- paste0("Scenarios/", county1, "_", filestr1)
  }

  inputs$internal.args$output.filestr <- filestr

  #k_mu_beta_inter is multiplier to get to 100% open
  mu_beta <- sqrt(pmax(1, k_mu_beta_inter * k_max_open))
  new.int <- data.table(mu_t_inter = c(tier_date, as.Date("2021/6/15")),
                        sigma_t_inter = 2, mu_beta_inter = c(mu_beta, mu_beta), sigma_beta_inter = 1e-04,
                        mu_len_inter = 7, sigma_len_inter = 2)

  inputs$interventions <- rbind(inputs$interventions, new.int)

  if (is.null(lemma_statusquo)) {
    #refit
    lemma <- LEMMA:::CredibilityInterval(inputs)
  } else {
    lemma <- LEMMA:::ProjectScenario(lemma_statusquo, inputs)
  }

  pdf(paste0(filestr, ".pdf"), width = 11, height = 8.5)
  relative.contact.rate <- lemma$fit.extended$par$beta / (lemma$fit.extended$par$beta[1] * lemma$inputs$vaccines$transmission_variant_multiplier)
  dt <- data.table(date = lemma$projection$date, relative.contact.rate)
  print(ggplot(dt, aes(x = date, y = relative.contact.rate)) + geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%b") + ggtitle("Effective contact rate relative to initial effective contact rate\nnot including vaccine or variant effects") + xlab(""))

  doses <- lemma$inputs$vaccines_nonstan$doses
  doses[, doses_given := dose1 + dose2 + doseJ]
  print(ggplot(doses[date >= as.Date("2021/1/1")], aes(x = date, y = doses_given)) + geom_point() + scale_x_date(date_breaks = "1 month", date_labels = "%b") + xlab("") + labs(title = "Actual and Projected Vaccines Doses", subtitle = "note: scattered doses in the summer are wrapping up second doses, later doses are children"))

  variant_frac <- lemma$inputs$vaccines_nonstan$variant_frac
  colnames(variant_frac) <- lemma$inputs$vaccines_nonstan$variants$name
  variant_frac <- data.table(date = lemma$projection$date, variant_frac)
  dt <- melt(variant_frac[date >= as.Date("2020/10/1")], id.vars = "date", variable.name = "variant", value.name = "fraction")
  print(ggplot(dt, aes(x = date, y = fraction, color = variant)) + geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%b") + xlab("")) + ylab("Fraction of SARS-COV2")

  print(lemma$gplot$long.term)
  dev.off()

  results <- GetResults(lemma$projection, filestr1)
  invisible(results)
}

GetResults <- function(projection, name) {
  projection1 <- projection[date >= Sys.Date()]
  hosp.peak <- projection1[, max(hosp)]
  hosp.peak.date <- projection1[, date[which.max(hosp)]]
  additional.admits <- projection1[, sum(admits)]
  additional.deaths <- projection1[, max(deaths) - min(deaths)]
  additional.cases <- projection1[, max(totalCases) - min(totalCases)]
  return(data.table(name, hosp.peak, hosp.peak.date, additional.admits, additional.deaths, additional.cases))
}

RunOneCounty <- function(county1, county.dt, doses.dt, remote = FALSE, writedir = NULL) {
  Scenario1 <- function(filestr1, ...) {
    results <- Scenario(filestr1, county1, k_mu_beta_inter, ...)
    results.dt <<- rbind(results.dt, results)
  }

  results.dt <- NULL
  lemma <- Scenario("statusquo", county1, remote = remote, writedir = writedir)

  relative.contact.rate.statusquo <- lemma$fit.extended$par$beta / (lemma$fit.extended$par$beta[1] * lemma$inputs$vaccines$transmission_variant_multiplier)
  k_mu_beta_inter <- 1 / pmin(1, tail(relative.contact.rate.statusquo, 1))

  Scenario1("base", lemma, remote = remote, writedir = writedir)
  Scenario1("open90percent", lemma, k_max_open = 0.9, remote = remote, writedir = writedir)
  Scenario1("uptake85", lemma = NULL, k_uptake = "high", remote = remote, writedir = writedir) #refit - can change age dist
  Scenario1("UKvariant", lemma, k_ukgrowth = 1.06, remote = remote, writedir = writedir)
  Scenario1("BRvariant", lemma, k_brgrowth = 1.06, remote = remote, writedir = writedir)

  if (county1 == "San Francisco") {
    Scenario1("uptake85_open90percent", lemma = NULL, k_uptake = "high", k_max_open = 0.9, remote = remote, writedir = writedir)
    Scenario1("uptake85_open90percent_UKvariant", lemma = NULL, k_uptake = "high", k_max_open = 0.9, k_ukgrowth = 1.06, remote = remote, writedir = writedir)
    Scenario1("open90percent_UKvariant", lemma = NULL, k_max_open = 0.9, k_ukgrowth = 1.06, remote = remote, writedir = writedir)
    Scenario1("uptake85_BRvariant", lemma = NULL, k_uptake = "high", k_brgrowth = 1.06, remote = remote, writedir = writedir)
    Scenario1("uptake85_open90percent_BRvariant", lemma = NULL, k_uptake = "high", k_max_open = 0.9, k_brgrowth = 1.06, remote = remote, writedir = writedir)

    options(scipen = 3)

    if (remote) {
      scen_path <- paste0(writedir, "/Scenarios")
      if (dir.exists(scen_path)) {
        sink(file = paste0(scen_path, "/San Francisco_ScenarioSummary.txt"))
      }
    } else {
      sink("Scenarios/San Francisco_ScenarioSummary.txt")
    }

    print(results.dt, digits=0)

    cat("base = 75% open by June 22; uptake: 70% for <65, 85% for 65+; wild type and West Coast variants; 12-15 eligible May 1, 0-11 eligible Jan 1 \n")
    cat("other scenarios same as base except:\n")
    cat("open90percent = 90% open\n")
    cat("uptake85 = 85% uptake all ages\n")
    cat("UKvariant = UK variant dominant by August\n")
    cat("BRvariant = Brazil variant dominant by August\n")
    sink()
  }
  return(lemma)
}

# RunOneCounty <- function(county1, county.dt, doses.dt, remote = FALSE, writedir = NULL) {
#   if (is.null(writedir) & remote) {
#     stop("if running for shiny, a writedirectory to write to must be provided")
#   }
#   lemma <- Scenario("statusquo", county1, remote = remote, writedir = writedir)
#
#   relative.contact.rate.statusquo <- lemma$fit.extended$par$beta / (lemma$fit.extended$par$beta[1] * lemma$inputs$vaccines$transmission_variant_multiplier)
#   k_mu_beta_inter <- 1 / pmin(1, tail(relative.contact.rate.statusquo, 1))
#
#   Scenario("base", county1, k_mu_beta_inter, lemma, remote = remote, writedir = writedir)
#   Scenario("open90percent", county1, k_mu_beta_inter, lemma, k_max_open = 0.9, remote = remote, writedir = writedir)
#   Scenario("uptake85", county1, k_mu_beta_inter, lemma = NULL, k_uptake = "normal", remote = remote, writedir = writedir) #refit - can change age dist
#   Scenario("UKvariant", county1, k_mu_beta_inter, lemma, k_ukgrowth = 1.06, remote = remote, writedir = writedir)
#   Scenario("BRvariant", county1, k_mu_beta_inter, lemma, k_brgrowth = 1.06, remote = remote, writedir = writedir)
#
#   return(lemma)
# }

