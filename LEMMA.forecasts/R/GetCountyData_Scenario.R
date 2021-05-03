# --------------------------------------------------------------------------------
#   Functions to prepare data for running scenarios:
#   1. GetCountyInputs_scen
#   2. Scenario
#   3. GetResults_scen
# --------------------------------------------------------------------------------


#' @title Get county inputs for scenarios
#' @description This function is called from \code{\link[LEMMA.forecasts]{Scenario}}.
#' @param county1 a character string giving the name of the county
#' @param county.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetCountyData}}
#' @param doses.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetDosesData}}
#' @param k_uptake a character string, "low" or "high" giving vaccine uptake
#' @param k_ukgrowth growth rate of UK variant
#' @param k_brgrowth growth rate of BR variant
#' @param remote a logical value, if \code{TRUE} download all data from remotes, otherwise use local data
#' @param writedir a character string giving a directory to write to, it should only be used if \code{remote} is \code{TRUE}.
#' This assumes the directory whose path is given already exists.
#' @return a named list of values
GetCountyInputs_scen <- function(county1, county.dt, doses.dt, k_uptake, k_ukgrowth, k_brgrowth, remote = FALSE, writedir = NULL) {
  sheets <- GetCountySheets(county1, county.dt, doses.dt,remote = remote)

  stopifnot(k_uptake %in% c("low", "high"))
  if (k_uptake == "low") {
    sheets$`Vaccine Distribution`[age < 65, vax_uptake := 0.75]
    sheets$`Vaccine Distribution`[age >= 65, vax_uptake := 0.87]
  } else {
    sheets$`Vaccine Distribution`[, vax_uptake := 0.87]
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
  inputs <- ModifyCountyInputs(county1, inputs)

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


#' @title Run a scenario
#' @description This function is called from \code{\link[LEMMA.forecasts]{RunOneCounty_scen}}.
#' @param filestr1 a character string giving the type of scenario to run
#' @param county1 a character string giving the name of the county
#' @param county.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetCountyData}}
#' @param doses.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetDosesData}}
#' @param k_mu_beta_inter
#' @param lemma_statusquo either \code{NULL} or the result of a call to \code{Scenario} with \code{filestr1 = "statusquo"}
#' @param k_uptake a character string, "low" or "high" giving vaccine uptake
#' @param k_ukgrowth growth rate of UK variant
#' @param k_brgrowth growth rate of BR variant
#' @param k_max_open
#' @param remote a logical value, if \code{TRUE} download all data from remotes, otherwise use local data
#' @param writedir a character string giving a directory to write to, it should only be used if \code{remote} is \code{TRUE}.
#' This assumes the directory whose path is given already exists.
#' @return a named list of values
Scenario <- function(
  filestr1, county1, county.dt, doses.dt,
  k_mu_beta_inter = NULL, lemma_statusquo = NULL, k_uptake = "low",
  k_ukgrowth = 1, k_brgrowth = 1, k_max_open = 0.75,
  remote = FALSE, writedir = NULL
) {

  inputs <- GetCountyInputs_scen(
    county1 = county1, county.dt = county.dt, doses.dt = doses.dt, k_uptake = k_uptake,
    k_ukgrowth = k_ukgrowth, k_brgrowth = k_brgrowth,
    remote = remote, writedir = writedir
  )

  if (filestr1 == "statusquo") {
    lemma <- LEMMA:::CredibilityInterval(inputs)
    return(lemma)
  }

  tier_date <- as.Date("2021/5/7")

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

  results <- GetResults_scen(lemma$projection, filestr1)
  invisible(results)
}


#' @title Get results of running a scenario
#' @description This function is called from \code{\link[LEMMA.forecasts]{Scenario}}.
#' @param projection output of a LEMMA model run
#' @param name a character string giving the type of scenario to run
GetResults_scen <- function(projection, name) {
  projection1 <- projection[date >= Sys.Date()]
  hosp.peak <- projection1[, max(hosp)]
  hosp.peak.date <- projection1[, date[which.max(hosp)]]
  additional.admits <- projection1[, sum(admits)]
  additional.deaths <- projection1[, max(deaths) - min(deaths)]
  additional.deaths.byNov2021 <- projection1[date <= as.Date("2021/11/1"), max(deaths) - min(deaths)]
  additional.cases <- projection1[, max(totalCases) - min(totalCases)]
  return(data.table(name, hosp.peak, hosp.peak.date, additional.admits, additional.deaths, additional.deaths.byNov2021, additional.cases))
}
