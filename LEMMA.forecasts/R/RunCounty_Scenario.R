# --------------------------------------------------------------------------------
#   Run a scenario
#   1. RunOneCounty_scen
#   2. RunOneCounty_scen_input
#   3. Scenario
# --------------------------------------------------------------------------------


#' @title Run a county scenario
#' @param county1 a character string giving the name of the county
#' @param county.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetCountyData}}
#' @param doses.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetDosesData}}
#' @param remote a logical value, if \code{TRUE} download all data from remotes, otherwise use local data
#' @param writedir a character string giving a directory to write to, it should only be used if \code{remote} is \code{TRUE}.
#' This assumes the directory whose path is given already exists.
#' @export
RunOneCounty_scen <- function(county1, county.dt, doses.dt, remote = FALSE, writedir = NULL) {

  if (remote & is.null(writedir)) {
    stop("if 'remote' is TRUE, please provide a directory to write results to in 'writedir'")
  }

  Scenario1 <- function(filestr1, ...) {
    results <- Scenario(filestr1 = filestr1, county1 = county1, county.dt = county.dt, doses.dt = doses.dt, k_mu_beta_inter = k_mu_beta_inter, ...)
    results.dt <<- rbind(results.dt, results)
  }

  results.dt <- NULL
  lemma <- Scenario(filestr1 = "statusquo", county1 = county1, county.dt = county.dt, doses.dt = doses.dt, remote = remote, writedir = writedir)

  relative.contact.rate.statusquo <- lemma$fit.extended$par$beta / (lemma$fit.extended$par$beta[1] * lemma$inputs$vaccines$transmission_variant_multiplier)
  k_mu_beta_inter <- 1 / pmin(1, tail(relative.contact.rate.statusquo, 1))

  Scenario1("base", lemma_statusquo = lemma, remote = remote, writedir = writedir)
  Scenario1("open90percent", lemma_statusquo = lemma, k_max_open = 0.9, remote = remote, writedir = writedir)
  Scenario1("uptake83", lemma_statusquo = NULL, k_uptake = "high", remote = remote, writedir = writedir) #refit - can change age dist
  Scenario1("UKvariant", lemma_statusquo = lemma, k_ukgrowth = 1.06, remote = remote, writedir = writedir)
  Scenario1("BRvariant", lemma_statusquo = lemma, k_brgrowth = 1.06, remote = remote, writedir = writedir)

  if (county1 == "San Francisco") {
    Scenario1("uptake83_open90percent", lemma_statusquo = NULL, k_uptake = "high", k_max_open = 0.9, remote = remote, writedir = writedir)
    Scenario1("uptake83_open90percent_UKvariant", lemma_statusquo = NULL, k_uptake = "high", k_max_open = 0.9, k_ukgrowth = 1.06, remote = remote, writedir = writedir)
    Scenario1("uptake83_UKvariant", lemma_statusquo = NULL, k_uptake = "high", k_ukgrowth = 1.06, remote = remote, writedir = writedir)
    Scenario1("open90percent_UKvariant", lemma_statusquo = NULL, k_max_open = 0.9, k_ukgrowth = 1.06, remote = remote, writedir = writedir)
    Scenario1("uptake83_BRvariant", lemma_statusquo = NULL, k_uptake = "high", k_brgrowth = 1.06, remote = remote, writedir = writedir)
    Scenario1("uptake83_open90percent_BRvariant", lemma_statusquo = NULL, k_uptake = "high", k_max_open = 0.9, k_brgrowth = 1.06, remote = remote, writedir = writedir)

    prev.width <- getOption("width")
    options(scipen = 3, width = 9999)

    if (remote) {
      scen_path <- paste0(writedir, "/Scenarios")
      if (dir.exists(scen_path)) {
        sink(file = paste0(scen_path, "/San Francisco_ScenarioSummary.txt"))
      }
    } else {
      sink("Scenarios/San Francisco_ScenarioSummary.txt")
    }

    print(results.dt, digits=0)
    options(width = prev.width)

    cat("base = 75% open by June 22; uptake: 78% for <65, 90% for 65+; 40% UK, 30% West Coast variants; 12-15 eligible May 13, 0-11 eligible Jan 1 \n")
    cat("other scenarios same as base except:\n")
    cat("open90percent = 90% open\n")
    cat("uptake83 = uptake 83% for <65\n")
    cat("UKvariant = UK variant dominant by August\n")
    cat('BRvariant = "Brazil-like" (near worst case) variant dominant by August (possible but unlikely)\n')
    sink()
  }
  return(lemma)
}


#' @title Run a county scenario (adjustable for Shiny interface)
#' @param county1 a character string giving the name of the county
#' @param county.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetCountyData}}
#' @param doses.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetDosesData}}
#' @param k_uptake a character string, "low" or "high" giving vaccine uptake
#' @param k_ukgrowth growth rate of UK variant
#' @param k_brgrowth growth rate of BR variant
#' @param k_max_open percentage of pre-pandemic activity after reopening (scales contact rate)
#' @param vaccine_uptake a numeric vector with 3 values, for vaccine uptake in age groups 12-15, 16-64, and 65+; if not \code{NULL}
#' this will override the option \code{k_uptake}
#' @param vaccine_dosing_jj daily increase in J&J vaccine delivery, leave \code{NULL} for default
#' @param vaccine_dosing_mrna daily increase in mRNA vaccines delivery, leave \code{NULL} for default
#' @param remote a logical value, if \code{TRUE} download all data from remotes, otherwise use local data
#' @param writedir a character string giving a directory to write to, it should only be used if \code{remote} is \code{TRUE}.
#' This assumes the directory whose path is given already exists.
#' @export
RunOneCounty_scen_input <- function(
  county1, county.dt, doses.dt,
  k_uptake = "low", k_ukgrowth = 1, k_brgrowth = 1, k_max_open = 0.75,
  vaccine_uptake = NULL, vaccine_dosing_jj = NULL, vaccine_dosing_mrna = NULL,
  remote = FALSE, writedir = NULL) {

  if (remote & is.null(writedir)) {
    stop("if 'remote' is TRUE, please provide a directory to write results to in 'writedir'")
  }

  lemma <- Scenario(
    filestr1 = "statusquo", county1 = county1, county.dt = county.dt, doses.dt = doses.dt, remote = remote, writedir = writedir
  )

  relative.contact.rate.statusquo <- lemma$fit.extended$par$beta / (lemma$fit.extended$par$beta[1] * lemma$inputs$vaccines$transmission_variant_multiplier)
  k_mu_beta_inter <- 1 / pmin(1, tail(relative.contact.rate.statusquo, 1))

  Scenario(
    filestr1 = "custom", lemma_statusquo = NULL, county1 = county1, county.dt = county.dt, doses.dt = doses.dt,
    k_mu_beta_inter = k_mu_beta_inter,k_uptake = k_uptake, k_ukgrowth = k_ukgrowth, k_brgrowth = k_brgrowth, k_max_open = k_max_open,
    vaccine_uptake = vaccine_uptake, vaccine_dosing_jj = vaccine_dosing_jj, vaccine_dosing_mrna = vaccine_dosing_mrna,
    remote = remote, writedir = writedir
  )

  return(lemma)
}


#' @title Run a scenario
#' @description This function is called from \code{\link[LEMMA.forecasts]{RunOneCounty_scen}}.
#' @param filestr1 a character string giving the type of scenario to run
#' @param county1 a character string giving the name of the county
#' @param county.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetCountyData}}
#' @param doses.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetDosesData}}
#' @param k_mu_beta_inter multiplier to contact rate required to get to 100\% reopening
#' @param lemma_statusquo either \code{NULL} or the result of a call to \code{Scenario} with \code{filestr1 = "statusquo"}
#' @param k_uptake a character string, "low" or "high" giving vaccine uptake
#' @param k_ukgrowth growth rate of UK variant
#' @param k_brgrowth growth rate of BR variant
#' @param k_max_open percentage of pre-pandemic activity after reopening (scales contact rate)
#' @param vaccine_uptake a numeric vector with 3 values, for vaccine uptake in age groups 12-15, 16-64, and 65+; if not \code{NULL}
#' this will override the option \code{k_uptake}
#' @param vaccine_dosing_jj daily increase in J&J vaccine delivery, leave \code{NULL} for default
#' @param vaccine_dosing_mrna daily increase in mRNA vaccines delivery, leave \code{NULL} for default
#' @param remote a logical value, if \code{TRUE} download all data from remotes, otherwise use local data
#' @param writedir a character string giving a directory to write to, it should only be used if \code{remote} is \code{TRUE}.
#' This assumes the directory whose path is given already exists.
#' @return a named list of values
Scenario <- function(
  filestr1, county1, county.dt, doses.dt,
  k_mu_beta_inter = NULL, lemma_statusquo = NULL, k_uptake = "low",
  k_ukgrowth = 1, k_brgrowth = 1, k_max_open = 0.75,
  vaccine_uptake = NULL, vaccine_dosing_jj = NULL, vaccine_dosing_mrna = NULL,
  remote = FALSE, writedir = NULL
) {

  inputs <- GetCountyInputs_scen(
    county1 = county1, county.dt = county.dt, doses.dt = doses.dt, k_uptake = k_uptake,
    k_ukgrowth = k_ukgrowth, k_brgrowth = k_brgrowth,
    vaccine_uptake = vaccine_uptake, vaccine_dosing_jj = vaccine_dosing_jj, vaccine_dosing_mrna = vaccine_dosing_mrna,
    remote = remote, writedir = writedir
  )

  if (filestr1 == "statusquo") {
    lemma <- LEMMA:::CredibilityInterval(inputs)
    return(lemma)
  }


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
  if (F) {
    tier_date <- as.Date("2021/5/15") #after 5/15, delete this and change to one intervention on 6/15
    mu_beta <- sqrt(pmax(1, k_mu_beta_inter * k_max_open))
    new.int <- data.table(mu_t_inter = c(tier_date, as.Date("2021/6/15")),
                          sigma_t_inter = 2, mu_beta_inter = c(mu_beta, mu_beta), sigma_beta_inter = 1e-04,
                          mu_len_inter = 7, sigma_len_inter = 2)
  } else {
    mu_beta <- pmax(1, k_mu_beta_inter * k_max_open)
    new.int <- data.table(mu_t_inter = as.Date("2021/6/15"),
                          sigma_t_inter = 2, mu_beta_inter = mu_beta, sigma_beta_inter = 1e-04,
                          mu_len_inter = 7, sigma_len_inter = 2)
  }


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
  dt[, type := ifelse(date >= inputs$obs.data[, max(date) - 7], "Scenario", "Estimate")]
  print(ggplot(dt, aes(x = date, y = relative.contact.rate)) + geom_line(aes(color = type), size = 2) + scale_x_date(date_breaks = "1 month", date_labels = "%b") + ggtitle("Effective contact rate relative to initial effective contact rate\nnot including vaccine or variant effects") + xlab("") + ylab("Effective Contact Rate") + theme(legend.title = element_blank()))

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
