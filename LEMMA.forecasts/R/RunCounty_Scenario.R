# --------------------------------------------------------------------------------
#   Run a scenario
#   1. RunOneCounty_scen
# --------------------------------------------------------------------------------


#' @title Run a county scenario
#' @param county1 a character string giving the name of the county
#' @param county.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetCountyData}}
#' @param doses.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetDosesData}}
#' @param remote a logical value, if \code{TRUE} download all data from remotes, otherwise use local data
#' @param writedir a character string giving a directory to write to. Ignored if \code{remote} is \code{FALSE}
#' @export
RunOneCounty_scen <- function(county1, county.dt, doses.dt, remote = FALSE, writedir = NULL) {

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
  Scenario1("uptake85", lemma_statusquo = NULL, k_uptake = "high", remote = remote, writedir = writedir) #refit - can change age dist
  Scenario1("UKvariant", lemma_statusquo = lemma, k_ukgrowth = 1.06, remote = remote, writedir = writedir)
  Scenario1("BRvariant", lemma_statusquo = lemma, k_brgrowth = 1.06, remote = remote, writedir = writedir)

  if (county1 == "San Francisco") {
    Scenario1("uptake85_open90percent", lemma_statusquo = NULL, k_uptake = "high", k_max_open = 0.9, remote = remote, writedir = writedir)
    Scenario1("uptake85_open90percent_UKvariant", lemma_statusquo = NULL, k_uptake = "high", k_max_open = 0.9, k_ukgrowth = 1.06, remote = remote, writedir = writedir)
    Scenario1("open90percent_UKvariant", lemma_statusquo = NULL, k_max_open = 0.9, k_ukgrowth = 1.06, remote = remote, writedir = writedir)
    Scenario1("uptake85_BRvariant", lemma_statusquo = NULL, k_uptake = "high", k_brgrowth = 1.06, remote = remote, writedir = writedir)
    Scenario1("uptake85_open90percent_BRvariant", lemma_statusquo = NULL, k_uptake = "high", k_max_open = 0.9, k_brgrowth = 1.06, remote = remote, writedir = writedir)

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
