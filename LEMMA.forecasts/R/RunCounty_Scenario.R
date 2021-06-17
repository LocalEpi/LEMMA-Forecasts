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
    scen_list <- Scenario(filestr1 = filestr1, county1 = county1, county.dt = county.dt, doses.dt = doses.dt, remote = remote, writedir = writedir, ...)
    results.dt <<- rbind(results.dt, scen_list$results)
    return(scen_list$lemma)
  }


  results.dt <- NULL
  lemma <- Scenario1("base") #generates forecast
  if (!remote) {
    for (ext in c("pdf", "xlsx")) {
      file.copy(paste0("Scenarios/", county1, "_base.", ext), paste0("Forecasts/", county1, ".", ext), overwrite = T)
    }
  }

  if (county1 == "San Francisco") {
    k_uptake_increase_set <- c(F, T)
    k_beta_mult_set <- c(1.05, 1.1, 1.2)
  } else {
    k_uptake_increase_set <- F
    k_beta_mult_set <- 1.1
  }
  for (delta_epi_optimistic in c(T, F)) {
    for (k_beta_mult in k_beta_mult_set) {
      for (k_uptake_increase in k_uptake_increase_set) {
        if (delta_epi_optimistic) {
          k_in_hosp <- 1.3
          k_in_trans <- 2.1
          k_duration_years <- 999
          delta_epi_optimistic_str <- "epiOptimistic"
        } else {
          k_in_hosp <- 2.6
          k_in_trans <- 2.4
          k_duration_years <- NULL
          delta_epi_optimistic_str <- "epiPessimistic"
        }
        if (k_uptake_increase) {
          k_vaccine_uptake <- c(0.85, 0.85, -1) #keep 65+ as is
          k_uptake_increase_str <- "uptake85"
        } else {
          k_vaccine_uptake <- NULL
          k_uptake_increase_str <- "uptakeCurrent"
        }
        name <- paste0(delta_epi_optimistic_str, "_reopenInc", 100 * (k_beta_mult - 1), "_", k_uptake_increase_str)
        Scenario1(name, k_in_hosp = k_in_hosp, k_in_trans = k_in_trans, k_duration_years = k_duration_years, vaccine_uptake = k_vaccine_uptake, k_beta_mult = k_beta_mult)
      }
    }
  }

  if (county1 == "San Francisco") {
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

    cat("base: Delta is 50% more transmissible than Alpha, no increase in severity over Alpha, waning immunity, 10% increase in effective contact rate on June 15, current vaccine uptake\n")
    cat("epiOptimistic: Delta is 40% more transmissible than Alpha, no increase in severity over Alpha, no waning immunity\n")
    cat("epiPessimistic: Delta is 60% more transmissible than Alpha, hospitalization rate twice Alpha, waning immunity\n")
    cat("reopenInc5: 5% increase in effective contact rate on June 15\n")
    cat("reopenInc10: 10% increase in effective contact rate on June 15\n")
    cat("reopenInc20: 20% increase in effective contact rate on June 15\n")
    cat("uptake85: 85% vaccine uptake in 12-64\n")
    cat("uptakeCurrent: 82% vaccine uptake in 12-64\n")
    cat("All scenarios: vaccine uptake 92% in 65+; age 0-11 eligible Jan 1; Delta 30% today and dominant by July\n")
    cat("rel.cont.rate.cur = relative effective rate today\n")
    cat("rel.cont.rate.new = relative effective rate after reopening\n")
    sink()
  }
  return(lemma)
}


#' @title Run a county scenario (adjustable for Shiny interface)
#' @param county1 a character string giving the name of the county
#' @param county.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetCountyData}}
#' @param doses.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetDosesData}}
#' @param k_ukgrowth growth rate of UK variant (B.1.1.7)
#' @param k_brgrowth growth rate of BR variant (P.1)
#' @param k_ingrowth growth rate of IN variant (B.1.617.2)
#' @param k_max_open percentage of pre-pandemic activity after reopening (scales contact rate)
#' @param vaccine_uptake a numeric vector with 3 values, for vaccine uptake in age groups 12-15, 16-64, and 65+
#' @param vaccine_dosing a named list that requires specific input, see section \code{vaccine_dosing}, or \code{NULL} for no adjustment of doses available
#' @param remote a logical value, if \code{TRUE} download all data from remotes, otherwise use local data
#' @param writedir a character string giving a directory to write to, it should only be used if \code{remote} is \code{TRUE}.
#' This assumes the directory whose path is given already exists.
#' @section vaccine_dosing:
#' This is a named list that requires the following elements
#' \itemize{
#'  \item{vaccine_dosing_jj}{daily increase in J&J vaccine delivery}
#'  \item{vaccine_dosing_mrna}{daily increase in mRNA vaccines delivery}
#'  \item{vaccine_dosing_mrna_max}{increase over baseline in maximum number of doses per day for mRNA vaccines}
#'  \item{vaccine_dosing_jj_max}{increase over baseline in maximum number of doses per day for J&J vaccines}
#' }
#' @export
RunOneCounty_scen_input <- function(
  county1, county.dt, doses.dt,
  k_ukgrowth = 1, k_brgrowth = 1, k_ingrowth = 1, k_max_open = 0.75,
  vaccine_uptake = NULL,
  vaccine_dosing = NULL,
  remote = FALSE,
  writedir = NULL) {

  if (remote & is.null(writedir)) {
    stop("if 'remote' is TRUE, please provide a directory to write results to in 'writedir'")
  }

  lemma <- Scenario(
    filestr1 = "statusquo", county1 = county1, county.dt = county.dt, doses.dt = doses.dt, remote = remote, writedir = writedir
  )$lemma

  relative.contact.rate.statusquo <- lemma$fit.extended$par$beta / lemma$fit.extended$par$beta[1]
  multiplier_to_100open <- 1 / pmin(1, tail(relative.contact.rate.statusquo, 1))
  k_beta_mult <- pmax(1, multiplier_to_100open * k_max_open)

  Scenario(
    filestr1 = "custom", lemma_statusquo = NULL, county1 = county1, county.dt = county.dt, doses.dt = doses.dt,
    k_beta_mult = k_beta_mult, k_ukgrowth = k_ukgrowth, k_brgrowth = k_brgrowth, k_ingrowth = k_ingrowth,
    vaccine_uptake = vaccine_uptake, vaccine_dosing = vaccine_dosing,
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
#' @param k_ukgrowth growth rate of UK variant (B.1.1.7)
#' @param k_brgrowth growth rate of BR variant (P.1)
#' @param k_ingrowth growth rate of IN variant (B.1.617.2)
#' @param k_max_open percentage of pre-pandemic activity after reopening (scales contact rate)
#' @param vaccine_uptake a numeric vector with 3 values, for vaccine uptake in age groups 12-15, 16-64, and 65+
#' @param vaccine_dosing a named list that requires specific input, see section \code{vaccine_dosing}, or \code{NULL} for no adjustment of doses available
#' @param remote a logical value, if \code{TRUE} download all data from remotes, otherwise use local data
#' @param writedir a character string giving a directory to write to, it should only be used if \code{remote} is \code{TRUE}.
#' This assumes the directory whose path is given already exists.
#' @section vaccine_dosing:
#' This is a named list that requires the following elements
#' \itemize{
#'  \item{vaccine_dosing_jj}{daily increase in J&J vaccine delivery}
#'  \item{vaccine_dosing_mrna}{daily increase in mRNA vaccines delivery}
#'  \item{vaccine_dosing_mrna_max}{increase over baseline in maximum number of doses per day for mRNA vaccines}
#'  \item{vaccine_dosing_jj_max}{increase over baseline in maximum number of doses per day for J&J vaccines}
#' }
#' @return a named list of values
Scenario <- function(
  filestr1, county1, county.dt, doses.dt,
  lemma_statusquo = NULL,
  k_ukgrowth = NULL, k_brgrowth = NULL, k_ingrowth = NULL, k_beta_mult = 1,
  k_in_trans = NULL, k_in_hosp = NULL, k_duration_years = NULL,
  vaccine_uptake = NULL,
  vaccine_dosing = NULL,
  remote = FALSE, writedir = NULL
) {

  inputs <- GetCountyInputs_scen(
    county1 = county1, county.dt = county.dt, doses.dt = doses.dt,
    k_ukgrowth = k_ukgrowth, k_brgrowth = k_brgrowth, k_ingrowth = k_ingrowth,
    k_in_trans = k_in_trans, k_in_hosp = k_in_hosp, k_duration_years = k_duration_years,
    vaccine_uptake = vaccine_uptake,
    vaccine_dosing = vaccine_dosing,
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
    if (filestr1 == "base") {
      filestr <- paste0("Forecasts/", county1)
    } else {
      filestr <- paste0("Scenarios/", county1, "_", filestr1)
    }
  }

  inputs$internal.args$output.filestr <- filestr

  index <- which(inputs$interventions$mu_t_inter == as.Date("2021/6/15"))
  if (length(index) != 1) {
    stop("there should be an intervention with date 2021/6/15")
  }
  inputs$interventions$mu_beta_inter[index] <- k_beta_mult

  if (is.null(lemma_statusquo)) {
    #refit
    lemma <- LEMMA:::CredibilityInterval(inputs)
  } else {
    lemma <- LEMMA:::ProjectScenario(lemma_statusquo, inputs)
  }

  pdf(paste0(filestr, ".pdf"), width = 11, height = 8.5)
  relative.contact.rate <- lemma$fit.extended$par$beta / lemma$fit.extended$par$beta[1]
  dt <- data.table(date = lemma$projection$date, relative.contact.rate)
  relative.contact.rate.cur <- round(100 * dt[date == inputs$obs.data[, max(date) - 7], relative.contact.rate])
  relative.contact.rate.final <- round(100 * dt[date == max(date), relative.contact.rate])
  dt[, type := ifelse(date >= inputs$obs.data[, max(date) - 7], "Scenario", "Estimate")]
  print(ggplot(dt, aes(x = date, y = relative.contact.rate)) + geom_line(aes(color = type), size = 2) + scale_x_date(date_breaks = "1 month", date_labels = "%b") + ggtitle("Effective contact rate relative to initial effective contact rate\nnot including vaccine or variant effects") + xlab("") + ylab("Effective Contact Rate") + theme(legend.title = element_blank()) + labs(caption = paste("Current =", relative.contact.rate.cur, "%")))

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

  results <- GetResults_scen(lemma$projection, filestr1, relative.contact.rate.cur, relative.contact.rate.final)
  return(list(results = results, lemma = lemma))
}
