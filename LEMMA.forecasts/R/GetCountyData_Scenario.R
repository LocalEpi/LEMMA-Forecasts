# --------------------------------------------------------------------------------
#   Functions to prepare data for running scenarios:
#   1. GetCountyInputs_scen
#   3. GetResults_scen
# --------------------------------------------------------------------------------

# #' @param vaccine_dosing_jj a numeric vector with 2 values giving future dosing information for J&J vaccine, the first
# #' value is the daily increase in number of doses and second is the maximum number of doses per day
# #' @param vaccine_dosing_mrna a numeric vector with 2 values giving future dosing information for mRNA based vaccines, the first
# #' value is the daily increase in number of doses and second is the maximum number of doses per day

#' @title Get county inputs for scenarios
#' @description This function is called from \code{\link[LEMMA.forecasts]{Scenario}}.
#' @param county1 a character string giving the name of the county
#' @param county.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetCountyData}}
#' @param doses.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetDosesData}}
#' @param k_uptake a character string, "low" or "high" giving vaccine uptake
#' @param k_ukgrowth growth rate of UK variant (B.1.1.7)
#' @param k_brgrowth growth rate of BR variant (P.1)
#' @param k_ingrowth growth rate of IN variant (B.1.617.2)
#' @param vaccine_uptake a numeric vector with 3 values, for vaccine uptake in age groups 12-15, 16-64, and 65+; if not \code{NULL}
#' this will override the option \code{k_uptake}
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
GetCountyInputs_scen <- function(
  county1, county.dt, doses.dt, k_uptake, k_ukgrowth, k_brgrowth, k_ingrowth,
  vaccine_uptake = NULL,
  vaccine_dosing = NULL,
  remote = FALSE, writedir = NULL
) {

  sheets <- GetCountySheets(county1, county.dt, doses.dt,remote = remote)

  # vaccine uptake; use low/high or 3 age groups input?
  if (is.null(vaccine_uptake)) {

    stopifnot(k_uptake %in% c("low", "high"))
    #uptake in 65+ is set in CAcounties.xlsx
    if (k_uptake == "low") {
      sheets$`Vaccine Distribution`[age < 65, vax_uptake := 0.78]
    } else {
      sheets$`Vaccine Distribution`[age < 65, vax_uptake := 0.83]
    }

  } else {

    stopifnot(all(is.finite(vaccine_uptake)))
    stopifnot(length(vaccine_uptake)==3L)

    sheets$`Vaccine Distribution`[12 <= age & age <= 15, vax_uptake := vaccine_uptake[1]]
    sheets$`Vaccine Distribution`[16 <= age & age <= 64, vax_uptake := vaccine_uptake[2]]
    sheets$`Vaccine Distribution`[age >= 65, vax_uptake := vaccine_uptake[3]]

  }

  # vaccine dosing: user input?
  if (!is.null(vaccine_dosing)) {

    stopifnot(all(sapply(X = vaccine_dosing,FUN = is.finite)))

    sheets$`Vaccine Doses - Future`[internal.name == "doses_per_day_increase", jj := vaccine_dosing$vaccine_dosing_jj]
    sheets$`Vaccine Doses - Future`[internal.name == "start_increase_day", jj := as.Date(Sys.time())]
    sheets$`Vaccine Doses - Future`[internal.name == "doses_per_day_maximum", jj := as.numeric(jj) + vaccine_dosing$vaccine_dosing_jj_max]

    sheets$`Vaccine Doses - Future`[internal.name == "doses_per_day_increase", mrna := vaccine_dosing$vaccine_dosing_mrna]
    sheets$`Vaccine Doses - Future`[internal.name == "start_increase_day", mrna := as.Date(Sys.time())]
    sheets$`Vaccine Doses - Future`[internal.name == "doses_per_day_maximum", mrna := as.numeric(mrna) + vaccine_dosing$vaccine_dosing_mrna_max]

  }

  # download from remote?
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
  india <- x[index, B.1.617.2] / 100
  wild <- 1 - (uk + ca + br + sa + india)
  max.date <- sheets$Data[, max(date)]
  sheets$Variants[, variant_day0 := max.date]
  sheets$Variants[, frac_on_day0 := c(wild, uk, ca, br, sa, india)]
  sheets$Variants[, daily_growth_prior := c(1, 1.05, 1.034, 1.03, 1.03, 1.03)]
  sheets$Variants[, daily_growth_future := 1]

  #these have to be positive or growth won't matter
  if (k_ukgrowth > 0) stopifnot(sheets$Variants[name == "UK", frac_on_day0 > 0])
  if (k_brgrowth > 0) stopifnot(sheets$Variants[name == "BR", frac_on_day0 > 0])
  if (k_ingrowth > 0) stopifnot(sheets$Variants[name == "IN", frac_on_day0 > 0])

  sheets$Variants[name == "UK", daily_growth_future := k_ukgrowth]
  sheets$Variants[name == "BR", daily_growth_future := k_brgrowth]
  sheets$Variants[name == "IN", daily_growth_future := k_ingrowth]

  inputs <- LEMMA:::ProcessSheets(sheets)
  inputs <- ModifyCountyInputs(county1, inputs)

  if (!is.null(writedir)) {
    forecast_path <- paste0(writedir, "/Forecasts")
    dir.create(path = forecast_path,showWarnings = FALSE)
    inputs$internal.args$output.filestr <- paste0(forecast_path, "/", county1)
  } else {
    inputs$internal.args$output.filestr <- paste0("Forecasts/", county1)
  }

  return(inputs)
}


#' @title Get results of running a scenario
#' @description This function is called from \code{\link[LEMMA.forecasts]{Scenario}}.
#' @param projection output of a LEMMA model run
#' @param name a character string giving the type of scenario to run
GetResults_scen <- function(projection, name) {
  projection1 <- projection[date >= Sys.Date()]
  hosp.peak <- projection1[, max(hosp)]
  hosp.peak.date <- projection1[, date[which.max(hosp)]]
  additional.admits.byNov1 <- projection1[date <= as.Date("2021/11/1"), sum(admits)]
  additional.deaths.byNov1 <- projection1[date <= as.Date("2021/11/1"), max(deaths) - min(deaths)]
  additional.cases.byNov1 <- projection1[date <= as.Date("2021/11/1"), max(totalCases) - min(totalCases)]
  return(data.table(name, hosp.peak, hosp.peak.date, additional.admits.byNov1, additional.deaths.byNov1, additional.cases.byNov1))
}
