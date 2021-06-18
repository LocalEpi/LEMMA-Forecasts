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
#' @param k_ukgrowth growth rate of UK variant (B.1.1.7)
#' @param k_brgrowth growth rate of BR variant (P.1)
#' @param k_ingrowth growth rate of IN variant (B.1.617.2)
#' @param vaccine_uptake a numeric vector with 3 values, for vaccine uptake in age groups 12-15, 16-64, and 65+ (if a value is -1, keep as is)
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
  county1, county.dt, doses.dt, k_ukgrowth, k_brgrowth, k_ingrowth,
  k_in_trans, k_in_hosp, k_duration_years, k_beta_mult,
  vaccine_uptake = NULL,
  vaccine_dosing = NULL,
  remote = FALSE, writedir = NULL
) {

  sheets <- GetCountySheets(county1, county.dt, doses.dt,remote = remote)

  # vaccine uptake; use low/high or 3 age groups input?
  if (!is.null(vaccine_uptake)) {
    stopifnot(all(is.finite(vaccine_uptake)))
    stopifnot(length(vaccine_uptake)==3L)

    if (vaccine_uptake[1] != -1) sheets$`Vaccine Distribution`[12 <= age & age <= 15, vax_uptake := vaccine_uptake[1]]
    if (vaccine_uptake[2] != -1) sheets$`Vaccine Distribution`[16 <= age & age <= 64, vax_uptake := vaccine_uptake[2]]
    if (vaccine_uptake[3] != -1) sheets$`Vaccine Distribution`[age >= 65, vax_uptake := vaccine_uptake[3]]
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


  #these have to be positive or growth won't matter
  # if (k_ukgrowth > 0) stopifnot(sheets$Variants[name == "alpha", frac_on_day0 > 0])
  # if (k_brgrowth > 0) stopifnot(sheets$Variants[name == "gamma", frac_on_day0 > 0])
  # if (k_ingrowth > 0) stopifnot(sheets$Variants[name == "delta", frac_on_day0 > 0])

  if (!is.null(k_ukgrowth)) sheets$Variants[name == "alpha", daily_growth_future := k_ukgrowth]
  if (!is.null(k_brgrowth)) sheets$Variants[name == "gamma", daily_growth_future := k_brgrowth]
  if (!is.null(k_ingrowth)) sheets$Variants[name == "delta", daily_growth_future := k_ingrowth]

  if (!is.null(k_in_trans)) sheets$Variants[name == "delta", transmisson_mult := k_in_trans]
  if (!is.null(k_in_hosp)) sheets$Variants[name == "delta", hosp_mult := k_in_hosp]
  if (!is.null(k_duration_years)) {
    sheets$Variants[, duration_vaccinated_years := k_duration_years]
    sheets$Variants[, duration_natural_years := k_duration_years]
  }

  if (!is.null(k_beta_mult)) {
    index <- which(sheets$Interventions$mu_t_inter == as.Date("2021/6/15"))
    if (length(index) != 1) {
      stop("there should be an intervention with date 2021/6/15")
    }
    sheets$Interventions[index, mu_beta_inter := k_beta_mult]
  }

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
GetResults_scen <- function(projection, name, relative.contact.rate.cur, relative.contact.rate.final) {
  projection1 <- projection[date >= Sys.Date()]
  hosp.peak <- projection1[, max(hosp)]
  hosp.peak.date <- projection1[, date[which.max(hosp)]]
  admits.byNov1 <- projection1[date <= as.Date("2021/11/1"), sum(admits)]
  deaths.byNov1 <- projection1[date <= as.Date("2021/11/1"), max(deaths) - min(deaths)]
  cases.byNov1 <- projection1[date <= as.Date("2021/11/1"), max(totalCases) - min(totalCases)]
  return(data.table(name, hosp.peak, hosp.peak.date, admits.byNov1, deaths.byNov1, cases.byNov1, rel.cont.rate.cur = relative.contact.rate.cur, rel.cont.rate.new = relative.contact.rate.final))
}
