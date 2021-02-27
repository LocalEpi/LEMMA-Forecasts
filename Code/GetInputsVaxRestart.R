Get1 <- function(zz) {
  stopifnot(uniqueN(zz) == 1)
  zz[1]
}

GetInputsVaxRestart <- function(county1, county.dt, restart.date, end.date, initial.state, vaccines) {
  county.pop1 <- county.dt[county == county1, Get1(population)]
  if (county1 == "San Francisco") {
    input.file <- "Inputs/SF_sigmaobs.xlsx"
    sheets <- LEMMA:::ReadInputs(input.file)
  } else {
    input.file <- "Inputs/CAcounties_sigmaobs.xlsx"
    sheets <- LEMMA:::ReadInputs(input.file)
    county.dt1 <- county.dt[county == county1, .(date, hosp.conf, hosp.pui, icu.conf, icu.pui,  deaths.conf)]
    county.dt1[!is.na(deaths.conf), deaths.pui := 0]
    county.dt1[, cum.admits.conf := NA_integer_]
    county.dt1[, cum.admits.pui := NA_integer_]
    sheets$Data <- county.dt1

    sheets$`Model Inputs`[internal.name == "total.population", value := county.pop1]
  }

  sheets$Data <- sheets$Data[date >= restart.date & date <= end.date]
  sheets$Data[is.na(deaths.pui) & !is.na(deaths.conf), deaths.pui := 0]
  sheets$Internal[internal.name == "simulation.start.date", value := restart.date - 1]

  inputs <- LEMMA:::ProcessSheets(sheets, input.file)
  inputs$internal.args$iter <- 40 #1000

  inputs$interventions <- inputs$interventions[mu_t_inter >= restart.date & mu_t_inter <= end.date]
  inputs$model.inputs$start.display.date <- restart.date

  if (is.null(initial.state)) {
    inputs$initial.state <- list(mu_iniE = 1e-5 * county.pop1, from_beginning = 1)
  } else {
    initial.state$from_beginning <- 0
    inputs$initial.state <- initial.state
  }

  inputs$model.inputs$end.date <- end.date
  inputs$vaccines <- vaccines
  return(inputs)
}
