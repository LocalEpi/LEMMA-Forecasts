GetInputsRestart <- function(inputs.orig, restart.date, end.date, initial.state) {
  inputs <- copy(inputs.orig)

  inputs$obs.data <- inputs$obs.data[date >= restart.date & date <= end.date]
  inputs$internal.args$simulation.start.date <- restart.date - 1
  inputs$interventions <- inputs$interventions[mu_t_inter >= restart.date & mu_t_inter <= end.date]
  inputs$vaccines <- inputs$vaccines[date >= restart.date & date <= end.date]
  inputs$model.inputs$start.display.date <- restart.date

  if (is.null(initial.state)) {
    inputs$initial.state <- list(mu_iniE = 1e-5 * inputs$model.inputs$total.population, from_beginning = 1)
  } else {
    initial.state$from_beginning <- 0
    inputs$initial.state <- initial.state
    inputs$params[name == "r0", mu := 1]
    inputs$params[name == "r0", sigma := 0.1]
  }

  inputs$model.inputs$end.date <- end.date
  inputs$internal.args$output.filestr <- paste0(inputs$internal.args$output.filestr, "_", restart.date)
  return(inputs)
}
