library(data.table)
library(ParallelLogger)

source('Code/GetCountyData.R')

quick.test <- T
if (quick.test) {
  cat("\n\n++++++++++++++++++  quick.test = T +++++++++++++++++ \n\n")
}

exclude.set <- c("Nevada", #not enough data to fit
                 "San Francisco") #run separately with county data

RunOneCounty <- function(county1, county.dt, county.pop, quick.test) {
  sink.file <- paste0("Logs/progress-", county1, ".txt")
  sink(sink.file)
  cat("county = ", county1, "\n")
  input.file <- "Inputs/CAcounties.xlsx"
  sheets <- LEMMA:::ReadInputs(input.file)
  county.pop1 <- county.pop[county == county1, population]
  sheets$`Model Inputs`[internal.name == "total.population", value := county.pop1]
  county.dt1 <- county.dt[county == county1, -1]
  county.dt1[, deaths.pui := NA_integer_]
  county.dt1[, cum.admits.conf := NA_integer_]
  county.dt1[, cum.admits.pui := NA_integer_]
  if (county1 == "Los Angeles") {
    sheets$Interventions[2, mu_beta_inter := 0.5] #LA needs more informative prior to converge
    sheets$`Parameters with Distributions`[9, Mean := 0.64] #mort/ICU seems very high in LA
    sheets$`Parameters with Distributions`[9, `Standard Deviation` := 0.1]
  } else if (county1 == "Imperial") {
    county.dt1[, hosp.conf := NA_integer_] #Imperial is tranferring a lot hospitalized out of county
    county.dt1[, hosp.pui := NA_integer_]
    county.dt1[, icu.conf := NA_integer_]
    county.dt1[, icu.pui := NA_integer_]
  } else if (county1 == "Kings") {
    county.dt1[, icu.conf := NA_integer_] #data error in Kings ICU?
    county.dt1[, icu.pui := NA_integer_]
  } else if (county1 == "San Mateo") {
    sheets$Interventions[7, mu_beta_inter := 1] #very recent increase
    sheets$Interventions[8, mu_beta_inter := 1.5]
  }

  sheets$Data <- county.dt1

  inputs <- LEMMA:::ProcessSheets(sheets, input.file)
  if (quick.test) inputs$internal.args$iter <- 300
  inputs$internal.args$warmup <- NA #defaults to iter/2
  if (county1 == "Los Angeles") {
    inputs$internal.args$warmup <- round(inputs$internal.args$iter * 0.75)
  }
  inputs$internal.args$output.filestr <- paste0("Forecasts/", county1)
  mean.ini <- 1e-5 * county.pop1
  inputs$internal.args$lambda_ini_exposed <- 1 / mean.ini

  cred.int <- LEMMA:::CredibilityInterval(inputs)
  sink()
  ParallelLogger::logInfo("county = ", county1)
  return(cred.int)
}


county.dt <- GetCountyData(exclude.set)
county.set <- unique(county.dt$county)

if (quick.test) county.set <- c("San Mateo", "Kings", "Los Angeles")

county.pop <- fread("Inputs/county population.csv")

options(warn = 1)
assign("last.warning", NULL, envir = baseenv())

logfile <- "Logs/logger.txt"
unlink(logfile)
clearLoggers()
addDefaultFileLogger(logfile)

cl <- makeCluster(3)
z <- clusterApply(cl, county.set, RunOneCounty, county.dt, county.pop, quick.test)
stopCluster(cl)
cat("Data through", as.character(county.dt[, max(date)]), "\n")
unregisterLogger(1)
