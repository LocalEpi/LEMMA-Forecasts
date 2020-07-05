library(data.table)
library(ParallelLogger)

source('Code/GetCountyData.R')

quick.test <- F
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
    sheets$Interventions <- sheets$Interventions[2:.N]
    sheets$Interventions[1, mu_beta_inter := 0.5] #LA needs more informative prior to converge
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
  } else if (county1 %in% c("Yolo", "Yuba", "El Dorado", "Lake", "Tuolumne")) {
    # county.dt1 <- county.dt1[date >= as.Date("2020/5/1")] #noisy data early on
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

  max.date <- max(cred.int$inputs$obs.data$date)
  outfile <- paste0("Scenarios/", county1)
  cred.int$inputs$model.inputs$end.date <- as.Date("2020/12/31")

  ProjScen <- function(int.date) {
    if (is.na(int.date)) {
      intervention <- NULL
      subtitl <- "Scenario: No change from current Re"
    } else {
      intervention <- data.frame(mu_t_inter = int.date, sigma_t_inter = 0.0001, mu_beta_inter = 0.5, sigma_beta_inter = 0.0001, mu_len_inter = 7, sigma_len_inter = 2)
      subtitl <- paste("Scenario: Reduce Re by 50% starting", as.character(as.Date(int.date), format = "%b%e"))
    }
    lapply(LEMMA:::ProjectScenario(cred.int, new.int=intervention, "Logs/temp")$gplot$long.term, function (z) z + ggplot2::labs(subtitle = subtitl))
  }

  scen.plots <- lapply(list(NA, max.date + 3, max.date + 17), ProjScen)
  grDevices::pdf(file = paste0("Scenarios/", county1, "_scenarios.pdf"), width = 9.350, height = 7.225)
  print(scen.plots)
  dev.off()

  sink()
  ParallelLogger::logInfo("county = ", county1)
  return(cred.int)
}

county.dt <- GetCountyData(exclude.set)
county.set <- unique(county.dt$county)

if (quick.test) county.set <- c("Butte",  "Kern")

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
