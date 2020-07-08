library(data.table)
library(ParallelLogger)

source('Code/GetCountyData.R')

quick.test <- F
if (quick.test) {
  cat("\n\n++++++++++++++++++  quick.test = T +++++++++++++++++ \n\n")
}

exclude.set <- c("Nevada", "El Dorado", "Yolo", "Yuba", "Tuolumne") #not enough data to fit

RunOneCounty <- function(county1, county.dt, county.pop, quick.test) {
  sink.file <- paste0("Logs/progress-", county1, ".txt")
  sink(sink.file)
  cat("county = ", county1, "\n")
  if (county1 == "San Francisco") {
    input.file <- "Inputs/SF.xlsx"  #SF is separate because it has hospital transfer data manually entered from https://data.sfgov.org/COVID-19/COVID-19-Hospitalizations/nxjg-bhem
    cred.int <- LEMMA::CredibilityIntervalFromExcel(input.file)
  } else {
    input.file <- "Inputs/CAcounties.xlsx"
    sheets <- LEMMA:::ReadInputs(input.file)
    county.pop1 <- county.pop[county == county1, population]
    sheets$`Model Inputs`[internal.name == "total.population", value := county.pop1]
    county.dt1 <- county.dt[county == county1, -1]
    county.dt1[, deaths.pui := NA_integer_]
    county.dt1[, cum.admits.conf := NA_integer_]
    county.dt1[, cum.admits.pui := NA_integer_]
    if (county1 == "Los Angeles") {
      #LA has convergence problems
      sheets$Interventions <- sheets$Interventions[2:.N]
      sheets$Interventions[1, mu_beta_inter := 0.5] #more informative prior
      sheets$`Parameters with Distributions`[7, `Standard Deviation` := 0.005] #reduce frac_hosp sd
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
    if (quick.test) {
      inputs$internal.args$iter <- 300
      inputs$internal.args$max_treedepth <- 10
      inputs$internal.args$adapt_delta <- 0.8
    }
    inputs$internal.args$warmup <- NA #defaults to iter/2
    if (county1 == "Los Angeles") {
      inputs$internal.args$warmup <- round(inputs$internal.args$iter * 0.75)
    }
    inputs$internal.args$output.filestr <- paste0("Forecasts/", county1)
    mean.ini <- 1e-5 * county.pop1
    inputs$internal.args$lambda_ini_exposed <- 1 / mean.ini

    cred.int <- LEMMA:::CredibilityInterval(inputs)
  }

  max.date <- max(cred.int$inputs$obs.data$date)
  outfile <- paste0("Scenarios/", county1)
  cred.int$inputs$model.inputs$end.date <- as.Date("2020/12/31")

  ProjScen <- function(int.list) {
    int.date <- int.list$date
    int.str <- int.list$str
    if (is.na(int.date)) {
      intervention <- NULL
      subtitl <- "Scenario: No change from current Re"
    } else {
      intervention <- data.frame(mu_t_inter = int.date, sigma_t_inter = 0.0001, mu_beta_inter = 0.5, sigma_beta_inter = 0.0001, mu_len_inter = 7, sigma_len_inter = 2)
      subtitl <- paste("Scenario: Reduce Re by 50% starting", as.character(as.Date(int.date), format = "%b%e"))
    }
    lapply(LEMMA:::ProjectScenario(cred.int, new.int=intervention, paste0("Scenarios/", county1, "_scenario_", int.str))$gplot$long.term, function (z) z + ggplot2::labs(subtitle = subtitl))
  }

  scen.plots <- lapply(list(list(date = NA, str = "noChange"), list(date = max.date + 3, str = "actToday"), list(date = max.date + 17, str = "actTwoWeeks")), ProjScen)
  grDevices::pdf(file = paste0("Scenarios/", county1, "_scenarios_summary.pdf"), width = 9.350, height = 7.225) #overwrite the .pdf
  print(scen.plots)
  dev.off()

  sink()
  ParallelLogger::logInfo("county = ", county1)
  return(cred.int)
}

county.dt <- GetCountyData(exclude.set)
county.set <- unique(county.dt$county)

if (quick.test) county.set <- c("San Francisco")

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
