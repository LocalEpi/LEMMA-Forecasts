library(data.table)
library(ParallelLogger)

source('Code/GetCountyData.R')

exclude.set <- c("San Francisco") #SF is run separately
county.dt <- GetCountyData(exclude.set)
saveRDS(county.dt, "Inputs/CountyData.rds")

quick.test <- T
if (quick.test) {
  cat("\n\n++++++++++++++++++  quick.test = T +++++++++++++++++ \n\n")
  county.set <- "Mariposa" #c("Glenn")
} else {
  #order by last Rt date in forecasts and then last run time
  max.date <- county.dt[, max(date)]
  dt.max <- county.dt[date == max.date, ]
  stopifnot(setequal(dt.max$county, unique(county.dt$county)))
  for (i in unique(county.dt$county)) {
    filestr <- paste0("Forecasts/", i, ".xlsx")
    if (file.exists(filestr)) {
      x <- as.data.table(readxl::read_excel(filestr, sheet = "rt"))
      date1 <- max(as.Date(x$date))
    } else {
      date1 <- as.Date("2020/1/1")
    }
    dt.max[county == i, last.rt := date1]
  }
  dt.max[, run.time := sapply(county, GetRunTime)]

  setorder(dt.max, last.rt, -run.time)
  print(dt.max)
  # dt.max <- dt.max[last.rt <= as.Date("2020/12/11")]; print(dt.max) #temp
  sink("Logs/CountySet.txt")
  print(dt.max)
  sink()
  county.set <- dt.max[, county]
}
print(county.set)
cat("Data through", as.character(county.dt[, max(date)]), "\n")

RunOneCounty <- function(county1) {
  try.result <- try({
    county.dt <- readRDS("Inputs/CountyData.rds")
    county.pop <- data.table::fread("Inputs/county population.csv")
    county.dt1 <- county.dt[county == county1, -1]
    county.pop1 <- county.pop[county == county1, population]

    restart.set <- c("Tehama", "Mono", "Yolo", "Yuba", "Mendocino", "Nevada", "El Dorado",
                     "Tuolumne", "Amador", "Inyo", "Calaveras",
                     "Madera", "Humboldt", "Siskiyou", "Butte", "San Benito",
                     "Merced", "Colusa", "Glenn") #infections went to near zero - restart sim
    if (county1 %in% restart.set) {
      if (county1 == "San Benito") {
        restart.date <- as.Date("2020/11/03")
      } else if (county1 %in% c("Amador")) {
        restart.date <- as.Date("2020/11/04")
      } else if (county1 %in% c("Modoc", "Lassen")) {
        restart.date <- as.Date("2020/11/05")
      } else if (county1 == "Lake") {
        restart.date <- as.Date("2020/10/10")
      } else if (county1 == "El Dorado") {
        restart.date <- as.Date("2020/10/24")
      } else if (county1 == "Merced") {
        restart.date <- as.Date("2020/5/1")
      } else if (county1 == "Colusa") {
        restart.date <- as.Date("2020/11/9")
      } else if (county1 == "Glenn") {
        restart.date <- as.Date("2020/12/4")
      } else {
        restart.date <- as.Date("2020/6/1")
      }
    }
    sink.file <- paste0("Logs/progress-", county1, ".txt")
    sink(sink.file)
    cat("county = ", county1, "\n")

    input.file <- "Inputs/CAcounties.xlsx"
    sheets <- LEMMA:::ReadInputs(input.file)
    sheets$`Model Inputs`[internal.name == "total.population", value := county.pop1]

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
      county.dt1[date < as.Date("2020/9/1"), hosp.conf := NA_integer_] #Imperial was tranferring a lot hospitalized out of county
      county.dt1[date < as.Date("2020/9/1"), hosp.pui := NA_integer_]
      county.dt1[date < as.Date("2020/9/1"), icu.conf := NA_integer_]
      county.dt1[date < as.Date("2020/9/1"), icu.pui := NA_integer_]
    } else if (county1 %in% restart.set) {
      sheets$`Parameters with Distributions`[1, Mean := 1] #R0 = 1
      initial.deaths <- county.dt1[date == (restart.date - 1), deaths.conf]
      county.dt1 <- county.dt1[date >= restart.date] #infections went to near zero - restart sim
      sheets$Internal[internal.name == "simulation.start.date", value := restart.date - 10]
    }

    sheets$Data <- county.dt1
    inputs <- LEMMA:::ProcessSheets(sheets, input.file)

    inputs$internal.args$warmup <- NA #defaults to iter/2
    if (county1 %in% restart.set) {
      inputs$interventions <- inputs$interventions[mu_t_inter >= restart.date]
      inputs$model.inputs$start.display.date <- restart.date
      inputs$internal.args$initial.deaths <- initial.deaths
    }
    if (county1 %in% c("San Mateo", "San Joaquin")) {
      inputs$internal.args$iter <- 1500
    }

    inputs$internal.args$output.filestr <- paste0("Forecasts/", county1)
    mean.ini <- 1e-5 * county.pop1
    inputs$internal.args$lambda_ini_exposed <- 1 / mean.ini

    # inputs$internal.args$warmup <- NA
    # inputs$internal.args$iter <- 500
    # inputs$internal.args$max_treedepth <- 10
    # inputs$internal.args$adapt_delta <- 0.8

    cred.int <- LEMMA:::CredibilityInterval(inputs)


    max.date <- max(cred.int$inputs$obs.data$date)
    outfile <- paste0("Scenarios/", county1)

    ProjScen <- function(int.list) {
      int.date <- int.list$date
      int.str <- int.list$str
      intervention <- data.frame(mu_t_inter = int.date, sigma_t_inter = 0.0001, mu_beta_inter = 0.5, sigma_beta_inter = 0.0001, mu_len_inter = 7, sigma_len_inter = 2)
      subtitl <- paste("Scenario: Reduce Re by 50% starting", as.character(as.Date(int.date), format = "%b%e"))
      lapply(LEMMA:::ProjectScenario(cred.int, new.int=intervention, paste0("Scenarios/", county1, "_scenario_", int.str))$gplot$long.term, function (z) z + ggplot2::labs(subtitle = subtitl))
    }

    scen.plots <- lapply(list(list(date = max.date + 3, str = "actToday"), list(date = max.date + 17, str = "actTwoWeeks")), ProjScen)

    sink()
    ParallelLogger::logInfo("county = ", county1)

    commit.name <- paste0('"', county1, " data through ", as.character(max.date), '"')
    system2("git", args = c('commit', '-a', '-m', commit.name))
    system2("git", args = "pull")
    system2("git", args = "push")
  })

  if (inherits(try.result, "try-error")) {
    ParallelLogger::logInfo("ERROR in = ", county1)
    ParallelLogger::logInfo(as.character(try.result))
  }
  return(NULL)
}


options(warn = 1)
assign("last.warning", NULL, envir = baseenv())

logfile <- "Logs/logger.txt"
unlink(logfile)
clearLoggers()
addDefaultFileLogger(logfile)

if (length(county.set) == 1) {
  county.results <- lapply(county.set, RunOneCounty)
} else {
  num.clusters <- floor(parallel::detectCores() / 4) - 1
  cat("num.clusters = ", num.clusters, "\n")
  cl <- makeCluster(num.clusters)
  county.results <- clusterApply(cl, county.set, RunOneCounty)
  stopCluster(cl)
}
names(county.results) <- county.set
cat("Data through", as.character(county.dt[, max(date)]), "\n")
unregisterLogger(1)

dt <- fread("Logs/logger.txt")
setnames(dt, c("time", "threadLabel", "level", "packageName", "functionName", "message"))
setkey(dt, threadLabel, time)



for (i in 1:nrow(dt)) {
  if (dt[i, level == "WARN" & IsBad(message)]) {
    cat(dt[i, message], "\n")
    next.county <- dt[, grep("county = ", message)]
    next.county <- min(next.county[next.county > i])
    cat(dt[next.county, message], "\n\n")
  }
}

cat("\n\nData through", as.character(county.dt[, max(date)]), "\n")

