library(data.table)
library(ParallelLogger)

source('Code/GetCountyData.R')


county.dt <- GetCountyData(include.regions = F)


exclude.set <- c("San Francisco", "Santa Clara") #SF is run separately, SC uses local data
county.dt <- county.dt[!(county %in% exclude.set)]
county.dt <- rbind(county.dt, GetSantaClaraData())
saveRDS(county.dt, "Inputs/CountyData.rds")

quick.test <- F
if (quick.test) {
  cat("\n\n++++++++++++++++++  quick.test = T +++++++++++++++++ \n\n")
  county.set <- c("San Mateo")
} else {
  #order by last Rt date in forecasts and then last run time
  dt.max <- merge(county.dt, county.dt[, .(date = max(date)), by = "county"], by = c("county", "date"))
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
  sink("Logs/CountySet.txt")
  print(dt.max)
  sink()
  county.set <- dt.max[, county]
  county.set <- c("Colusa", setdiff(county.set, "Colusa")) #run Colusa first as a test (it's fast)
  insuff.data <- c("Glenn", "Mariposa", "Del Norte", "Mono", "Plumas", "Modoc")
  for (i in insuff.data) {
    cat("Excluding", i, "need more data\n")
    print(tail(county.dt[county == i], 10))
  }
  county.set <- setdiff(county.set, insuff.data)
}
print(county.set)

RunOneCounty <- function(county1, quick.test) {
  library(data.table)
  source("Code/GetVaccineParams.R")
  Get1 <- function(zz) {
    stopifnot(uniqueN(zz) == 1)
    zz[1]
  }

  try.result <- try({
    county.dt <- readRDS("Inputs/CountyData.rds")
    county.dt1 <- county.dt[county == county1, -1]

    county.pop1 <- county.dt[county == county1, Get1(population)]
    is.region <- county.dt[county == county1, Get1(is.region)]

    restart.set <- c("Tehama", "Mono", "Yolo", "Yuba", "Mendocino", "Nevada", "El Dorado",
                     "Tuolumne", "Amador", "Inyo", "Calaveras",
                     "Madera", "Humboldt", "Siskiyou", "Butte", "San Benito",
                     "Merced", "Colusa", "Glenn", "Mono", "Plumas", "Shasta", "Tehama") #infections went to near zero - restart sim
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
      } else if (county1 == "Mendocino") {
        restart.date <- as.Date("2020/7/15")
      } else if (county1 == "Mono") {
        restart.date <- as.Date("2020/12/9")
      } else if (county1 == "Plumas") {
        restart.date <- as.Date("2020/12/8")
      } else if (county1 == "Siskiyou") {
        restart.date <- as.Date("2020/10/26")
      } else if (county1 == "Inyo") {
        restart.date <- as.Date("2020/11/19")
      } else if (county1 == "Shasta") {
        restart.date <- as.Date("2020/6/17")
      } else if (county1 == "Tehama") {
        restart.date <- as.Date("2020/9/1")
      } else {
        restart.date <- as.Date("2020/6/1")
      }
    }
    sink.file <- paste0("Logs/progress-", county1, ".txt")
    sink(sink.file)
    cat("county = ", county1, "\n")
    cat("start time = ", as.character(Sys.time() - 3600 * 8), "\n")
    cat("max date = ", as.character(max(county.dt1$date)), "\n")

    input.file <- "Inputs/CAcounties_sigmaobs.xlsx"
    sheets <- LEMMA:::ReadInputs(input.file)
    sheets$`Model Inputs`[internal.name == "total.population", value := county.pop1]

    county.dt1[, deaths.pui := NA_integer_]
    county.dt1[, cum.admits.conf := NA_integer_]
    county.dt1[, cum.admits.pui := NA_integer_]
    if (county1 %in% restart.set) {
      sheets$`Parameters with Distributions`[1, Mean := 1] #R0 = 1
      initial.deaths <- county.dt1[date == (restart.date - 1), deaths.conf]
      county.dt1 <- county.dt1[date >= restart.date] #infections went to near zero - restart sim
      sheets$Internal[internal.name == "simulation.start.date", value := restart.date - 10]
    }

    county.dt1[date < as.Date("2020/8/1"), icu.conf := NA_integer_]
    county.dt1[date < as.Date("2020/8/1"), icu.pui := NA_integer_]
    county.dt1[date < as.Date("2020/8/1"), deaths.conf := NA_integer_]
    county.dt1[date < as.Date("2020/8/1"), deaths.pui := NA_integer_]

    sheets$Data <- county.dt1
    inputs <- LEMMA:::ProcessSheets(sheets, input.file)

    inputs$internal.args$warmup <- NA #defaults to iter/2
    if (county1 %in% restart.set) {
      inputs$interventions <- inputs$interventions[mu_t_inter >= restart.date]
      inputs$model.inputs$start.display.date <- restart.date
      inputs$internal.args$initial.deaths <- initial.deaths
    }


    inputs$internal.args$output.filestr <- paste0("Forecasts/", county1)
    mean.ini <- 1e-5 * county.pop1
    inputs$internal.args$lambda_ini_exposed <- 1 / mean.ini


    inputs$internal.args$iter <- 300; inputs$internal.args$warmup <- NULL  #temp #1.4 hours, rhat = 1.05
    inputs$model.inputs$end.date <- as.Date("2021/8/1")
    nt <- as.numeric(inputs$model.inputs$end.date - inputs$internal.args$simulation.start.date)
    #uk now and growth is good for San Diego but uk now is too high for SF -- need estimates of uk now and sa now by county? assume no SA for now
    inputs$vaccines <- GetVaccineParams(vaccinated_per_day_max = 6000, vaccinated_per_day_increase = 135, uk_growth = 1.05, sa_growth = 1, uk_now = 0.01, sa_now = 0.0001, nt = nt)
    inputs$vaccines$vaccinated_per_day <- inputs$vaccines$vaccinated_per_day * county.pop1 / 883305
    stopifnot(exists("branchname", envir = asNamespace("LEMMA")))

    cred.int <- LEMMA:::CredibilityInterval(inputs)

    max.date <- max(cred.int$inputs$obs.data$date)
    outfile <- paste0(dir, "Scenarios/", county1)

    sink()
    ParallelLogger::logInfo("county = ", county1)

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
  county.results <- lapply(county.set, RunOneCounty, quick.test)
} else {
  num.clusters <- floor(parallel::detectCores() / 4) - 1
  cat("num.clusters = ", num.clusters, "\n")
  cl <- makeCluster(num.clusters)
  county.results <- clusterApply(cl, county.set, RunOneCounty, quick.test)
  stopCluster(cl)
}
names(county.results) <- county.set
cat("Data through", as.character(county.dt[, max(date)]), "\n")
ParallelLogger::logInfo("done")
unregisterLogger(1)

dt <- fread(logfile)
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



