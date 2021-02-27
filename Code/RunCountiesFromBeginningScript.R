library(matrixStats)
library(data.table)
library(ParallelLogger)
# setwd("~/Documents/GitHub/LEMMA-Forecasts/")

source('Code/GetCountyData.R')

RunOneCounty1 <- function(county1, county.dt) {
  source('Code/RunCountiesFromBeginning.R')
  RunOneCounty(county1, county.dt)
}

if (!exists("county.dt")) {
  county.dt <- GetCountyData(include.regions = F)
}

options(warn = 1)
county.set <- c("Kings", "Lake", "Solano", "Sonoma", "Stanislaus", "Tulare", "Ventura")


options(warn = 1)
assign("last.warning", NULL, envir = baseenv())

logfile <- "Restart/Logs/logger.txt"
unlink(logfile)
clearLoggers()
addDefaultFileLogger(logfile)

num.clusters <- floor(parallel::detectCores() / 4) - 1
cat("num.clusters = ", num.clusters, "\n")
cl <- makeCluster(num.clusters)
clusterApply(cl, county.set, RunOneCounty1, county.dt)
stopCluster(cl)

logInfo("done")
unregisterLogger(1)
