setwd("~/Documents/GitHub/LEMMA-Forecasts/")

source('Code/GetCountyData.R')
source('Code/RunCountiesFromBeginning.R')
source('Code/Scenarios.R')

county.dt <- GetCountyData()
max.date <- Get1(county.dt[!is.na(hosp.conf), max(date), by = "county"]$V1)
cat("max date = ", as.character(max.date), "\n")

saveRDS(county.dt, "Inputs/savedCountyData.rds") #save in case HHS server is down later
doses.dt <- GetDosesData()

county.by.pop <- unique(county.dt[!is.na(population), .(county, population)]) #NA population if no hospitalizations
setorder(county.by.pop, -population)
county.set <- county.by.pop[, county]

county.set <- setdiff(county.set, "Colusa"); cat("excluding Colusa\n")
print(county.set)

print(system.time(
lemma.set <- parallel::mclapply(county.set, RunOneCounty, county.dt, doses.dt, mc.cores = parallel::detectCores() - 1)
))


# debugonce(RunOneCounty)
# scen_dir <- tempdir()
# RunOneCounty(county1 = county.set[11],county.dt = county.dt,doses.dt = doses.dt,remote = TRUE,writedir = scen_dir)
# unlink(x = paste0(scen_dir,"/Forecasts"),recursive = T)
# unlink(x = paste0(scen_dir,"/Scenarios"),recursive = T)
# list.files(scen_dir)
# list.files(paste0(scen_dir,"/Forecasts"))
# list.files(paste0(scen_dir,"/Scenarios"))
