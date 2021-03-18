setwd("~/Documents/GitHub/LEMMA-Forecasts/")

source('Code/GetCountyData.R')
source('Code/RunCountiesFromBeginning.R')

county.dt <- GetCountyData()
saveRDS(county.dt, "~/Documents/Temp/savedCountyData.rds") #save in case HHS server is down later
max.date <- Get1(county.dt[!is.na(hosp.conf), max(date), by = "county"]$V1)
cat("max date = ", as.character(max.date), "\n")
doses.dt <- GetDosesData()

county.by.pop <- unique(county.dt[!is.na(population), .(county, population)]) #NA population if no hospitalizations
setorder(county.by.pop, -population)
county.set <- county.by.pop[, county]

print(county.set)
print(system.time(
lemma.set <- parallel::mclapply(county.set, RunOneCounty, county.dt, doses.dt, mc.cores = 15)
))


