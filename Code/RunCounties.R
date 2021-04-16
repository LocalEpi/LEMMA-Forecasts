source('Code/GetCountyData.R')
source('Code/RunCountiesFromBeginning.R')

county.dt <- GetCountyData(dload = TRUE)
max.date <- Get1(county.dt[!is.na(hosp.conf), max(date), by = "county"]$V1)
cat("max date = ", as.character(max.date), "\n")

# saveRDS(county.dt, "Inputs/savedCountyData.rds") #save in case HHS server is down later
doses.dt <- GetDosesData(dload = TRUE)

county.by.pop <- unique(county.dt[!is.na(population), .(county, population)]) #NA population if no hospitalizations
setorder(county.by.pop, -population)
county.set <- county.by.pop[, county]

county.set <- setdiff(county.set, "Glenn")
cat("excluding Glenn\n")
print(county.set)

# print(system.time(
#   lemma.set <- parallel::mclapply(county.set, RunOneCounty, county.dt, doses.dt, mc.cores = parallel::detectCores() - 1)
# ))

# test first county
system.time(
  RunOneCounty(county1 = county.set[1],county.dt = county.dt,doses.dt = doses.dt,dload = TRUE)
)
