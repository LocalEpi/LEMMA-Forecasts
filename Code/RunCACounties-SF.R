setwd("~/Documents/GitHub/LEMMA-Forecasts/")
library(data.table)
devtools::load_all("LEMMA.forecasts")

SFDosesToAWS()


county.dt <- GetCountyData()
max.date <- Get1(county.dt[!is.na(hosp.conf), max(date), by = "county"]$V1)
cat("max date = ", as.character(max.date), "\n")

saveRDS(county.dt, "Inputs/savedCountyData.rds") #save in case HHS server is down later
doses.dt <- GetDosesData()
ReadSFDoses(print_outdate = T) #only for printing

county.by.pop <- unique(county.dt[!is.na(population), .(county, population)]) #NA population if no hospitalizations
setorder(county.by.pop, -population)
county.set <- county.by.pop[, county]

print(county.set)

print(system.time(
  lemma.set <- parallel::mclapply(county.set, RunOneCounty_scen, county.dt, doses.dt, mc.cores = parallel::detectCores() - 1)
))



print(tail(doses.dt[county == "San Francisco", .(date, doses = dose1 + dose2 + doseJ, doses7 = frollmean(dose1 + dose2 + doseJ, 7))], 20))
county.rt <- CreateOverview(lemma.set)
stopifnot(county.rt[, Rt] > 0.5 & county.rt[, Rt] < 2)
setorder(county.rt, -Rt)
print(county.rt, digits = 2)

if (T) {
  outfile <- paste0("~/Dropbox/LEMMA_shared/JS code branch/lemma input and output/SF-updating/SF-", max.date)
  file.copy("~/Documents/GitHub/LEMMA-Forecasts/Forecasts/San Francisco.pdf", paste0(outfile, ".pdf"), overwrite = T)
  file.copy("~/Documents/GitHub/LEMMA-Forecasts/Forecasts/San Francisco.xlsx", paste0(outfile, ".xlsx"), overwrite = T)
  file.copy("~/Documents/GitHub/LEMMA-Forecasts/Forecasts/StateOverview.pdf", "~/Dropbox/LEMMA_shared/JS code branch/lemma input and output/SF-updating/StateOverview.pdf", overwrite = T)


  for (i in list.files("~/Documents/GitHub/LEMMA-Forecasts/Scenarios/", pattern = "San Francisco")) {
    file.copy(paste0("~/Documents/GitHub/LEMMA-Forecasts/Scenarios/", i), paste0("~/Dropbox/LEMMA_shared/JS code branch/lemma input and output/SF-updating/", i), overwrite = T)
  }
}

system2("git", args = "pull")
commit.name <- paste0('"', "data through ", as.character(max.date), '"')
system2("git", args = c('commit', '-a', '-m', commit.name))
system2("git", args = "push")

GenerateEmail(max.date)
cat("\n")
RtMap(max.date, county.rt)
cat("\n")
system2("cat", '"Scenarios/San Francisco_ScenarioSummary.txt"')

file.copy(paste0("~/Documents/GitHub/LEMMA-Forecasts/Map/Rt_map_", as.character(max.date - 14), ".csv"), paste0("~/Dropbox/LEMMA_shared/JS code branch/lemma input and output/SF-updating/Rt_map_", as.character(max.date - 14), ".csv"), overwrite = T)
file.copy(paste0("~/Documents/GitHub/LEMMA-Forecasts/Map/Rt_map_", as.character(max.date - 14), ".pdf"), paste0("~/Dropbox/LEMMA_shared/JS code branch/lemma input and output/SF-updating/Rt_map_", as.character(max.date - 14), ".pdf"), overwrite = T)
