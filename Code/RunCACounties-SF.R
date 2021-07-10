setwd("~/Documents/GitHub/LEMMA-Forecasts/")
library(data.table)
devtools::load_all("LEMMA.forecasts")

SFDosesToAWS()

if (T) {
  county.dt <- GetCountyData()
  saveRDS(county.dt, "Inputs/savedCountyData.rds") #save in case HHS server is down later

  doses.dt <- GetDosesData()
  saveRDS(doses.dt, "Inputs/savedDoses.rds") #save in case HHS server is down later
  ReadSFDoses(print_outdate = T) #only for printing

  deaths.cases <- data.table::fread("https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv")
  deaths.cases[, date := as.Date(date)]
  deaths.cases[, county := area]
  deaths.cases <- deaths.cases[!(county %in% c("Unknown", "Out of state", "California")) & !is.na(date)]
  data.table::setkey(deaths.cases, county, date)
  print(tail(deaths.cases[county == "San Francisco", .(date, cases, cases7=frollmean(cases, 7), total_tests, total_tests_7 = frollmean(total_tests, 7))], 20))
} else {
  cat("using saved county data\n")
  county.dt <- readRDS("Inputs/savedCountyData.rds")
  doses.dt <- readRDS("Inputs/savedDoses.rds")
}
max.date <- Get1(county.dt[!is.na(hosp.conf), max(date), by = "county"]$V1)
cat("max date = ", as.character(max.date), "\n")

county.set <- county.dt[, unique(county)]

print(ggplot(county.dt[date >= as.Date("2021/6/10") & !is.na(cases.conf), .(cases = sum(cases.conf)), by = "date"], aes(x = date, y = cases)) + geom_point() + scale_y_log10() + ggtitle("State Cases") + ylab("7 day average cases (log scale)"))

print(ggplot(county.dt[date >= as.Date("2021/6/10") & !is.na(hosp.conf), .(hosp = sum(hosp.conf)), by = "date"], aes(x = date, y = hosp)) + geom_point() + scale_y_log10() + ggtitle("State Hosp") + ylab("sum hosp (log scale)"))

print(ggplot(county.dt[date >= as.Date("2021/6/10") & county == "San Francisco" & !is.na(cases.conf), ], aes(x = date, y = cases.conf)) + geom_point() + scale_y_log10() + ggtitle("SF Cases") + ylab("7 day average cases (log scale)"))

print(tail(doses.dt[county == "San Francisco", .(date, doses = dose1 + dose2 + doseJ, doses_7 = frollmean(dose1 + dose2 + doseJ, 7), dose1, dose1_7 = frollmean(dose1, 7))], 20))

print(tail(county.dt[county == "San Francisco"]))

county.pop <- rowSums(readRDS("Inputs/county population by age.rds"))
county.set <- names(sort(county.pop[county.set], decreasing = T))
if ("Lassen" %in% county.set) {
  cat("exlcuding Lassen\n")
  print(tail(county.dt[county == "Lassen"], 10))
  county.set <- setdiff(county.set, "Lassen")
}

print(county.set)

print(system.time(
  lemma.set <- parallel::mclapply(county.set, RunOneCounty_scen, county.dt, doses.dt, mc.cores = parallel::detectCores() - 1)
))

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

AggregateState(county.set, county.pop, max.date, path = "Forecasts", "")
AggregateState(county.set, county.pop, max.date, path = "Scenarios", "_epiOptimistic")
AggregateState(county.set, county.pop, max.date, path = "Scenarios", "_epiPessimistic")


system2("git", args = "pull")
system2("git", args = c("add", "-A"))
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


