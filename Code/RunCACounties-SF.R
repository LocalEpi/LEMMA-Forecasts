setwd("~/Documents/GitHub/LEMMA-Forecasts/")
library(data.table)
source('Code/GetCountyData.R')

SFDosesToAWS()
source('Code/RunCountiesFromBeginningScript.R') #assigns max.date and lemma.set
source('Code/CreateStateOverview.R')
source('Code/GenerateEmail.R')

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

