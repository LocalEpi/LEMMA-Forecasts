#update commit to only commit SC files (avoids problems with logger.txt and CountyData.rds)

library(data.table)

source('Code/GetCountyData.R')

#git.pw <- readline("Enter github password: ")
git.pw <- "ND"
git.pw <- paste0("Q2zDSR4BEaV6GnHgYh", git.pw)

library(data.table)
Get1 <- function(zz) {
  stopifnot(uniqueN(zz) == 1)
  zz[1]
}

county.dt1 <- GetSantaClaraData()
county.pop1 <- county.dt1[, Get1(population)]

cat("county = ", county1, "\n")
cat("start time = ", as.character(Sys.time() - 3600 * 8), "\n")
cat("max date = ", as.character(max(county.dt1$date)), "\n")

input.file <- "Inputs/CAcounties.xlsx"
sheets <- LEMMA:::ReadInputs(input.file)
sheets$`Model Inputs`[internal.name == "total.population", value := county.pop1]

county.dt1[, deaths.pui := NA_integer_]
county.dt1[, cum.admits.conf := NA_integer_]
county.dt1[, cum.admits.pui := NA_integer_]

county.dt1[date < as.Date("2020/8/1"), icu.conf := NA_integer_]
county.dt1[date < as.Date("2020/8/1"), icu.pui := NA_integer_]
county.dt1[date < as.Date("2020/8/1"), deaths.conf := NA_integer_]
county.dt1[date < as.Date("2020/8/1"), deaths.pui := NA_integer_]

sheets$Data <- county.dt1
inputs <- LEMMA:::ProcessSheets(sheets, input.file)

inputs$internal.args$warmup <- NA #defaults to iter/2


inputs$internal.args$output.filestr <- paste0("Forecasts/", county1)
mean.ini <- 1e-5 * county.pop1
inputs$internal.args$lambda_ini_exposed <- 1 / mean.ini

cred.int <- LEMMA:::CredibilityInterval(inputs)

max.date <- max(cred.int$inputs$obs.data$date)
outfile <- paste0(dir, "Scenarios/", county1)

ProjScen <- function(int.list) {
  int.date <- int.list$date
  int.str <- int.list$str
  intervention <- data.frame(mu_t_inter = int.date, sigma_t_inter = 0.0001, mu_beta_inter = 0.5, sigma_beta_inter = 0.0001, mu_len_inter = 7, sigma_len_inter = 2)
  subtitl <- paste("Scenario: Reduce Re by 50% starting", as.character(as.Date(int.date), format = "%b%e"))
  lapply(LEMMA:::ProjectScenario(cred.int, new.int=intervention, paste0("Scenarios/", county1, "_scenario_", int.str))$gplot$long.term, function (z) z + ggplot2::labs(subtitle = subtitl))
}

scen.plots <- lapply(list(list(date = max.date + 3, str = "actToday"), list(date = max.date + 17, str = "actTwoWeeks")), ProjScen)


commit.name <- paste0('"', county1, " data through ", as.character(max.date), '"')

files <- list.files(pattern = "Santa Clara", recursive = T)
for (f in files) {
  system2("git", args = c("add", paste0('"', f, '"')))
}
system2("git", args = c('commit', '-m', commit.name))
system2("git", args = "pull")
git.dest <- paste0("https://joshuaschwab:", git.pw, "@github.com/LocalEpi/LEMMA-Forecasts")
system2("git", args = c("push", git.dest))



