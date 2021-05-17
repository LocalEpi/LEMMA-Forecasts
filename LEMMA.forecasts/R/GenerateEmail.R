# --------------------------------------------------------------------------------
#   Functions to auto generate email:
#   1. GenerateEmail
# --------------------------------------------------------------------------------


#' @title Generate email from results
#' @param max.date maximum date of data
#' @export
GenerateEmail <- function(max.date) {
  Format <- function(x) sprintf("%2.2f", x)
  GetStr <- function(prev, curr) {
    if (prev > curr) {
      paste("has decreased from", Format(prev), "to", Format(curr))
    } else if (prev < curr) {
      paste("has increased from", Format(prev), "to", Format(curr))
    } else {
      paste("remains", Format(curr))
    }
  }
  ShortDate <- function(d) as.character(as.Date(d), format = "%b%e")
  HospStr <- function(h) {
    if (h >= 5) {
      round(h, -1)
    } else {
      "less than 5"
    }
  }

  x <- as.data.table(readxl::read_excel("Forecasts/San Francisco.xlsx", sheet = "projection"))
  x[, date := as.Date(date)]
  curr.rt <- x[date == (max.date - 14), round(rt, 2)]

  filename <- "Inputs/SF_Rt.rds"
  rt.record <- readRDS(filename)
  prev <- rt.record[[as.character(max.date - 1)]]
  if (is.null(prev)) {
    warning(paste("rt not found for", as.character(max.date - 1)))
    prev.rt  <- -999
  } else {
    prev.rt <- prev[1]
  }
  rt.record[[as.character(max.date)]] <- curr.rt
  saveRDS(rt.record, filename)

  cat("Estimated Re (14 days ago) ", GetStr(prev.rt, curr.rt), " with data through ", ShortDate(max.date), ".", sep = "")


  today.plus30 <- max.date + 30
  cat("\n30 day forecast (", ShortDate(today.plus30), "): Hospitalization ", HospStr(x[date == today.plus30, hosp]), ".\n", sep = "")
}
