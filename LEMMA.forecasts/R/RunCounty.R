# --------------------------------------------------------------------------------
#   Run a county
#   1. RunOneCounty
# --------------------------------------------------------------------------------


#' @title Run a county
#' @param county1 a character string giving the name of the county
#' @param county.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetCountyData}}
#' @param doses.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetDosesData}}
#' @param remote a logical value, if \code{TRUE} download all data from remotes, otherwise use local data
#' @export
RunOneCounty <- function(county1, county.dt, doses.dt, remote = FALSE) {
  inputs <- GetCountyInputs(county1, county.dt, doses.dt, remote)
  lemma <- LEMMA:::CredibilityInterval(inputs)
  return(lemma)
}


