# --------------------------------------------------------------------------------
#   Run a county
#   1. RunOneCounty
# --------------------------------------------------------------------------------


#' @title Run a county
#' @param county1 a character string giving the name of the county
#' @param county.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetCountyData}}
#' @param doses.dt a \code{\link[data.table]{data.table}} object returned from \code{\link[LEMMA.forecasts]{GetDosesData}}
#' @param remote a logical value, if \code{TRUE} download all data from remotes, otherwise use local data
#' @param writedir a character string giving a directory to write to. Ignored if \code{remote} is \code{FALSE}
#' @export
RunOneCounty <- function(county1, county.dt, doses.dt, remote = FALSE, writedir = NULL) {

  if (remote & is.null(writedir)) {
    stop("if 'remote' is TRUE, please provide a directory to write results to in 'writedir'")
  }

  inputs <- GetCountyInputs(county1 = county1, county.dt = county.dt, doses.dt = doses.dt, remote = remote, writedir = writedir)
  lemma <- LEMMA:::CredibilityInterval(inputs)
  return(lemma)
}


