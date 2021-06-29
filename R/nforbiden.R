#' forbiden patterns
#'
#' This function calculates Number of "forbiden patterns" (cf. Amigo 2010)
#' Using package statcomp (https://www.rdocumentation.org/packages/statcomp/versions/0.0.1.1000/topics/global_complexity)
#'
#' @param time_serie input time series data
#' @param ndemb (OPTIONAL) If x is given, the embedding dimension (ndemb) is required.
#' @importFrom statcomp global_complexity
#' @return Number of "forbiden patterns" (cf. Amigo 2010)
measure.PE_nforbiden <- function(time_serie, ndemb) {
  result = tryCatch({
    out=statcomp::global_complexity(x = time_serie, ndemb = ndemb)
    out=out[c(3)]
    names(out)=c("nforbiden")
    out
  }, error = function(e) {
    out=c(NA)
    names(out)=c("nforbiden")
    out
  })
  return (result)
}
