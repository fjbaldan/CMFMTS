#' Estimation of Shannon Entropy
#'
#' Using http://cran.r-project.org/web/packages/entropy/entropy.pdf
#'
#' @import entropy
#'
#' @param time_serie A time serie, object of type ts.
#' @param method how to calculate shannon entropy: "SG", "CS", "ML"
#' @importFrom entropy entropy
#' @importFrom entropy discretize
#' @return The value of the complexity measure: Estimation of Entropy.
measure.shannonEntropy <- function(time_serie, method) {
  result = tryCatch({
    if(method %in% c("Jeffreys", "Laplace", "SG", "minimax","MM","shrink","CS","ML")){
      counts=entropy::discretize(time_serie, numBins=10)
      # counts=time_serie
      out=entropy::entropy(counts, method=method,verbose=F)
      out
    }else{
      warning ("Incorrect Method.")
      out=NA
    }
    out
  }, error = function(e) {
    out=NA
    out
  })
  names(result)=paste0("shannon_entropy_",method)
  return (result)
}
