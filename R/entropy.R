#' Permutation Entropy
#'
#' Using package tsExpKit (https://github.com/cbergmeir/tsExpKit/blob/master/R/dataComplexity.R)
#'
#' @param time_serie A time serie, object of type ts.
#' @param order the order of the permutation entropy
#' @return The value of the complexity measure: Permutation Entropy.
#' @importFrom combinat permn
measure.permutation_entropy <- function(time_serie, order=4) {
  result = tryCatch({
    y <- time_serie
    ly <- length(time_serie)

    permlist <- combinat::permn(1:order)
    vec <- seq(0,0,length=length(permlist))

    for (j in 1:(ly-order)) {

      sort.res <- sort.int(y[j:(j+order-1)], index.return = TRUE);
      iv <- sort.res$ix

      for (jj in 1:length(permlist)) {
        if (sum(abs(permlist[[jj]]-iv))==0) {
          vec[jj] <- vec[jj] + 1 ;
        }
      }
    }

    p <- apply(as.matrix(vec/(ly-order)), 1, function (x) {max(1/ly, x)})
    out <- -sum(p * log(p))/(order-1)
    out
    }, error = function(e) {
      out=NA
      out
    })
    names(result)="permutation_entropy"
    return(result)
}


#' Aproximation Entropy
#'
#' Using package pracma (http://www.inside-r.org/packages/cran/pracma/docs/approx_entropy)
#'
#' @param time_serie A time serie, object of type ts.
#' @param edim the embedding dimension, as for chaotic time series; a preferred value is 2
#' @param r filter factor; work on heart rate variability has suggested setting r to be 0.2 times the standard deviation of the data
#' @importFrom pracma approx_entropy
#' @return The value of the complexity measure: aproximation entropy.
measure.aproximation_entropy <- function(time_serie, edim = 2, r = 0.2*sd(time_serie)) {
  result = tryCatch({
    out=pracma::approx_entropy(time_serie, edim = edim, r = r)
    out
  }, error = function(e) {
    out=NA
    out
  })
  names(result)="aproximation_entropy"
  return(result)
}

#' Sample Entropy
#'
#' Based on: http://www.physionet.org/physiotools/sampen/matlab/1.1/sampenc.m
#'
#' @param y input data
#' @param M maximum template length
#' @param r matching tolerance
#' @param package version of aproximation entropy (pracma or sampenc)
#' @importFrom pracma sample_entropy
#' @return sample entropy estimates for m<-0,1,...,M-1
measure.sample_entropy <-function(y, M=2, r=0.2*sd(y), package="") {
  if (package == "pracma") {
    result = tryCatch({
      out=pracma::sample_entropy(y, edim = M, r = r)
      out
    }, error = function(e) {
      out=NA
      out
    })
    names(result)="sample_entropy_pracma"
    return(result)
  }
  else {
    result = tryCatch({
      n <- length(y)
      lastrun <- matrix(0,1,n)
      run <- matrix(0,1,n)
      A <- matrix(0,M,1)
      B <- matrix(0,M,1)
      p <- matrix(0,M,1)
      e <- matrix(0,M,1)
      for (i in 1:(n-1)) {
        nj <- n - i
        y1 <- y[i]
        for (jj in 1:nj) {
          j <- jj + i
          if (abs(y[j] - y1) < r) {
            run[jj] <- lastrun[jj] + 1
            M1 <- min(M, run[jj])
            for (m in 1:M1) {
              A[m] <- A[m] + 1
              if (j < n)
                B[m] <- B[m] + 1
            }
          }
          else
            run[jj] <- 0
        }
        for (j in 1:nj) {
          lastrun[j] <- run[j]
        }
      }
      N <- n * (n-1)/2
      p <- c(A[1]/N)
      for(m in 2:M) {
        p1 <- A[m] / B[m-1]
        if(!is.nan(p1) && p1 != 0)
          p <- c(p, p1)
      }
      out=sum(-p*log(p))
      out
    }, error = function(e) {
      out=NA
      out
    })
    names(result)="sample_entropy"
    return(result)
  }
}

#' Spectral Entropy
#'
#' Calculation of the value of the spectral entropy.
#' Based on: http://uic.edu.hk/~kentsang/fyp2014/Fast%20Fourier%20Transform11.htm
#'
#' @param y input time series data
#' @return spectral entropy
measure.spectral_entropy <- function(y) {
  result = tryCatch({
    sumyf=0.0
    n=length(y)
    yf=fft(y)
    sumyf=sum(sapply(yf,function(x){
      abs(x)
    }))

    yf=yf/sumyf
    entropy=sum(sapply(yf,function(x){
      abs(x)*log(1/abs(x))
    }))
    entropy
  }, error = function(e) {
    out=NA
    out
  })
  names(result)="spectral_entropy"
  return(result)
}
