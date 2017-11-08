#' @title Two-scaled realized volatility estimator
#' @param logprices vector with log prices
#' @param K (Slow) Time Scale (in Seconds), default is 300 Seconds (=5 Minutes)
#' @param J (Fast) Time Scale (in Seconds), default is 1 Second
#' @return Realized volatility estimator
#'@export
two_scale_realized_volatility <- function(logprices, K = 300, J = 1) {

    n <- length(logprices)
    nbarK <- (n - K + 1)/(K)
    nbarJ <- (n - J + 1)/(J)
    adj <- (1 - (nbarK/nbarJ))^-1  #adjust for finite sample bias
    logreturns_K  <- logreturns_J  <- c()
    for (k in 1:K) {
        sel <- seq(from = k, to = n, by = K)
        logreturns_K <- c(logreturns_K, diff(logprices[sel])[-1])
    }
    for (j in 1:J) {
        sel <- seq(j, n, J)
        logreturns_J <- c(logreturns_J, diff(logprices[sel])[-1])
    }
    TSRV <- adj * ((1/K) * sum(logreturns_K^2) - ((nbarK/nbarJ) * (1/J) * sum(logreturns_J^2)))
    return(TSRV)
}
