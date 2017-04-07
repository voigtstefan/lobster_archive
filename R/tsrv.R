#' Two-scaled realized volatility estimator
#'@export
tsrv <- function(logprices, K = 300, J = 1) {
    # pdata contains prices for a stock K the slow time scale = 300 seconds J is the fast time scale = 1 second
    n = length(logprices)
    nbarK = (n - K + 1)/(K)
    nbarJ = (n - J + 1)/(J)
    adj = (1 - (nbarK/nbarJ))^-1  #adjust for finite sample bias
    logreturns_K = logreturns_J = c()
    for (k in 1:K) {
        sel = seq(k, n, K)
        logreturns_K = c(logreturns_K, diff(logprices[sel])[-1])
    }
    for (j in 1:J) {
        sel = seq(j, n, J)
        logreturns_J = c(logreturns_J, diff(logprices[sel])[-1])
    }
    TSRV = adj * ((1/K) * sum(logreturns_K^2) - ((nbarK/nbarJ) * (1/J) * sum(logreturns_J^2)))
    return(TSRV)
}
