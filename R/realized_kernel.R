#' Realized Kernel estimator (univariate)
#' @param hft_prices vector with (log) prices
#' @export
realized_kernel <- function(hft_prices) {
    N <- length(hft_prices)

    omega <- function(q, prices) {
        q_returns <- diff(prices[seq(from = 1, to = length(prices), by = q)])[-1]
        sum(q_returns^2)/(sum(q_returns!=0) - 1)
    }
    omegahat2 <- mean(sapply(1:25, omega, hft_prices))
    IVhat <- IVhat_f(hft_prices)
    noise <- omegahat2/IVhat
    Hval <- cstar * noise^(2/5) * length(hft_prices)^(3/5)
    Hvalstar <- min(15, Hval)
    bans <- 0
    hft_returns <- diff(hft_prices)[-1]
    for (i in -(ceiling(Hvalstar)):(ceiling(Hvalstar))) {
        kernw = parzen.kernel(abs(i)/(Hvalstar + 1))
        autocov = autocovariance(hft_returns, i)
        bans = bans + kernw * autocov
    }

    return(bans)
}
