#' plot_lobster
#'
#' @import ggplot2
#' @importFrom scales date_breaks
#' @export
plot_lobster <- function(v, var, date = NA, title = "") {
    if (is.na(date))
        date = as.Date(v$Time[1])
    ggplot(v, aes(x = as.POSIXct(date, tz = Sys.timezone()) + Secs, y = eval(parse(text = var)))) + geom_line() + scale_x_datetime(expand = c(0,
        0), breaks = date_breaks("1 hour"), labels = date_format("%H:%M", tz = Sys.timezone())) + xlab("Time") + ylab(var) +
        ggtitle(title) + theme_bw()
}

#' applyNS
#'
#' Counts number of trades during last k seconds
#' @export
applyNS <- function(s, k = 1.5) {
    cnt <- numeric(length(s))
    for (i in 1:length(s)) {
        res <- (s[(1 + i):length(s)] - s[1:(length(s) - i)]) <= k
        cnt[(1 + i):length(s)] <- cnt[(1 + i):length(s)] + res
        if (!any(res))
            break
    }
    # cnt[cnt==0]<-1
    return(cnt)
}

#' base_mapply
#'
#' Base mapply function
#' Use this function to compute 'rolling-window'-like calculations with columnd depend window sizes.
#' e.g. if you want to compute the rolling mean over all trades which took place during the last 1.5 seconds
#' the command is base_mapply(lobster$Price,width=applyNS(lobster$Secs),FUN=mean)
#' @export

base_mapply <- function(x, width, FUN, ...) {
    FUN <- match.fun(FUN)
    f <- function(i, width, data) {
        if (i < width)
            return(NA_real_)
        return(FUN(data[(i - (width - 1)):i], ...))
    }
    mapply(FUN = f, seq_along(x), width, MoreArgs = list(data = x))
}

#' Computes the Parzen Kernel
#'
#' @export
parzen.kernel <- function(x) {
    w <- rep(0, length(x))
    x <- abs(x)
    w[x > 1] <- 0
    w[x <= 0.5] <- 1 - 6 * x[x <= 0.5]^2 + 6 * x[x <= 0.5]^3
    w[(x <= 1) & (x > 0.5)] <- 2 * (1 - x[x <= 1 & (x > 0.5)])^3
    return(w)
}

#' Realized Kernel
#'
#' @export
realized.kernel <- function(data, kernel = parzen.kernel, H) {
    x <- as.vector(data$lret)
    t <- as.vector(data$Secs)
    n <- length(x)


    cstar <- 3.5134
    omegahat2 <- mean((sapply(1:25, function(q, x) mean(x[seq(1, length(x), q)]^2), x))/2)
    prevtime <- rep(NA, 1199)
    for (sec in 1:1199) {
        grid <- seq(from = min(t) + sec, by = 20 * 60, to = max(t))
        prevtime[sec] <- sum(x[findInterval(grid, t)]^2, na.rm = TRUE)
    }
    IVhat <- mean(prevtime, na.rm = TRUE)
    noise <- omegahat2/IVhat
    Hval <- cstar * noise^(2/5) * length(x)^(3/5)
    H <- min(400, max(0, Hval, na.rm = FALSE))
    h <- (-H):H
    w <- kernel(h/(H + 1))
    sum(w * unlist(sapply(h, function(h, x) t(x[(abs(h) + 1):n]) %*% x[1:(n - abs(h))], x)))

}


#' Exponential kernel
#'
#' @export
exponential.kernel <- function(x) exp(-abs(x)) * (x <= 0)


#' Autocovariane
#'
#' computes the Autocovariance matrices
#' inputs are Refresh time sampled returns and optimal Hval
#' @export
autocovariance <- function(rftdata, hval) {
    if(ncol(rftdata)>1){
        GammaH = t(rftdata[(abs(hval) + 1):nrow(rftdata), ]) %*% rftdata[1:(nrow(rftdata) - abs(hval)), ]
        if (hval < 0)
            return(t(GammaH))
        if (hval >= 0)
            return(GammaH)
    }
    if(ncol(rftdata)==1){
        rftdata =as.vector(rftdata)
        GammaH = sum(rftdata[(abs(hval) + 1):length(rftdata)] * rftdata[1:(length(rftdata) - abs(hval))])
        return(GammaH)
    }
        }


#' IV estimate based on 20 minute RV estimates
#' @importFrom lubridate hour
#' @importFrom lubridate minute
#' @importFrom lubridate second
#' @export
IVhat_f <- function(rftdata) {
    seconds <- cbind(hour(time(rftdata)), minute(time(rftdata)), second(time(rftdata))) %*% c(3600, 60, 1)
    prevtime <- rep(NA, 1199)
    for (sec in 1:1199) {
        grid <- seq(from = 34200 + sec, by = 20 * 60, to = 57600)
        prevtime[sec] <- sum(rftdata[findInterval(grid, seconds)]^2, na.rm = TRUE)
    }
    return(mean(prevtime, na.rm = TRUE))
}

#' RK estimates
#' @export

RK.univariate <- function(hft_returns){
    N<-length(hft_returns)
    omegaest <- function(returns) max(sum(returns[-1] * returns[1:(length(returns) - 1)])/(length(returns) - 1), 0)
    omegahat2 <- unlist(lapply(hft_returns, omegaest))
    IVhat <- unlist(lapply(hft_returns, IVhat_f))
    noise <- omegahat2/IVhat
    cstar <- (12^2/0.269)^(1/5)
    Hval <- cstar * noise^(2/5) * unlist(lapply(hft_returns, length))^(3/5)

    rk <- function(x){
    bans <- 0
    for (i in -min(20,ceiling(Hval[x])):min(20,ceiling(Hval[x]))) {
        kernw <- parzen.kernel(abs(i)/(Hval[x] + 1))
        autocov <- autocovariance(hft_returns[[x]], i)
        bans <- bans + kernw * autocov
    }
    return(bans)}

    t <- lapply(1:N,rk)
    return(unlist(t))

}

#' cond
#' @export
cond <- function(mat){
    val <- eigen(mat)$values
    return(abs(max(val)/min(val)))
}

#' inverse wishart
#' @export
riwish <- function (v, S) {
    return(solve(rwish(v, solve(S))))}

#' wishart
#' @export
rwish <- function (v, S) {
    if (!is.matrix(S)) S <- matrix(S)
    if (v < nrow(S))   stop(message = "v is less than the dimension of S in rwish().\n")
    p  <- nrow(S)
    CC <- chol(S)
    Z  <- matrix(0, p, p)
    diag(Z) <- sqrt(rchisq(p, v:(v - p + 1)))
    if (p > 1) {
        pseq <- 1:(p - 1)
        Z[rep(p * pseq, pseq) + unlist(lapply(pseq, seq))] <- rnorm(p *
                                                                        (p - 1)/2)
    }
    return(crossprod(Z %*% CC))
}

