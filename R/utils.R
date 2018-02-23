#' @title applyNS
#' Counts number of trades during last k seconds
#' @param s time-vector (seconds)
#' @param k (default = 1.5)
#' @return cnt vector of same length at s, indicating number of occurences during [t,t-k]
#' @export
applyNS <- function(s, k = 1.5) {
    cnt <- numeric(length(s))
    for (i in 1:length(s)) {
        res <- (s[(1 + i):length(s)] - s[1:(length(s) - i)]) <= k
        cnt[(1 + i):length(s)] <- cnt[(1 + i):length(s)] + res
        if (!any(res))
            break
    }
    return(cnt)
}

#' @title base_mapply
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

#' @title Parzen Kernel
#' Computes the Parzen-Kernel
#' @param x value (also vectors allowed) at which to evaluate the Parzen-Kernel
#' @return w Parzen-Kernel
#' @export
parzen.kernel <- function(x) {
    w <- rep(0, length(x))
    x <- abs(x)
    w[x > 1] <- 0
    w[x <= 0.5] <- 1 - 6 * x[x <= 0.5]^2 + 6 * x[x <= 0.5]^3
    w[(x <= 1) & (x > 0.5)] <- 2 * (1 - x[x <= 1 & (x > 0.5)])^3
    return(w)
}

#' @title Exponential kernel
#' @param x input value (vectors also allowed)
#' @return Exponential kernel evaluated at x
#' @export
exponential.kernel <- function(x) exp(-abs(x)) * (x <= 0)

#' @title Autocovariane
#'
#' computes the Autocovariance matrices
#' inputs are Refresh time sampled returns and optimal Hval
#' @param rftdata Refresh time sampled returns (xts object with N columns (assets) and T rows (observations))
#' @param hval bandwith (lag)
#' @return Autocovariance matrix at lat hval
#' @export
autocovariance <- function(rftdata, hval) {
    if (ncol(rftdata) > 1) {
        GammaH = t(rftdata[(abs(hval) + 1):nrow(rftdata), ]) %*% rftdata[1:(nrow(rftdata) -
            abs(hval)), ]
        if (hval < 0)
            return(t(GammaH))
        if (hval >= 0)
            return(GammaH)
    }
    if (ncol(rftdata) == 1) {
        rftdata = as.vector(rftdata)
        GammaH = sum(rftdata[(abs(hval) + 1):length(rftdata)] * rftdata[1:(length(rftdata) -
            abs(hval))])
        return(GammaH)
    }
}

#' IV estimate based on 20 minute RV estimates
#' @param rftprices Refresh time sampled (log) prices (xts object with N columns (assets) and T rows (observations))
#' @importFrom lubridate hour
#' @importFrom lubridate minute
#' @importFrom lubridate second
#' @export
IVhat_f <- function(rftprices) {
    seconds <- cbind(hour(time(rftprices)), minute(time(rftprices)), second(time(rftprices))) %*%
        c(3600, 60, 1)
    prevtime <- rep(NA, 1199)
    for (sec in 1:1199) {
        grid <- seq(from = 34200 + sec, by = 20 * 60, to = 57600)
        prevtime[sec] <- sum(diff(rftprices[findInterval(grid, seconds)])^2, na.rm = TRUE)
    }
    return(mean(prevtime, na.rm = TRUE))
}

#' @title Conditioning number
#' Returns the conditioning number of a matrix
#' @param mat N x N matrix
#' @return Ratio of highest over smallest eigenvalue
#' @export
cond <- function(mat) {
    val <- eigen(mat)$values
    return(max(val)/min(val))
}

#' @title Inverse wishart
#' Samples from an inverse wishart distribution with Scale matrix S and degrees of freedom v.
#' Equivalent to Wishart distribution with scale $S^{-1}$.
#' @param S scale matrix
#' @param v degrees of freedom
#' @export
riwish <- function(v, S) {
    return(solve(rwish(v, solve(S))))
}

#' @title Wishart
#' Samples from a Wishart distribution
#' @param S scale matrix
#' @param v degrees of freedom
#' @export
rwish <- function(v, S) {
    if (!is.matrix(S))
        S <- matrix(S)
    if (v < nrow(S))
        stop(message = "v is less than the dimension of S in rwish().\n")
    p <- nrow(S)
    CC <- chol(S)
    Z <- matrix(0, p, p)
    diag(Z) <- sqrt(rchisq(p, v:(v - p + 1)))
    if (p > 1) {
        pseq <- 1:(p - 1)
        Z[rep(p * pseq, pseq) + unlist(lapply(pseq, seq))] <- rnorm(p * (p - 1)/2)
    }
    return(crossprod(Z %*% CC))
}

#' @title HAC Weight kernel
#' @param m weight vector
aux_hac_weight <- function(m) {
    w <- (1:m)/m
    idx <- w <= 0.5
    w[idx] <- 1 - 6*w[idx]^2 + 6*w[idx]^3
    w[!idx] <- 2*(1 - w[!idx])^3
    return(w)
}
