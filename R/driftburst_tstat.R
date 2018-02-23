#' @title Drift-Burst t-statistic
#' Function computes drift-burst statistics from [Christensen, Oomen, Reno](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2842535)
#' @param lobster Data-frame from lobster
#' @param bndw_m bandwidth (mean)
#' @param bndw_v bandwidth(volatility)
#' @param grid Testing time (default = once per Minute (after discarding the first 5))
#' @importFrom magrittr '%>%'
#' @importFrom dplyr mutate_each
#' @return vector with t stat values
#' @export
driftburst_tstat <- function(lobster,
                             bndw_m = 300,
                             bndw_v =5*bndw_m,
                             grid = seq(from=lobster$Time[1]+bndw_v,
                                        to=lobster$Time[nrow(lobster)],
                                        by =60)) {

    dat <- lobster %>%
        dplyr::mutate(dxi = c(NA, diff(log(Midquote)))) %>%
        na.omit() %>%
        dplyr::filter(dxi != 0)

    dxi <- (dat%>%.$dxi)[-1]

    n_mu <- rep(NA,length(grid))
    n_sigma <- rep(NA,length(grid))
    mu_t <- rep(NA,length(grid))
    var_t <- rep(NA,length(grid))
    n_aclag <- rep(NA, length(grid))

    for(i in 1:length(grid)){
        x <- as.numeric(dat$Time[-1]-grid[i])
        w <- exponential.kernel(x/bndw_m)
        n_mu[i] <- sum(w)
        idx <- w>1e-5
        mu_t[i] <- sum(w[idx]*dxi[idx]) / bndw_m

        w <- exponential.kernel(x/bndw_v)
        n_sigma[i] <- sum(w)
        idx <- w > 1e-5
        wdxi <- w[idx]*dxi[idx]

        v0 <- sum(wdxi^2)
        c <- 2.6614
        max_lag <- 20
        q <- 2
        n <- round(4*(n_mu[i]/100)^(4/25))
        root <- 1/(2*q+1)

        ac <- rep(0, n)
        for (j in 1:n) ac[j] <- 2*sum(wdxi[(j+1):(length(wdxi))]*wdxi[1:(length(wdxi)-j)]);

        s0 <- v0 + sum(ac);
        sq <- sum((1:n)^q*ac)

        gamma <- c*(((sq/s0)^q)^root)
        n_aclag[i] <- max(1, min(round(gamma*n_mu[i]^root),max_lag))
        ac <- rep(0, n_aclag[i])
        for (j in 1:n_aclag[i]) ac[j] <- 2*sum(wdxi[(j+1):(length(wdxi))]*wdxi[1:(length(wdxi)-j)])

        var_t[i] <- (v0 + sum(ac*aux_hac_weight(n_aclag[i])))/ bndw_v
    }

    db_t <- sqrt(bndw_m) * mu_t/sqrt(var_t)
    return(list(db_t = db_t,
                grid = grid,
                mu_t = mu_t,
                var_t = var_t,
                n_mu = n_mu,
                n_sigma = n_sigma,
                n_aclag = n_aclag))
}
