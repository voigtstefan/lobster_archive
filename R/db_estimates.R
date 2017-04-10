#' Drift-Burst t-statistic
#' data(lobster)
#' lobster
#' @export
db_estimates <- function(lobster, testing.time = unique(ceiling(lobster$Secs/2) * 2)) {

    # L_m <- 5 # Parzen Kernel Bandwith L_v <- 25 interval <- 5 # seconds

    data <- lobster %>% transmute(Secs = Secs, lret = c(NA, diff(log(Midquote)))) %>% na.omit() %>% filter(lret != 0)

    bndw_m <- 300  # 300 seconds
    bndw_v <- 1500  # 25 Minutes (=25*60 seconds)

    mu_t <- rep(NA, length(testing.time))
    var_t <- rep(NA, length(testing.time))
    n_mu <- rep(NA, length(testing.time))
    n_sigma <- rep(NA, length(testing.time))
    n_lag <- rep(NA, length(testing.time))

    for(i in 1:length(testing.time)){
    dat_m <- data %>%
        filter(Secs < testing.time[i] & Secs > testing.time[i] - bndw_m) %>%
        transform(w_mu = parzen.kernel((Secs - testing.time[i])/bndw_m))
    n_mu <- sum(dat_m$w_mu)
    mu_t <- sum(dat_m$w_mu * dat_m$lret)/bndw_m

    dat_v <- data %>% filter(Secs < testing.time[i] & Secs > testing.time[i] - bndw_v) %>% transform(w_v = parzen.kernel((Secs -
        testing.time[i])/bndw_v))
    n_v <- sum(dat_v$w_v)
    wdxi <- dat_v$w_v * dat_v$lret
    v0 <- sum(wdxi^2)

    c <- 2.6614
    max_lag <- 20
    q <- 2
    T <- n_mu

    n <- round(4 * (T/100)^(4/25))
    root <- 1/(2 * q + 1)
    ac <- rep(1, n)

    for (j in 1:n) {
        ac[j] <- 2 * sum(wdxi[(j + 1):length(wdxi)] * wdxi[1:(length(wdxi) - j)])
    }

    s0 <- v0 + sum(ac)
    sq <- sum((1:n)^q * ac)

    gamma <- c * (((sq/s0)^q)^root)
    n_aclag <- min(round(gamma * T^root), max_lag)

    ac <- rep(0, n_aclag)

    for (j in 1:n_aclag) {
        ac[j] <- 2 * sum(wdxi[(j + 1):length(wdxi)] * wdxi[1:(length(wdxi) - j)])
    }

    aux_hac_weight <- parzen.kernel((1:n_aclag)/n_aclag)
    var_t[i] <- v0 + sum(ac * aux_hac_weight)
    n_lag[i] <- n_aclag
    }

    var_t <- var_t/bndw_v
    db_t <- sqrt(bndw_m) * mu_t/sqrt(var_t)
    return(list(db = db_t, mu = mu_t, var = var_t, n_mu = n_mu, n_sigma = n_sigma))
}
