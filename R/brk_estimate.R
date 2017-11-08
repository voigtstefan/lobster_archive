#' @title Compute Blocked-Realized Kernels
#' @param tickerlist Vector of length N with the names of the tickers
#' @param date Date for which the BRK should be computed (String)
#' @param nob Number of Groups
#' @param folder Relative path to the storage of the lobster files (extracted)
#' @param output_file Relative path to store the estimated matrices (naming convention: BRK_date_nob.rds)
#' ticker <- as.character(read.table('AddInformation/assets.csv')$x)
#' sim_days <- as.Date(read.table('AddInformation/trading_days.csv')$x)
#' @importFrom xts as.xts
#' @importFrom xts to.period
#' @importFrom highfrequency refreshTime
#' @export

brk_estimate <- function(tickerlist,
                         date,
                         nob = 4,
                         folder = Sys.getenv("GLOBAL"),
                         output_file = paste0("BRK_",date, "_", nob, ".rds")) {

    MidQuotedata <- vector("list", length(tickerlist))
    N <- length(tickerlist)
    names(MidQuotedata) <- tickerlist
    N <- length(tickerlist)
    sdate <- as.POSIXct(date, tz = "UTC")
    attr(sdate, "tzone") <- "UTC"

    for (i in 1:N) {
        ticker <- tickerlist[i]
        cat("# Read-in ", ticker, "(", i, ") ")
        path <- paste(folder, ticker, sep = "/")
        tmp <- readin_lobster(ticker, date, folder = path)
        tmp <- tmp %>% filter(Type == 4 | Type == 5) %>% select(Secs, Midquote)
        tmp <- na.omit(tmp)
        tmp <- as.xts(tmp$Midquote, order.by = sdate + tmp$Secs)
        MidQuotedata[[i]] <- log(tmp)
        cat(nrow(MidQuotedata[[i]]), "\n")
    }

    cstar <- (12^2/0.269)^(1/5)

    MidQuotedata <- lapply(MidQuotedata, na.omit)
    MidQuotedata <- lapply(MidQuotedata, function(x) x[!is.infinite(x)])

    nobs <- unlist(as.numeric(as.character(lapply(MidQuotedata, nrow))))
    not_existent <- tickerlist[is.na(nobs)]
    MidQuotedata[is.na(nobs)] <- NULL
    nobs <- na.omit(nobs)
    nobs_index <- order(nobs, decreasing = TRUE)

    groups <- split(names(MidQuotedata)[nobs_index], ceiling(1:N/(N/nob)))
    blockspace <- matrix(NA, ncol = N, nrow = N)
    myblocks <- unlist(lapply(groups, length))

    data_sorted <- MidQuotedata[names(MidQuotedata)[nobs_index]]

    groupval <- function(z) {
        GLrange <- 1:sum(myblocks[1:(nob - z + 1)])
        print(z)
        tmpspace <- matrix(NA, ncol = N, nrow = N)
        for (w in 1:z) {
            if (w == 1)
                MYrange <- GLrange
            if (w != 1)
                MYrange <- GLrange + sum(myblocks[1:(w - 1)])
            rt_prices <- refreshTime(data_sorted[MYrange])
            indexTZ(rt_prices) <- ""
            rt_returns <- diff(rt_prices)[-1, ]
            T <- nrow(rt_returns)
            omega <- function(q, prices) {
                q_returns <- diff(prices[seq(1, length(prices), q)])[-1]
                sum(q_returns^2)/(length(q_returns) - 1)
            }
            omegahat2 <- unlist(lapply(rt_prices, function(prices) mean(sapply(1:25, omega, prices))))

            # omegaest <- function(returns) max(sum(returns[-1] * returns[1:(nrow(returns) - 1)])/(nrow(returns) - 1), 0)
            # omegahat2 <- unlist(lapply(rt_returns, omegaest))
            IVhat <- unlist(lapply(rt_prices, IVhat_f))
            noise <- omegahat2/IVhat
            Hval <- cstar * noise^(2/5) * unlist(lapply(rt_returns, nrow))^(3/5)
            Hvalstar <- min(400, max(0, mean(Hval, na.rm = FALSE)))
            bans <- matrix(0, nrow = ncol(rt_returns), ncol = ncol(rt_returns))
            for (i in -(ceiling(Hvalstar)):(ceiling(Hvalstar))) {
                kernw = parzen.kernel(abs(i)/(Hvalstar + 1))
                autocov = autocovariance(rt_returns, i)
                bans = bans + kernw * autocov
                bans[lower.tri(bans)] = t(bans)[lower.tri(bans)]
            }
            V <- diag(bans)^(-0.5)
            R <- diag(V, nrow = length(MYrange)) %*% bans %*% diag(V, nrow = length(MYrange))
            R[lower.tri(R)] = t(R)[lower.tri(R)]
            tmpspace[MYrange, MYrange] <- R
        }
        return(tmpspace)
    }

    a <- lapply(1:nob, groupval)
    for (z in 1:nob) {
        GLrange = 1:sum(myblocks[1:(nob - z + 1)])
        for (w in 1:z) {
            if (w == 1)
                MYrange <- GLrange
            if (w != 1)
                MYrange <- GLrange + sum(myblocks[1:(w - 1)])
            blockspace[MYrange, MYrange] <- a[[z]][MYrange, MYrange]
        }
    }

    hft_returns <- lapply(data_sorted, function(x) diff(x)[-1])
    hft_returns <- lapply(hft_returns, function(x) x[abs(x) < 0.8])
    RK <- RK.univariate(hft_returns)
    RK[RK <= 0] <- 1e-07
    RK[is.na(RK)] <- 1e-07
    BRK <- diag(RK^0.5) %*% blockspace %*% diag(RK^0.5)
    BRK[lower.tri(BRK)] = t(BRK)[lower.tri(BRK)]
    rownames(BRK) <- names(data_sorted)
    colnames(BRK) <- names(data_sorted)
    BRK <- BRK[names(MidQuotedata), names(MidQuotedata)]
    saveRDS(BRK, output_file)
    output.T <- nrow(refreshTime(data_sorted))
    return(list(BRK = BRK, T.num = output.T))
}
