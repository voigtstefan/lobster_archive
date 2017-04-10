#' BRK Estimate
#' ticker <- as.character(read.table('AddInformation/assets.csv')$x)
#' sim_days <- as.Date(read.table('AddInformation/trading_days.csv')$x)
#' @importFrom xts as.xts 
#' @importFrom xts to.period 
#' @importFrom highfrequency refreshTime
#' @export

brk_estimate <- function(tickerlist, date, nob = 6, folder = ".") {
    
    N <- length(tickerlist)
    midquotedata <- vector("list", N)
    names(midquotedata) <- tickerlist
    
    dir.create(as.character(date), showWarnings = FALSE)
    setwd(as.character(date))
    
    for (i in 1:N) {
        ticker <- tickerlist[i]
        tryCatch({
            invisible(extract_lobster(ticker, date, folder = paste0("../", folder)))
            tmp <- readin_lobster(ticker, date)
            tmp <- as.xts(tmp$Midquote, order.by = tmp$Time)
            midquotedata[[i]] <- log(tmp)
            remove_lobster(ticker, date)
        }, error = function(e) {
            cat("Not existent: ", ticker, "\n")
        })
    }
    setwd("..")
    unlink(date, recursive = TRUE)
    
    cstar <- (12^2/0.269)^(1/5)
    
    nobs <- unlist(as.numeric(as.character(lapply(midquotedata, nrow))))
    not_existent <- tickerlist[is.na(nobs)]
    cat("Missing tickers: ", not_existent, "\n")
    midquotedata[is.na(nobs)] <- NULL
    nobs <- na.omit(nobs)
    nobs_index <- sort(nobs, index = TRUE, decreasing = TRUE)$ix
    Nadj <- length(midquotedata)
    groups <- split(names(midquotedata)[nobs_index], ceiling(1:Nadj/(Nadj/nob)))
    blockspace <- matrix(NA, ncol = Nadj, nrow = Nadj)
    myblocks <- unlist(lapply(groups, length))
    data_sorted <- midquotedata[names(midquotedata)[nobs_index]]
    data_sorted <- lapply(data_sorted,na.omit)
    groupval <- function(z) {
        GLrange <- 1:sum(myblocks[1:(nob - z + 1)])
        print(z)
        tmpspace <- matrix(NA, ncol = Nadj, nrow = Nadj)
        for (w in 1:z) {
            if (w == 1) 
                MYrange <- GLrange
            if (w != 1) 
                MYrange <- GLrange + sum(myblocks[1:(w - 1)])
            rt_prices <- refreshTime(data_sorted[MYrange])
            indexTZ(rt_prices) <- ""
            rt_returns <- diff(rt_prices)[-1, ]
            T <- nrow(rt_returns)
            omegaest <- function(returns) max(sum(returns[-1] * returns[1:(nrow(returns) - 1)])/(nrow(returns) - 1), 0)
            omegahat2 <- unlist(lapply(rt_returns, omegaest))
            IVhat <- unlist(lapply(rt_returns, IVhat_f))
            noise <- omegahat2/IVhat
            Hval <- cstar * noise^(2/5) * unlist(lapply(rt_returns, nrow))^(3/5)
            Hvalstar <- min(400, max(0, mean(Hval, na.rm = FALSE)))
            bans = matrix(0, nrow = ncol(rt_returns), ncol = ncol(rt_returns))
            for (i in -(floor(Hvalstar) + 1):(floor(Hvalstar) + 1)) {
                kernw = parzen.kernel(abs(i)/(Hvalstar + 1))
                autocov = autocovariance(rt_returns, i)
                bans = bans + kernw * autocov
            }
            bans <- 0.5 * (bans + t(bans))
            V <- diag(bans)^(-0.5)
            R <- diag(V, nrow = length(MYrange)) %*% bans %*% diag(V, nrow = length(MYrange))
            R <- (R + t(R))/2
            tmpspace[MYrange, MYrange] <- R
        }
        return(tmpspace)
    }
    
    # if(Sys.info()['sysname']!='Windows'){ no_cores <- detectCores() cl<-makeCluster(max(nob,no_cores),type='FORK')
    # a<-parLapply(cl, 1:nob, groupval) stopCluster(cl) }
    
    # if(Sys.info()['sysname']=='Windows') a<-lapply(1:nob,groupval)
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
    second_prices <- lapply(data_sorted, function(x) to.period(x, period = "seconds", OHLC = FALSE))
    RK <- unlist(lapply(second_prices, tsrv))
    BRK <- diag(RK^0.5) %*% blockspace %*% diag(RK^0.5)
    BRK <- (BRK + t(BRK))/2
    rownames(BRK) <- names(data_sorted)
    colnames(BRK) <- names(data_sorted)
    BRK <- BRK[names(midquotedata), names(midquotedata)]
    saveRDS(BRK, paste0("BRK_", date, "_", nob, ".rds"))
}
