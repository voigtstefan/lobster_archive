#' create midprice
#'
#'Cleans quote data according to Barndorff-Nielsen et al. (2009)
#'  Delete entries with a time stamp outside 9:30 am until 4 pm (IMPLEMENTED)
#'  Delete entries with bid, ask or transaction price equal to zero. (IMPLEMENTED)
#'  Quotes:
#' When multiple quotes have the same time stamp, we replace all these with a single entry
#'   with the median bid and median ask price.
#' Delete entries for which the spread is negative. (IMPLEMENTED)
#' Delete entries for which the spread is more that 50 times the median spread on that day. (IMPLEMENTED)
#' Delete entries for which the mid-quote deviated by more than 10 mean absolute deviations
#'  from a rolling centred median (excluding the observation under consideration) of 50
#'  observations (25 observations before and 25 after). (IMPLEMENTED)
#' Trades:
#' If multiple transactions have the same time stamp, use the median price.
#' This function generates the Ledoit-Wolf covariance estimator and mean vector as inputs to draw from the predictive posterior distribution
#' @importFrom zoo rollmedian
#' @importFrom stats median
#' @export
create_midprice <- function(ticker, date, nlevels = 1, folder = ".") {

    start <- 3600 * 9.5
    end <- 3600 * 16
    dataM <- readin_lobster(ticker, date, nlevels, output = "MB", folder)
    numberObservations <- dim(dataM)[1]
    dataM <- dataM %>% filter(Secs > start & Secs < end & Price > 0, Type != 6)

    tradehaltIdx = which(dataM[, 2] == 7 & dataM[, 5] == -1)
    tradequoteIdx = which(dataM[, 2] == 7 & dataM[, 5] == 0)
    traderesumeIdx = which(dataM[, 2] == 7 & dataM[, 5] == 1)

    if (length(tradehaltIdx) == 0 & length(tradequoteIdx) == 0 & length(traderesumeIdx) == 0)
        print("No trading halts detected.")
    if (length(tradehaltIdx) != 0)
        cat("Data contains trading halt! at time stamp(s)", dataM[tradehaltIdx, 1], "\n")
    if (length(tradequoteIdx) != 0)
        cat(" Data contains quoting message! at time stamp(s)", dataM[tradequoteIdx, 1], "\n")
    if (length(traderesumeIdx) != 0)
        cat(" Data resumes trading! at time stamp(s) ", dataM[traderesumeIdx, 1], "\n")

    timeindex <- dataM$Secs >= start & dataM$Secs <= end

    dataO <- readin_lobster(ticker, date, nlevels, output = "OB", folder)
    dataO <- dataO[timeindex, ]
    dataO <- dataO %>% mutate(Spread = Askp1 - Bidp1)

    medspread <- median(dataO$Spread)
    dataO <- dataO %>% filter(Spread >= 0 & Spread < 50 * medspread)
    dataO <- dataO %>% mutate(temp.roll = rollmedian(x = Midquote, 25, align = "center", fill = median(Midquote)))
    dataO <- dataO %>% filter(Midquote <= 10 * temp.roll)
    dataO <- dataO %>% group_by(Secs) %>% summarise(Midquote = median(Midquote), Time = Time[1])
    return(dataO)
}
