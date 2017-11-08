#' @title Read-in lobster files
#'
#' This function reads-in extracted Lobster Files (either Orderbook, Messagebook or both)
#' @param ticker string Name of the ticker
#' @param date Date (string)
#' @param nlevels Number of Orderbook Levels available
#' @param output Either 'ALL', 'OB' (only orderbook), or 'MB' (only messages)
#' @param folder Relative path to file storage
#' @importFrom magrittr "%>%"
#' @importFrom data.table fread
#' @importFrom dplyr mutate_each
#' @importFrom dplyr filter
#' @export
readin_lobster <- function(ticker, date, nlevels = 1, output = "ALL", folder = ".") {
    if (missing(ticker))
        stop("Need to specify valid ticker.")
    if (missing(date) || class(try(as.Date(date, format = "%d-%m-%Y %H:%M:%S"))) %in% "try-error" || is.na(date))
        stop("Need to specify valid date.")
    start <- 3600 * 9.5 * 1000
    end <- 3600 * 16 * 1000
    mes <- paste0(paste(ticker, date, start, end, "message", nlevels, sep = "_"), ".csv")
    ord <- paste0(paste(ticker, date, start, end, "orderbook", nlevels, sep = "_"), ".csv")

    if (all(c(mes, ord) %in% dir(folder) == FALSE))
        stop("Files cannot be found in folder")

    m_raw <- data.table::fread(paste(folder, mes, sep = "/"),
                                  integer64 = "numeric")
    names(m_raw) <- c("Secs", "Type", "OrderID", "Size", "Price", "TradeDirection")
    m <- transform(m_raw, Price = Price/10000,
                          Time = as.POSIXct(trunc(as.POSIXct(date), units = "days") + Secs))
    if (!identical(output, "MB")) {
        o_raw <- data.table::fread(paste(folder, ord, sep = "/"), integer64 = "numeric")
        names(o_raw) <- paste0(rep(c("Askp", "Asks", "Bidp", "Bids"), each = 1, times = nlevels), rep(1:nlevels,
            each = 4))
        o <- as.data.frame(o_raw) %>% dplyr::mutate_each(dplyr::funs(./10000), seq(from = 1, to = nlevels * 4, by = 2)) %>% transform(Spread = Askp1 -
            Bidp1, Midquote = Bidp1 + (Askp1 - Bidp1)/2)
    }
    if (identical(output, "ALL"))
        return(cbind(m, o)%>% dplyr::filter(Type!=6))
    if (identical(output, "OB")) {
        o <- transform(o, Secs = m$Secs, Time = m$Time)
        return(o)
    }
    if (identical(output, "MB"))
        return(m)
}
