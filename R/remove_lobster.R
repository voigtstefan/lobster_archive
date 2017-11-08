#' @title Remove lobster files
#' This function removes extracted lobster data files
#' @param ticker string Name of the ticker
#' @param date Date (string)
#' @param nlevels Number of Orderbook Levels available
#' @param folder Relative path to file storage
#' @export
remove_lobster <- function(ticker, date, nlevels = 1, folder = ".") {
    orderbook_file <- paste(paste(ticker, strftime(date, "%Y-%m-%d"), 34200000, 57600000, "orderbook", nlevels,
        sep = "_"), "csv", sep = ".")
    message_file <- paste(paste(ticker, strftime(date, "%Y-%m-%d"), 34200000, 57600000, "message", nlevels, sep = "_"),
        "csv", sep = ".")
    file.remove(c(paste0(folder, "/", message_file), paste0(folder, "/", orderbook_file)))
}
