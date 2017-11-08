#' @title Extract lobster files
#' This function extracts 7z files containing LOBSTER date (NOTE: 7z has to be included into the system PATH!)
#' @param ticker string Name of the ticker
#' @param date Date (string)
#' @param nlevels Number of Orderbook Levels available
#' @param folder Relative path to file storage
#' @return file names to verify that extraction worked
#' @export
extract_lobster <- function(ticker, date, nlevels = 1, folder = ".") {
    orderbook_file <- paste(paste(ticker, date, 34200000, 57600000, "orderbook", nlevels, sep = "_"), "csv",
        sep = ".")
    message_file <- paste(paste(ticker, date, 34200000, 57600000, "message", nlevels, sep = "_"), "csv", sep = ".")
    if (!all(file.exists(c(orderbook_file, message_file)))) {
        zipfile <- paste0(folder, "/", ticker, ".7z")
        command <- paste0("7za x ", zipfile, " *", strftime(date, "%Y-%m-%d"), "* -r", sep = "")
        system(command)
    }
    return(c(orderbook_file, message_file))
}
