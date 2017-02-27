#' remove_lobster
#'
#' This function extracts 7z files containing LOBSTER date (NOTE: 7z has to be included into the system PATH!) 
#' @importFrom magrittr "%>%"
#' @export
remove_lobster<-function(date,ticker,nlevels=1){
    orderbook_file <- paste(paste(ticker, strftime(date, "%Y-%m-%d"),
        34200000, 57600000, "orderbook", nlevels, sep = "_"),
        "csv", sep = ".")
    message_file <- paste(paste(ticker, strftime(date, "%Y-%m-%d"),
        34200000, 57600000, "message", nlevels, sep = "_"), "csv",
        sep = ".")
	file.remove(c(message_file,orderbook_file))
}