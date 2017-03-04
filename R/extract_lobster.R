#' extract_lobster
#'
#' This function extracts 7z files containing LOBSTER date (NOTE: 7z has to be included into the system PATH!) 
#' @export
extract_lobster <- function(date, ticker,nlevels=1){
	orderbook_file <- paste(paste(ticker , strftime(date,'%Y-%m-%d') ,34200000,57600000,"orderbook" ,nlevels ,sep = "_"),"csv",sep = ".")
	message_file <- paste(paste(ticker , strftime(date,'%Y-%m-%d'),34200000,57600000,"message" ,nlevels ,sep = "_"),"csv",sep = ".")
	if(!all(file.exists(c(orderbook_file,message_file)))){
		zipfile <- paste0('../RawLobster/',ticker,'.7z')
		command <- paste0('7za e ',zipfile,' *',strftime(date,'%Y-%m-%d'),'*.csv -r',sep='')
		system(command)}
	return <- c(orderbook_file,message_file)
	}
