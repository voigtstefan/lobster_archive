#' extract_LOBSTER_files
#'
#' This function extracts 7z files containing LOBSTER date (NOTE: 7z has to be included into the system PATH!) 
#' @export
extract_LOBSTER_files <- function(date, ticker, startdate = toDate(2007,6,27),enddate = toDate(2016,6,27),starttrade = 9.5*60*60, endtrade = 16*60*60,nlevels = 1){
	zipfile <- paste(ticker,'_',strftime(startdate,'%Y-%m-%d'),'_',strftime(enddate,'%Y-%m-%d'),'_1.7z',sep='')
	command <- paste('7z e ',zipfile,' *',strftime(date,'%Y-%m-%d'),'*.csv -r',sep='')
	system(command)
	orderbook_file <- paste(paste(ticker , strftime(date,'%Y-%m-%d') ,starttrade*1000,endtrade*1000,"orderbook" ,nlevels ,sep = "_"),"csv",sep = ".")
	message_file <- paste(paste(ticker , strftime(date,'%Y-%m-%d'),starttrade*1000,endtrade*1000,"message" ,nlevels ,sep = "_"),"csv",sep = ".")
	return <- c(orderbook_file,message_file)
	}
