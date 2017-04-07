#' readin_lobster
#'
#' This function reads-in extracted Lobster Files (either Orderbook, Messagebook or both)
#' @import dplyr
#' @import dtplyr
#' @importFrom magrittr "%>%"
#' @importFrom data.table fread
#' @export
readin_lobster	<- function(ticker,date,nlevels=1,output='ALL', folder='.'){
	if (missing(ticker))
		stop("Need to specify valid ticker.")
	if (missing(date)||class(try(as.Date(date,format='%d-%m-%Y %H:%M:%S')))%in%'try-error'||is.na(date))
        	stop("Need to specify valid date.")
	start		<- 3600*9.5*1000
	end 		<- 3600*16 *1000
	mes 		<- paste0(paste(ticker,date,start,end,'message',nlevels,sep='_'),'.csv')
	ord 		<- paste0(paste(ticker,date,start,end,'orderbook',nlevels,sep='_'),'.csv')

	if (all(c(mes,ord) %in%dir(folder)==FALSE)) stop('Files cannot be found in folder')

	m_raw		<- fread(paste(folder,mes,sep='/'),integer64='numeric')
	names(m_raw)	<- c('Secs','Type','OrderID','Size','Price','TradeDirection')
	m		<- m_raw%>%transform(Price = Price/10000, Time = as.POSIXct(trunc(as.POSIXct(date), units="days") + Secs))  

	if(!identical(output,'MB')){
		o_raw		<- fread(paste(folder,ord,sep='/'),integer64='numeric')
		names(o_raw)	<- paste0(rep(c('Askp','Asks','Bidp','Bids'),each=1,times=nlevels),rep(1:nlevels,each=4))
		o		<- o_raw%>% 
					mutate_each(funs(./10000), seq(from=1,to=nlevels*4,by=2))%>%
					transform(Spread=Askp1-Bidp1,Midquote=Bidp1+(Askp1-Bidp1)/2)
	}
	if(identical(output,'ALL'))
		return(cbind(m,o))
	if(identical(output,'OB')){
		o 	<- o%>%transform(Secs=m$Secs,Time=m$Time)
		return(o)}
	if(identical(output,'MB'))
		return(m)
	}
