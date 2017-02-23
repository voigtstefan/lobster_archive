#' readin_lobster
#'
#' This function extracts 7z files containing LOBSTER date (NOTE: 7z has to be included into the system PATH!) 
#' @import dplyr
#' @import dtplyr
#' @export
readin_lobster	<- function(ticker,date,nlevels=1,output='ALL'){
	if (missing(ticker))
		stop("Need to specify valid ticker.")
	if (missing(date)||class(try(as.Date(date,format='%d-%m-%Y %H:%M:%S')))%in%'try-error'||is.na(date))
        	stop("Need to specify valid date.")
	if (Sys.timezone()!='America/New_York'){
		Sys.setenv(TZ='America/New_York')
		cat('System time zone changed to America/ New York \n')}
	start		<- 3600*9.5*1000
	end 		<- 3600*16 *1000
	mes 		<- paste0(paste(ticker,date,start,end,'message',nlevels,sep='_'),'.csv')
	ord 		<- paste0(paste(ticker,date,start,end,'orderbook',nlevels,sep='_'),'.csv')

	if (all(c(mes,ord) %in%dir()==FALSE)) stop('Files cannot be found in folder')

	m_raw		<- fread(mes,integer64='numeric')
	names(m_raw)	<- c('Secs','Type','OrderID','Size','Price','TradeDirection')
	m		<- m_raw%>%transform(Price = Price/10000, Time = as.POSIXct(trunc(as.POSIXct(date), units="days") + Secs))  

	if(!identical(output,'MB')){
		o_raw		<- fread(ord,integer64='numeric')
		names(o_raw)	<- paste0(rep(c('Askp','Asks','Bidp','Bids'),each=1,times=nlevels),rep(1:nlevels,each=4))
		o		<- o_raw%>% 
					mutate_each(funs(./10000), seq(from=1,to=nlevels*4,by=2))%>%
					transform(Spread=Askp1-Bidp1,Midquote=Bidp1+(Askp1-Bidp1)/2)
	}
	if(identical(output,'ALL'))
		return(cbind(m,o))
	if(identical(output,'OB')){
		o 	<- o%>%mutate(Secs=m$Secs,Time=m$Time)
		return(o)}
	if(identical(output,'MB'))
		return(m)
	}
