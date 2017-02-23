#' create midprice
#'
#'Cleans quote data according to Barndorff-Nielsen et al. (2009)
#' 	1. Delete entries with a time stamp outside 9:30 am until 4 pm (IMPLEMENTED)
#' 	2. Delete entries with bid, ask or transaction price equal to zero. (IMPLEMENTED)
#' 	3. Quotes: 
#'		a. When multiple quotes have the same time stamp, we replace all these with a single entry
#'		   with the median bid and median ask price.
#'		b. Delete entries for which the spread is negative. (IMPLEMENTED)
#'		c. Delete entries for which the spread is more that 50 times the median spread on that day. (IMPLEMENTED)
#'		d. Delete entries for which the mid-quote deviated by more than 10 mean absolute deviations
#'		   from a rolling centred median (excluding the observation under consideration) of 50
#'		   observations (25 observations before and 25 after). (IMPLEMENTED)
#'	4. Trades:
#'		b. If multiple transactions have the same time stamp, use the median price.
#' This function generates the Ledoit-Wolf covariance estimator and mean vector as inputs to draw from the predictive posterior distribution 
#' @export
create_midprice <- function(filenames,date,starttrade = 9.5*60*60, endtrade = 16*60*60){

	orderbook_file <- filenames[1]
	message_file <- filenames[2]
	dataM <- fread(message_file, header = F, sep = ',')
	columns <- c( "Time" , "Type" , "OrderID" , "Size" , "Price" , "TradeDirection" )
	setnames(dataM, columns)
	numberObservations <- dim(dataM)[1]
	dataM <- dataM%>%filter(Time>starttrade& Time<endtrade & Price>0)

	tradehaltIdx = which(dataM[,2] == 7 & dataM[,5] == -1 );
	tradequoteIdx = which(dataM[,2] == 7 & dataM[,5] == 0 );
	traderesumeIdx = which(dataM[,2] == 7 & dataM[,5] == 1 );
		
	if(length(tradehaltIdx)==0 & length(tradequoteIdx)==0  & length(traderesumeIdx)==0 )
		print("No trading halts detected.")
	if(length(tradehaltIdx) !=0)
		cat("Data contains trading halt! at time stamp(s)", dataM[tradehaltIdx,1],"\n" )
	if(length(tradequoteIdx) !=0)
		cat(" Data contains quoting message! at time stamp(s)", dataM[tradequoteIdx,1], "\n")
	if(length(traderesumeIdx) !=0)
		cat(" Data resumes trading! at time stamp(s) ", dataM[traderesumeIdx,1],"\n")

	timeindex <-dataM$Time>=starttrade & dataM$Time<=endtrade

	dataO <- fread(orderbook_file, header = F, sep = ',')
	columns2 <- c("ASKp1" , "ASKs1" , "BIDp1",  "BIDs1")

	setnames(dataO, columns2)

	dataO <- dataO[timeindex,]
	dataO$Time <- dataM$Time
	dataO <- dataO%>%mutate(ASKp1 = ASKp1/10000, BIDp1 = BIDp1/10000,MIDp = (ASKp1+BIDp1)/2, Spread=ASKp1-BIDp1)
	
	medspread <- median(dataO$Spread)
	dataO <- dataO%>%filter( Spread >= 0 & Spread < 50*medspread)
	dataO <- dataO%>%mutate(temp.roll = rollmedian(x=MIDp,25,align='center',fill = median(MIDp)))
	dataO <- dataO%>%filter( MIDp <= 10* temp.roll)
	dataO<-dataO%>%group_by(Time)%>%summarise(MIDp=median(MIDp))
	dataO$Date <- as.POSIXct(date, units="days") +dataO$Time
	return(dataO)
	}
