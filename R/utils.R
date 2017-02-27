#' plot_lobster
#'
#' @importFrom ggplot2 ggplot scale_x_datetime
#' @export
plot_lobster<-function(v,var,date=NA){
    if(is.na(date)) date=as.Date(v$Time[1])
    ggplot(v,aes(x=as.POSIXct(date,tz=Sys.timezone())+Secs,y=eval(parse(text=var))))+geom_line()+ scale_x_datetime(breaks=date_breaks("1 hour"), labels=date_format("%H:%M",tz=Sys.timezone()))+xlab('Time')
   }
#' applyNS
#'
#' Counts number of trades during last k seconds
#' @export
applyNS 	<- function(s,k=1.5){
		cnt 	<- numeric(length(s))
  		for(i in 1:length(s)){
    			res 	<- (s[(1+i):length(s)] - s[1:(length(s)-i)]) <= k
    			cnt[(1+i):length(s)] <- cnt[(1+i):length(s)] + res
    			if(!any(res)) break
	  	}
  		cnt[cnt==0]<-1
		return(cnt)
}

#' base_mapply
#'
#' Base mapply function
#' @export

base_mapply 	<- function(x, width, FUN, ...){
		FUN <- match.fun(FUN)
  		f <- function(i, width, data){
    			if(i < width) return(NA_real_)
    			return(FUN(data[(i-(width-1)):i], ...))
 		}
	  	mapply(FUN = f, 
         	seq_along(x), width,
         	MoreArgs = list(data = x))
}
