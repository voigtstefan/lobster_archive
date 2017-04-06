#' plot_lobster
#'
#' @import ggplot2
#' @importFrom scales date_breaks
#' @export
plot_lobster<-function(v,var,date=NA,title=''){
    if(is.na(date)) date=as.Date(v$Time[1])
    ggplot(v,aes(x=as.POSIXct(date,tz=Sys.timezone())+Secs,y=eval(parse(text=var))))+geom_line()+ scale_x_datetime(expand=c(0,0),breaks=date_breaks("1 hour"), labels=date_format("%H:%M",tz=Sys.timezone()))+xlab('Time') + ylab(var) + ggtitle(title)+ theme_bw()
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
#  	cnt[cnt==0]<-1
	return(cnt)
}

#' base_mapply
#'
#' Base mapply function
#' Use this function to compute 'rolling-window'-like calculations with columnd depend window sizes.
#' e.g. if you want to compute the rolling mean over all trades which took place during the last 1.5 seconds
#' the command is base_mapply(lobster$Price,width=applyNS(lobster$Secs),FUN=mean)
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

#' Computes the Parzen Kernel
#'
#' @export
parzen.kernel<- function(x){
  anstar = rep(0,length(x))	
  anstar[x > 1] = 0
  anstar[x <= 0.5] = 1 - 6*x[x<= 0.5]^2 +6*x[x <= 0.5]^3
  anstar[(x <= 1) & (x > 0.5)] = 2*(1-x[x <= 1 & (x > 0.5)])^3
  anstar[x < 0] = 0
  return(abs(anstar))
}

#' Exponential kernel
#' 
#' @export
exponential.kernel <- function(x) exp(-abs(x))*(x<=0)


#' Autocovariane
#' 
#' computes the Autocovariance matrices
#' inputs are Refresh time sampled returns and optimal Hval
#' @export
autocovariance<-function(rftdata, hval){
  GammaH = t(rftdata[(abs(hval)+1):nrow(rftdata),])%*%rftdata[1:(nrow(rftdata)-abs(hval)),]
  if(hval<0) return(t(GammaH))
  if(hval>=0) return(GammaH)
}


#' IV estimate based on 20 minute RV estimates
#' @importFrom lubridate hour
#' @importFrom lubridate minute
#' @importFrom lubridate second
#' @export
IVhat_f <- function(rftdata){
  seconds <- cbind(hour(time(rftdata)),minute(time(rftdata)),second(time(rftdata)))%*%c(3600,60,1)	
  prevtime <- rep(NA,1199)
  for (sec in 1:1199){
    grid <- seq(from=34200+sec, by=20*60, to=57600)
    prevtime[sec] <- sum(rftdata[findInterval(grid,seconds)]^2,na.rm=TRUE)
  }
  return(mean(prevtime,na.rm=TRUE))
}
