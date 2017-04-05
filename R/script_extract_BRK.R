brk_covariance <- function(ticker, date, nob = 6, folder='.')
library(xts) # as.xts
N <- length(ticker)
midquotedata <- vector("list", N)
names(midquotedata) <- ticker

sdate<-as.POSIXct(date,tz='UTC')
attr(sdate,'tzone')<-'UTC'

cstar <- (12^2/0.269)^(1/5)

for (i in 1:N){
	stock <- ticker[i]
		tryCatch({
			extract_lobster(stock,date, folder=folder)
			tmp <- readin_lobster(stock,date)
			tmp <- as.xts(tmp$Midquote,order.by=sdate+tmp$Secs)
			midquotedata[[i]]<-log(tmp)
		},error=function(cond)cat('Not existent: ',stock))
}

cat('# Read-in complete \n')
nobs <- unlist(as.numeric(as.character(lapply(midquotedata,nrow))))
not_existent<-ticker_names[is.na(nobs)]
midquotedata[is.na(nobs)]<-NULL
nobs <- na.omit(nobs)
nobs_index <- order(nobs,decreasing=TRUE)
Nadj<-length(midquotedata)
groups <-  split(names(midquotedata)[nobs_index], ceiling(1:Nadj/(Nadj/nob)))
blockspace <- matrix(NA,ncol=Nadj,nrow=Nadj)
myblocks <- unlist(lapply(groups,length))
data_sorted <- midquotedata[names(midquotedata)[nobs_index]]
	ParzenKernel<-function(xx){
	# Computes parzen kernel for value xx
		anstar = rep(0,length(xx))	
		anstar[xx > 1] = 0
     		anstar[xx <= 0.5] = 1 - 6*xx[xx <= 0.5]^2 +6*xx[xx <= 0.5]^3
		anstar[(xx <= 1) & (xx > 0.5)] = 2*(1-xx[xx <= 1 & (xx > 0.5)])^3
     	 	anstar[xx < 0] = 0
     	 	return(abs(anstar))
	}
	IVhat_f <- function(pdata){
		seconds <- cbind(hour(time(pdata)),minute(time(pdata)),second(time(pdata)))%*%c(3600,60,1)	
		prevtime <- rep(NA,1199)
		for (sec in 1:1199){
			grid <- seq(from=starttrade+sec, by=20*60, to=endtrade)
			prevtime[sec] <- sum(pdata[findInterval(grid,seconds)]^2,na.rm=TRUE)
		}
 		return(mean(prevtime,na.rm=TRUE))
	}
	autocovariance<-function(rftdata, hval){
 		# computes the Autocovariance matrices
		# inputs are Refresh time sampled returns and optimal Hval
		GammaH = t(rftdata[(abs(hval)+1):nrow(rftdata),])%*%rftdata[1:(nrow(rftdata)-abs(hval)),]
		if(hval<0) return(t(GammaH))
		if(hval>=0) return(GammaH)
	}
	groupval<-function(z){
		GLrange = 1:sum(myblocks[1:(nob-z+1)])   
		print(z) 
		tmpspace <- matrix(NA,ncol=Nadj,nrow=Nadj)
		for(w in 1:z){
		      if(w==1) MYrange <- GLrange
        		if(w!=1) MYrange <- GLrange + sum(myblocks[1:(w-1)])
			rt_prices <- refreshTime(data_sorted[MYrange])
			rt_returns <- diff(rt_prices)[-1,]
			T <- nrow(rt_returns)
			omegaest <- function(returns) max(sum(returns[-1]*returns[1:(nrow(returns)-1)])/(nrow(returns)-1),0)
			omegahat2 <- unlist(lapply(rt_returns,omegaest))
			IVhat <- unlist(lapply(rt_returns,IVhat_f))
			noise <- omegahat2/IVhat
			Hval <- cstar*noise^(2/5)*unlist(lapply(rt_returns,nrow))^(3/5)
			Hvalstar <- min(100,max(0,mean(Hval,na.rm=FALSE)))
			bans = matrix(0,nrow=ncol(rt_returns),ncol=ncol(rt_returns))
			for (i in -(floor(Hvalstar)+1):(floor(Hvalstar)+1)){
				kernw = ParzenKernel(abs(i)/(Hvalstar+1))
    				autocov = autocovariance(rt_returns, i)
    				bans = bans + kernw*autocov
			}
			V <- diag(bans)^(-0.5)
			R <- diag(V,nrow=length(MYrange))%*%bans%*%diag(V,nrow=length(MYrange))
			R <- (R+t(R))/2
			tmpspace[MYrange,MYrange] <- R	
		}
      	return(tmpspace)
	}        

if(Sys.info()['sysname']!='Windows'){
	no_cores <- detectCores() 
	cl<-makeCluster(max(nob,no_cores),type='FORK')
	a<-parLapply(cl, 1:nob, groupval)
	stopCluster(cl)
}
if(Sys.info()['sysname']=='Windows') a<-lapply(1:nob,groupval)
	
for(z in 1:nob){
	GLrange = 1:sum(myblocks[1:(nob-z+1)])   
	for(w in 1:z){
     		if(w==1) MYrange <- GLrange
		if(w!=1) MYrange <- GLrange + sum(myblocks[1:(w-1)])
		blockspace[MYrange,MYrange]<-a[[z]][MYrange,MYrange]
	}
}

rTSCov_adju <- function(data){rTSCov(exp(data),K=min(300,floor(nrow(data)/11)))}
RK <- unlist(lapply(data_sorted,rTSCov_adju)) 
BRK <- diag(RK^0.5)%*%blockspace%*%diag(RK^0.5)
BRK <- (BRK+t(BRK))/2
rownames(BRK) <- names(data_sorted)
colnames(BRK) <- names(data_sorted)
BRK <- BRK[names(midquotedata),names(midquotedata)]
saveRDS(BRK,output_file)

cat('# Iteration ',toString(date),' finished \n')
