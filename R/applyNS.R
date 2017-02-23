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

