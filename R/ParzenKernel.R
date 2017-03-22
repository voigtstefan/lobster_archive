#' Computes the Parzen Kernel
#'
#' @export
ParzenKernel<- function(xx){
        anstar = rep(0,length(xx))	
        anstar[xx > 1] = 0
        anstar[xx <= 0.5] = 1 - 6*xx[xx <= 0.5]^2 +6*xx[xx <= 0.5]^3
	    anstar[(xx <= 1) & (xx > 0.5)] = 2*(1-xx[xx <= 1 & (xx > 0.5)])^3
        anstar[xx < 0] = 0
        return(abs(anstar))
	  }

