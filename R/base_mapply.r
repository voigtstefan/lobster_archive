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
