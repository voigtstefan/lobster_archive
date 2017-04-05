#' eigenvalue cleaning following Hautsch,Kyj, Oomen 2010
#' @export

eigenval_clean <- function(cov_org){

p <- nrow(cov_org)
n_obs <- 20000
corr_org <-  diag(diag(cov_org)^(-.5))%*%cov_org%*%diag(diag(cov_org)^(-.5))
corr_org[is.na(corr_org)]<-0
vals  <- eigen(corr_org)
s <- vals$values
u <- vals$vectors
vals <- sort(s,decreasing=TRUE,index.return=TRUE)
lambda <- vals$x
ix <- vals$ix
u <- u[,ix]
lambda_max <- (1-lambda[1]/p)*(1+p/n_obs+2*sqrt(p/n_obs))
lambda_sm <- lambda[lambda<=lambda_max]
lambda_t=sum(lambda_sm[lambda_sm>0])/length(lambda_sm)
lambda_cl=c(lambda[lambda>lambda_max],lambda_t*rep(1,length(lambda_sm)))
corr_cl <- u%*%diag(lambda_cl)%*%t(u)
cov_cl <- diag(diag(cov_org)^(.5))%*%corr_cl%*%diag(diag(cov_org)^(.5))
rownames(cov_cl) <- rownames(cov_org)
colnames(cov_cl) <- colnames(cov_cl)
return(cov_cl)
}