#' eigenvalue cleaning following Hautsch,Kyj, Oomen 2010
#' @export

eigenval_clean <- function(cov_org,n_obs=2000) {
    p <- nrow(cov_org)
    corr_org <- cov2cor(cov_org)
    vals <- eigen(corr_org)
    lambda <- vals$values
    u <- vals$vectors
    lambda_max <- (1 - max(lambda)/p) * (1 + p/n_obs + 2 * sqrt(p/n_obs))
    lambda_sm <- lambda[lambda <= lambda_max]
    lambda_t <- sum(lambda_sm[lambda_sm > 0])/length(lambda_sm)
    lambda[lambda<lambda_max]<-lambda_t
    corr_cl <- vals$vectors %*% diag(lambda) %*% t(vals$vectors)
    cov_cl <- diag(diag(cov_org)^(0.5)) %*% corr_cl %*% diag(diag(cov_org)^(0.5))
    rownames(cov_cl) <- rownames(cov_org)
    colnames(cov_cl) <- colnames(cov_cl)
    return(cov_cl)
}
