#' regularization based on factor structure following Hautsch,Kyj, Oomen 2010
#' @export

baing_clean <- function(cov_org,factors=3) {
    p <- nrow(cov_org)
    corr_org <- cov2cor(cov_org)
    vals <- eigen(corr_org)
    lambda <- vals$values
    u <- vals$vectors
    lambda.new <- lambda[1:factors]
    u.new <- u[,1:factors]
    mat.new <- u.new%*%diag(lambda.new)%*%t(u.new)
    mat.new <- mat.new +diag(p) - diag(diag(mat.new))
    cov_cl <- diag(diag(cov_org)^(0.5)) %*% mat.new %*% diag(diag(cov_org)^(0.5))
    cov_cl[lower.tri(cov_cl)] <- t(cov_cl)[lower.tri(cov_cl)]
    rownames(cov_cl) <- rownames(cov_org)
    colnames(cov_cl) <- colnames(cov_cl)
    return(cov_cl)
}
