#' @title Regularization of Variance-Covariance Matrix (Hautsch, Kyj, Oomen; 2010)
#' @param cov_org N x N variance-covariance matrix
#' @param factors (default=3) number of retained eigenvalues
#' @return cov_cl N x N cleaned variance-covariance matrix
#' @export

baing_cleaning <- function(cov_org, factors = 3) {
    p <- nrow(cov_org)  # Dimension of matrix
    corr_org <- cov2cor(cov_org)  # Transformation to correlation matrix
    vals <- eigen(corr_org)  # Eigenvalue decomposition
    lambda <- vals$values  # Eigenvalues
    u <- vals$vectors  # Eigenvectors
    lambda.new <- lambda[1:factors]  # extract largest eigenvalues
    u.new <- u[, 1:factors]  # extract corresponding eigenvectors
    mat.new <- u.new %*% diag(lambda.new) %*% t(u.new)  # Recompose matrix
    mat.new <- mat.new + diag(p) - diag(diag(mat.new))  # set diagonal elements to 1
    cov_cl <- diag(diag(cov_org)^(0.5)) %*% mat.new %*% diag(diag(cov_org)^(0.5))  # Recompose covariances
    cov_cl[lower.tri(cov_cl)] <- t(cov_cl)[lower.tri(cov_cl)]  # Ensure symmetry
    rownames(cov_cl) <- rownames(cov_org)
    colnames(cov_cl) <- colnames(cov_cl)
    return(cov_cl)
}
