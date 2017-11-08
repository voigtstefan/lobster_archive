#' @title Smoothing
#' Smoothes (BRK) matrices as in Hautsch et al (2012)
#' Warning: BRK matrices have to be named in the following fashion: BRK_number of blocks_ date
#' @param date End date (latest matrix included)
#' @param periods Number of periods (Default = 5 days)
#' @param folder Relative path to the stored BRK matrices
#' @param nobs Number of blocks (relevant for the naming convention)
#' @export
brk_smoothing <- function(date, periods = 5, folder = ".", nobs = 4) {
    all.files <- list.files(path = folder, pattern = "BRK", full.names = TRUE)
    all.dates <- as.Date(strptime(all.files, paste0(folder, "/BRK_", nobs, "_%Y-%m-%d")))
    ind <- which(all.dates %in% date) - 0:(periods - 1)
    ind <- ind[ind > 0]
    files <- all.files[ind]
    s1 <- readRDS(files[1])
    for (s in 1:length(files)) s1 <- s1 + readRDS(files[s])
    s1 <- s1/length(files)
    return(s1)
}
