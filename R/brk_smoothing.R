#' smoothing
#' @export
brk_smoothing <- function(date, periods, folder='.', nobs=4){
    all.files <- list.files(path=folder,pattern='BRK')
    all.dates <- as.Date(strptime(all.files,'BRK_%Y-%m-%d'))

    files <- all.files[which(all.dates%in%date)-0:(periods-1)]
    s1 <- readRDS(files[1])
    for(s in 1:periods) s1 <- s1 + readRDS(files[s])
    s1 <- s1/periods
    return(s1)
    }
