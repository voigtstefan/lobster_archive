#' smoothing
#' @export
brk_smoothing <- function(date, periods, folder='.', nobs=4){
    all.files <- list.files(path=folder,pattern='BRK',full.names=TRUE)
    all.dates <- as.Date(strptime(all.files,paste0(folder,'/BRK_',nobs,'_%Y-%m-%d')))
    ind <- which(all.dates%in%date)-0:(periods-1)
    ind <- ind[ind>0]
    files <- all.files[ind]
    s1 <- readRDS(files[1])
    for(s in 1:length(files)) s1 <- s1 + readRDS(files[s])
    s1 <- s1/length(files)
    return(s1)
    }
