

###### RESCALING FUNCTION #######

#' rescale the isoscape and feather data
#'
#' This function conducts 1000 simulated regressions sampled from normal distributions of data randomly generated from the means and SDs at each site.
#' The output contains the slopes and intercepts of each of the 1000 regression lines, from which a mean or distrubtion can be extracted.
#' Start with a .csv table that has the calibration data organized by site with tissue mean and SD,
#' number of individuals per site, and the corresponding precip mean and SD extracted from the appropriate precip isoscape
#'
#' Function includes:
#table = the filename (with directory, if applicable) from which to load the data
#siteID = column # containing unique site IDS
#count = column # containing number of individuals sampled per site
#tissue.mean = column # containing mean d2H tissue values of individuals sampled at each site
#tissue.SD = column # containing SD of d2H tissue values of individuals sampled at each site
#precip.mean = column # containing mean d2H precip values at each site
#precip.SD = column # containing SD of d2H precip values at each site

rescale <- function(table, siteIDs, count, tissue.mean, tissue.SD, precip.mean, precip.SD) {
  calibration <- read.table(table, header=TRUE,
                            sep=",", na.strings="NA")

  AllSites <- unique(calibration[, siteIDs])
  slopes <- vector('numeric', length=1000)
  intercepts <- vector('numeric', length=1000)

  for (k in 1:1000){
    print(paste("Hold your horses, I am working on k =", k))
    counter <- 1
    tissue.d2H <- vector('numeric', length=sum(calibration[,count]))
    precip.d2H <- vector('numeric', length=sum(calibration[,count]))

    for (i in 1:length(AllSites)){
      Site.i <- AllSites[i]
      Data.i <- calibration[calibration[,siteIDs]==Site.i,]
      n <- Data.i[,count]

      for (j in 1:n){
        tissue.d2H[counter:(counter+n-1)] <- rnorm(n, mean=Data.i[1, tissue.mean], sd=Data.i[1, tissue.SD])
        precip.d2H[counter:(counter+n-1)] <- rnorm(n, mean=Data.i[1, precip.mean], sd=Data.i[1, precip.SD])
        counter <- counter+1
        lmResult.k <- lm(tissue.d2H~precip.d2H)
        intercepts[k] <- coef(lmResult.k)[1]
        slopes[k] <- coef(lmResult.k)[2]

      }}}

  slope <- mean(slopes)
  intercept <-mean(intercepts)
  return(data.frame(slopes, intercepts))
}
