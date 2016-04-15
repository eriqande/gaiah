
#' Parametric bootstrap for rescaling a la the vander zanden appendix
#'
#' This is a vectorized and \strong{much} more efficient implementation
#' of the original \code{rescale} function from the Vander Zanden appendix.
#' It takes the output of \code{\link{group_birds_by_location}} directly
#' and does the parametric bootstrapping for Reps samples.
#' @param SBL the data frame that summarizes the isotope feather data and
#' isoscape predictions at each location. This must have columns of \code{cnt},
#' \code{meanH}, \code{sdH}, \code{meaniso}, \code{sdiso}
#' @param Reps Number of simulated regressions to do.  Default is 1000.
#' @export
vza_rescale <- function(SBL, Reps = 1000) {

  D <- as.data.frame(SBL) # make sure to un-tbl-df it if need be

  # make a list of matrices that hold simulated values for tissue  that
  # we will bind together into one big matrix.  The rows are the simulated
  # isotope values from the birds and the columns are the Reps
  tmp <- lapply(1:nrow(D), function(i) {
    n <- D[i,"cnt"]
    rnorm(n * Reps, mean = D[i, "meanH"], sd = D[i, "sdH"]) %>%
      matrix(ncol = Reps)
  })
  tissue_mat <- do.call(rbind, tmp)

  # do the same for the precip values
  tmp <- lapply(1:nrow(D), function(i) {
    n <- D[i,"cnt"]
    rnorm(n * Reps, mean = D[i, "meaniso"], sd = D[i, "sdiso"]) %>%
      matrix(ncol = Reps)
  })
  precip_mat <- do.call(rbind, tmp)

  # now we just do the regression of each column (1,...,Reps) of the tissue_mat and the precip_mat
  # and we grab the slopes and intercepts and return them as a data frame
  lapply(1:ncol(tissue_mat), function(i) {
    lm(tissue_mat[,i] ~ precip_mat[,i]) %>%
      coef() %>%
      unname()
  }) %>%
    unlist() %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    as.data.frame() %>%
    setNames(c("intercept", "slope")) %>%
    dplyr::tbl_df()

}





######### BELOW HERE ARE ALL THE ORIGINAL VERSIONS OF THE FUNCTIONS ##################
###### RESCALING FUNCTION #######

# rescale the isoscape and feather data
#
# This function conducts 1000 simulated regressions sampled from normal distributions of data randomly generated from the means and SDs at each site.
# The output contains the slopes and intercepts of each of the 1000 regression lines, from which a mean or distrubtion can be extracted.
# Start with a .csv table that has the calibration data organized by site with tissue mean and SD,
# number of individuals per site, and the corresponding precip mean and SD extracted from the appropriate precip isoscape
#
# Function includes:
#calibration =  the table that you would get by reading table: table = the filename (with directory, if applicable) from which to load the data
#siteID = column # containing unique site IDS
#count = column # containing number of individuals sampled per site
#tissue.mean = column # containing mean d2H tissue values of individuals sampled at each site
#tissue.SD = column # containing SD of d2H tissue values of individuals sampled at each site
#precip.mean = column # containing mean d2H precip values at each site
#precip.SD = column # containing SD of d2H precip values at each site
rescale <- function(calibration, siteIDs, count, tissue.mean, tissue.SD, precip.mean, precip.SD) {
#  calibration <- read.table(table, header=TRUE,
#                            sep=",", na.strings="NA")

  AllSites <- unique(calibration[, siteIDs])
  slopes <- vector('numeric', length=1000)
  intercepts <- vector('numeric', length=1000)


  for (k in 1:1000){
    print(paste("Hold your horses, I am working on k =", k))
    counter <- 1
    tissue.d2H <- vector('numeric', length=sum(calibration[,count]))
    precip.d2H <- vector('numeric', length=sum(calibration[,count]))

    # eric added this line to see what is happening regarding over-running tissue and precip.d2H
    print(paste("Original length of tissue.d2H: ", length(tissue.d2H)))

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

      }}
      print(paste("Ending length of tissue.d2H: ", length(tissue.d2H)))
    }

  slope <- mean(slopes)
  intercept <-mean(intercepts)
  return(data.frame(slopes, intercepts))
}
