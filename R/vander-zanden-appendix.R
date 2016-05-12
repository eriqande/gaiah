
#' Parametric bootstrap for rescaling a la the vander zanden appendix
#'
#' This is a vectorized and \strong{much} more efficient implementation
#' of the original \code{rescale} function from the Vander Zanden appendix.
#' It takes the output of \code{\link{group_birds_by_location}} directly
#' and does the parametric bootstrapping for vza_rescale_reps samples.
#' @param SBL the data frame that summarizes the isotope feather data and
#' isoscape predictions at each location. This must have columns of \code{cnt},
#' \code{meanH}, \code{sdH}, \code{meaniso}, \code{sdiso}
#' @param vza_rescale_reps Number of simulated regressions to do.  Default is 1000.
#' @return Returns a matrix with vza_rescale_reps rows.  Column 1 is "intercepts" and column
#' two is "slopes"
#' @export
vza_rescale <- function(SBL, vza_rescale_reps = 1000) {

  D <- as.data.frame(SBL) # make sure to un-tbl-df it if need be

  # make a list of matrices that hold simulated values for tissue  that
  # we will bind together into one big matrix.  The rows are the simulated
  # isotope values from the birds and the columns are the vza_rescale_reps
  tmp <- lapply(1:nrow(D), function(i) {
    n <- D[i,"cnt"]
    rnorm(n * vza_rescale_reps, mean = D[i, "meanH"], sd = D[i, "sdH"]) %>%
      matrix(ncol = vza_rescale_reps)
  })
  tissue_mat <- do.call(rbind, tmp)

  # do the same for the precip values
  tmp <- lapply(1:nrow(D), function(i) {
    n <- D[i,"cnt"]
    rnorm(n * vza_rescale_reps, mean = D[i, "meaniso"], sd = D[i, "sdiso"]) %>%
      matrix(ncol = vza_rescale_reps)
  })
  precip_mat <- do.call(rbind, tmp)

  # now we just do the regression of each column (1,...,vza_rescale_reps) of the tissue_mat and the precip_mat
  # and we grab the slopes and intercepts and return them as a data frame
  lapply(1:ncol(tissue_mat), function(i) {
    lm(tissue_mat[,i] ~ precip_mat[,i]) %>%
      coef() %>%
      unname()
  }) %>%
    unlist() %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    as.data.frame() %>%
    setNames(c("intercepts", "slopes")) %>%
    dplyr::tbl_df()
}


#' calculate a raster of mean and Var of expected tissue isotopes from precip data and resampled regressions
#'
#' This is a rewrite of the function \code{raster.conversion} from the Vander Zanden
#' appendix.  They expressed things in terms of the standard deviations, but they need to
#' be turned into variances, anyway, so that is what we've done here.  Following the notation
#' of paper on Wilson's warbler, this function computes $tilde{T}^{(mu)}$ (returned as
#' list component \code{mean.raster}) and $R^{(sigma^2)}$ (returned as list component
#' \code{var.raster})
#' @param iso_raster the raster of isotope precipitation values, for example, like that
#' produced by \code{\link{isomap2raster}}.
#' @param si  slopes and intercepts from the resampled regressions.  This is a data frame
#' with columns named "slopes" and "intercepts" like that returned by \code{\link{vza_rescale}}
#' @export
vza_mean_and_var_rasters <- function(iso_raster, si) {

  # note, with really large rasters, we may end up wanting to write them to disk, but
  # I really doubt it for most of our work...
  stopifnot("slopes" %in% names(si),  "intercepts" %in% names(si))

  slopes <- si$slopes
  intercepts <- si$intercepts

  # make a list of rasters of tissue isotopes predicted by the resampled regressions:
  rlist <- lapply(seq_along(slopes), function(i) {
    iso_raster * slopes[i] + intercepts[i]
  })

  # make a RasterStack
  rstack <- raster::stack(rlist)

  # return the mean and var of those:
  list(
    mean.raster = iso_raster * mean(slopes) + mean(intercepts),
    var.raster = raster::stackApply(rstack, fun = var, indices = 1)
  )
}



#' assign posterior probability or origin for a bird in each cell in the raster
#'
#' This is a rewrite of the function \code{assignment} from the Vander
#' Zanden appendix code.
#' @param rescale_mean the tissue specific mean raster, such as the mean.raster
#' component of the output of \code{vza_mean_and_var_rasters}.
#' @param rescale_var tissue specific raster of standard deviations, such as the var.raster
#' component of the output of \code{vza_mean_and_var_rasters}.
#' @param precip_sd SD raster associated with the IsoMAP output.
#' This is the precip component of the variance term.
#' @param sd_indiv the individual component of the variance term.
#' This is a value to be determined by the user.  The standard approach is to
#' use the mean of the SDs observed among individuals at all of the calibration sites.
#' @param birds_iso a single value giving the isotope ratio found in the individual's feather.
#' @details This is a fairly low-level function.  It returns a raster of posterior probs (they are
#' scaled to sum to one over all cells).
#' @export
vza_assign <- function(rescale_mean,
                       rescale_var,
                       precip_sd,
                       sd_indiv,
                       bird_iso
                       ) {

  # first we compute the tilde{T}'s.  call them Tmu and Tsig
  Tmu <- rescale_mean    # no extra conversion to be done here
  Tsig <- rescale_var + precip_sd^2 + sd_indiv^2

  # now copy Tmu to get a raster of the right dimensions to returns then set
  # its cell values as normal densities.
  ret <- Tmu
  values(ret) <- dnorm(x = bird_iso, mean = values(Tmu), sd = sqrt(values(Tsig)))
  ret / cellStats(ret, sum)
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





###### RASTER CONVERSION ######
#This function uses the output from the rescaling function to convert the precip rasters to tissue-specific mean raster and rescale SD raster (used in the pooled error)
#Function includes:
#original.raster = the filename (and directory, if applicable) of the original precip raster created in IsoMAP
#reg.par = the output from the function above
#scratch.dir = the directory of a scratch folder to store the rasters temporarily

raster.conversion <- function (original.raster, reg.par, scratch.dir) {

  for (i in 1:length(reg.par[,1])) {
    reg.par.i <- reg.par[i,]
    raster.i <- original.raster*reg.par.i$slopes + reg.par.i$intercepts
    name <- paste(scratch.dir, i, ".grd", sep="")
    writeRaster(raster.i, filename=name, overwrite=TRUE)
  }
  setwd(scratch.dir)
  all.files <- dir(pattern=".grd")
  n <- length(all.files)
  all.rasters <- stack(all.files)
  mean.raster <- original.raster*mean(reg.par$slopes) + mean(reg.par$intercepts)
  SD.raster <- stackApply(all.rasters, fun=sd, indices=c(rep(1,n)))
  return(list(mean.raster=mean.raster, SD.raster=SD.raster))
}


###### ASSIGNMENT FUNCTION ######
#This function uses the likelihood term (Equation S2 in Appendix 1) to determine the probability that an individual sample was from a particular geographic location and writes an ascii file to a chosen directory
#Because IsoMAP produces the files in the ascii format that is compatible with ArcGIS software, I have maintained that format here and throughout
#Function includes:
#rescaled_raster = the tissue-specific d2H raster created in the function above
#rescaleded_SD_raster = the SD raster created in the function above.  This the component of the error term related to the rescaling process.
#precip_SD_raster = this is the SD raster associated with the IsoMAP output.  This is the precip component of the variance term.
#SD_indv =the individual component of the variance term.  This is a value determined by the user.  We calculated the mean of the SDs observed among individuals at all of the calibration sites.
#assign_table = this a csv filename (and directory, if applicable) containing the tissue d2H values of the individuals for which the assignments will be made
#d2Htissue = column number in the assign_table with the d2H tissue values
#ID = column number with individual identifiers
#save_dir is where the output assignments should be saved as an ascii, but could be changed

assignment <- function(rescaled_raster, rescaled_SD_raster, precip_SD_raster, SD_indv, assign_table, d2Htissue, ID, save_dir){
  error <- sqrt((rescaled_SD_raster)^2 + (precip_SD_raster)^2 + (SD_indv)^2)
  data <- read.table(assign_table, sep=",", header=T)
  #   data <- data[1:5,] #temp
  n <- length(data[,d2Htissue])
  dir.create(save_dir, showWarnings = FALSE)

  for (i in 1:n){
    indv.data <- data[i,]
    indv.id <- indv.data[1, ID]
    assign <- (1/sqrt((2*pi*error^2)))*exp(-1*(indv.data[1,d2Htissue]-rescaled_raster)^2/(2*error^2))#oops, I realized that this formula was missing a square root function
    assign_norm <- assign/cellStats(assign, "sum") #normalize so all pixels sum to 1
    filename <- paste(save_dir, "/", indv.id, ".like", ".asc", sep="")
    writeRaster(assign_norm, file=filename, format="ascii", overwrite=TRUE)
  }
}



