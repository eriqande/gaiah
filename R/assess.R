

#### Functions for summarizing the accuracy of the assignments given known location ####

# make a raster that holds the distances between a point and the center of every cell
# this is straightforward and fast.  Here is a synopsis:
#  bunk <- xyFromCell(riso, 1:ncell(riso))
#  risoDist <- riso
#  values(risoDist) <- distCosine(c(-100, 50), bunk)
#
# That is pretty much all there is to it!
# I can plot those as a density like so, roughly:
# ggplot(whoa, aes(x = dist, y = ..density.., weight = posterior_prob)) + geom_density()



#' return a raster of great circle distances (in km)
#'
#' Given an input raster R, this returns a raster of the same dimension where
#' every cell is the great circle distance between lat, and long, and the
#' center of every cell in R.
#' @param R a raster
#' @param lat a latitude value (must be of length 1)
#' @param long a longitude value (must be of length 1)
#' @export
great_circle_raster <- function(R, lat, long) {
  stopifnot(length(lat) == 1, length(long) == 1)

  ret <- R  # just initialize this way to get the right resolution, etc
  values(ret) <- geosphere::distCosine(c(long, lat), xyFromCell(R, 1:ncell(R)))
  ret * 0.001
}

