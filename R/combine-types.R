
# Functions for combining results from isotopes, genetics, and habitat



#' combine genetics, isotopes, and habitat raster with exponents as given
#'
#' This just multiplies the rasters together, each raised to the appropriate
#' exponent, normalizes and returns the result
#' @param Mgen  the genetic posteriors rasterStack.  Must be a rasterStack
#' @param Miso the isotope posteriors rasterStack.
#' @param Mhab a single layer raster with the habitat suitabiilty measure as a normalized
#' probability surface.
#' @param beta_gen the exponent to raise the genetic raster to
#' @param beta_iso the exponent to raise the isotope raster to
#' @param beta_gen the exponent to raise the habitat raster to
#' @export
comboize <- function(Mgen, Miso, Mhab, beta_gen, beta_iso, beta_hab) {
  stopifnot(class(Mgen) == "RasterStack")
  stopifnot(class(Miso) == "RasterStack")
  stopifnot(class(Mhab) == "RasterLayer")

  # make a rasterStack of Mhab that is the right length
  Mhab_stack <- raster::stack(lapply(1:nlayers(Miso), function(x) Mhab))

  tmp  <- (Mgen ^ beta_gen) * (Miso ^ beta_iso) * (Mhab_stack ^ beta_hab)

  # then normalize and return each one
  tmp / cellStats(tmp, sum)

}
