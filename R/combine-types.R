
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




#' prepare fortified output for multipanel plot
#'
#' This takes Mgen, Miso, and Mhab for a single bird
#' and, if available, the true breeding location.  Then it
#' computes the combo-ized raster at all the requested levels
#' of the exponents, and creates a fortified data frame of the
#' results suitable for plotting in ggplot
#' @param Mgen  genetics posterior raster
#' @param Miso isotope posterior raster
#' @param Mhab habitat suitability raster
#' @param true_lat  the true latitude where the bird was sampled.
#' @param true_long the true longitude where the bird was sampled.
#' @param gen_beta_levels
#' @param iso_beta_levels
#' @param hab_beta_levels
#' @export
comboize_and_fortify <- function(mgen, miso, mhab,
                                 true_lat = NA, true_long = NA,
                                 gen_beta_levels = 1,
                                 iso_beta_levels = c(1.0),
                                 hab_beta_levels = c(1.0)
) {
  names(gen_beta_levels) <- gen_beta_levels
  names(iso_beta_levels) <- iso_beta_levels
  names(hab_beta_levels) <- hab_beta_levels

  # get all the different levels there
  levs_ret <- lapply(gen_beta_levels, function(gbl) {
    lapply(iso_beta_levels, function(ibl) {
      lapply(hab_beta_levels, function(hbl) {
        comboize(raster::stack(mgen), raster::stack(miso), mhab, gbl, ibl, hbl) %>%
        raster::as.data.frame(., xy = TRUE, stringsAsFactors = FALSE) %>%
          setNames(c("long", "lat", "prob"))  %>%
          dplyr::tbl_df()
      }) %>% dplyr::bind_rows(.id = "habitat_beta")
    }) %>% dplyr::bind_rows(.id = "isotope_beta")
  }) %>% dplyr::bind_rows(.id = "genetics_beta")


  # add a column that names these betas:
  levs_ret2 <- levs_ret %>%
    mutate(beta_vals = paste("G=", genetics_beta, ", I=", isotope_beta, ", H=", habitat_beta, sep = ""))

  beta_levs <- unique(levs_ret2$beta_vals)

  # now, create three more that are just genetics, habitat, and density alone
  solos <- lapply(list(`Genetics Alone` = comboize(raster::stack(mgen), raster::stack(miso), mhab, 1, 0, 0),
       `Isotopes Alone` = comboize(raster::stack(mgen), raster::stack(miso), mhab, 0, 1, 0),
       `Habitat Alone` = comboize(raster::stack(mgen), raster::stack(miso), mhab, 0, 0, 1)), function(x)
         {
         raster::as.data.frame(x, xy = TRUE, stringsAsFactors = FALSE) %>%
           setNames(c("long", "lat", "prob"))  %>%
           dplyr::tbl_df()
       })  %>%
    dplyr::bind_rows(.id = "beta_vals")

  ret <- bind_rows(solos, levs_ret2)
  ret$beta_vals <- factor(ret$beta_vals, levels = c("Habitat Alone", "Genetics Alone", "Isotopes Alone", beta_levs))
  ret %>% select(beta_vals, long, lat, prob)
}




