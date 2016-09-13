

#' Isotope values, latitude, longitude and more data from 357 breeding Wilson's warblers
#'
#' A data frame containing hydrogen isotope values, lat, long, and IDs and some other
#' columns of data for birds sampled on the breeding grounds. Notice that the latitude
#' column is named "lat" and the longitude column is named "long".  Those names are both,
#' all lowercase.  That is the way we roll here.  Make sure that you use "lat" and "long" instead
#' of "Lat" and "Long".
#'
#' @format A tbl_df-ed (from dplyr) data frame with 357 rows and 14 variables. The relevant variables for
#' analyses here are:
#' \describe{
#'   \item{ID}{unique identifier for each bird}
#'   \item{Isotope.Value}{hydrogen isotope ratios measured in the bird's feather}
#'   \item{lat}{latitude of the bird's breeding/sampling location}
#'   \item{long}{latitude of the bird's breeding/sampling location}
#' }
#' @source Kristen Ruegg, Jeff Kelly, Thomas Smith
"breeding_wiwa_isotopes"



#' Isotope values, latitude, longitude and more data from 357 breeding Wilson's warblers
#'
#' A data frame containing hydrogen isotope values, lat, long, and IDs and some other
#' columns of data for birds sampled during migration from Arizona.
#' @format A tbl_df-ed (from dplyr) data frame with 688 rows and 14 variables. The relevant variables for
#' analyses here are:
#' \describe{
#'   \item{ID}{unique identifier for each bird}
#'   \item{Isotope.Value}{hydrogen isotope ratios measured in the bird's feather}
#' }
#' @source Kristina Paxton
"migrant_wiwa_isotopes"






#' Posterior probs of genetic region origin from Leave-one-out cross validation for breeding WIWAs
#'
#' A data frame of the same birds (roughly) that appear in \code{\link{breeding_wiwa_isotopes}}.  A long
#' format data frame with 2,358 rows and 5 columns
#'
#' @format A tbl_df-ed (from dplyr) data frame with 357 rows and 14 variables. The relevant variables for
#' analyses here are:
#' \describe{
#'   \item{ID}{unique identifier for each bird}
#'   \item{Short_Name}{another id for the bird}
#'   \item{Number of loci}{Number of loci successfully typed}
#'   \item{region}{one of the genetic regions}
#'   \item{posterior}{the posterior prob of originating from that region}
#' }
#' @source Kristen Ruegg, Eric Anderson, Thomas Smith
"breeding_wiwa_genetic_posteriors"




#' Posterior probs of genetic region of origin for 926 WIWAs sampled during migration
#'
#'  A long
#' format data frame with 5,556 rows and 6 columns
#'
#' @format A tbl_df-ed (from dplyr) data frame with 357 rows and 14 variables. The relevant variables for
#' analyses here are:
#' \describe{
#'   \item{ID}{unique identifier for each bird}
#'   \item{Short_Name}{same id for the bird}
#'   \item{Collection_Date} The date the bird was sampled.
#'   \item{NumberOfLoci}{Number of loci successfully typed}
#'   \item{region}{one of the genetic regions}
#'   \item{posterior}{the posterior prob of originating from that region}
#' }
#' @source Kristina Paxton, Kristen Ruegg, Eric Anderson, Thomas Smith
"migrant_wiwa_genetic_posteriors"





#' Predicted isotope values from ISOMAP
#'
#' A data frame containing predicted hydrogen isotope values, lat, long, and IDs and some other
#' columns of data prections made by ISOMAP
#'
#' @format A tbl_df-ed (from dplyr) data frame with 10,786 rows and 12 variables. The relevant variables for
#' analyses here are:
#' \describe{
#'   \item{lat}{latitude of the predicted location}
#'   \item{long}{longitude of the predicted location}
#'   \item{predreg}{Fill in}
#'   \item{stdreg}{Fill in}
#'   \item{predkrig}{Fill in}
#'   \item{stdkrig}{Fill in}
#' }
#' @source Kristina Paxton and ISOMAP (PROVIDE URL)
"isomap_job54152_prediction"





#' return the wrld_simpl data set from maptools
#'
#' I define this as a function so that we don't have to attach
#' maptools, but we can just have it in the imports. Couldn't figure
#' out how to do it otherwise.
#' @export
#' @examples
#' ws <- get_wrld_simpl()
#' head(ws)
#' \dontrun{plot(ws)}
get_wrld_simpl <- function() {
  load(system.file("data/wrld_simpl.rda", package = "maptools"))
  wrld_simpl
}


#' a raster of the breeding range of Wilson's warbler
#'
#' @format This a rasterized version of the breeding range of Wilson's warbler
#' It contains 1's in the breeding range and 0's elsewhere.
#' \describe{
#' \item{class}{RasterLayer}
#' \item{dimensions}{80, 228, 18240  (nrow, ncol, ncell)}
#' \item{resolution}{0.5, 0.5  (x, y)}
#' \item{extent}{-168.1, -54.1, 31.2, 71.2  (xmin, xmax, ymin, ymax)}
#' \item{coord. ref.}{+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0}
#' \item{data source}{in memory}
#' \item{names}{layer}
#' \item{values}{0, 1  (min, max)}
#' }
#'
#' @source The rasters were generated from shapefiles provided to us by
#' BirdLife International. (BirdLife International and NatureServe (2012)
#' Bird species distribution maps of the world. BirdLife International, Cambridge,
#' UK and NatureServe, Arlington, USA). Persons interested in the range map
#' should contact BirdLife International http://www.birdlife.org/ or
#' NatureServe http://www.natureserve.org/ directly.
"wiwa_breed"




#' RasterStack showing the 6 genetic regions that Wilson's warblers may be assigned to
#'
#' The sum over layers gives the same as \code{\link{wiwa_breed}}
#' @format RasterStack with 6 layers. Each contains 1's in the genetic region and 0's elsewhere.
#' The sum of these layers is the raster \code{\link{wiwa_breed}}.
#' \describe{
#' \item{class}{ RasterStack }
#' \item{dimensions}{ 80, 228, 18240, 6  (nrow, ncol, ncell, nlayers)}
#' \item{resolution}{ 0.5, 0.5  (x, y)}
#' \item{extent}{ -168.1, -54.1, 31.2, 71.2  (xmin, xmax, ymin, ymax)}
#' \item{coord. ref.}{ +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 }
#' \item{data source}{in memory}
#' \item{names}{ CalSierra, Basin.Rockies, Eastern, AK.EastBC.AB, Wa.To.NorCalCoast, CentCalCoast }
#' }
#'
#' @source Ruegg et al 2014
"genetic_regions"


#' RasterLayer showing the MaxEnt habitat suitability model unclipped by the known breeding range
#'
#' \describe{
#' \item{class}{ RasterLayer }
#' \item{dimensions}{ 80, 228, 18240, 6  (nrow, ncol, ncell, nlayers)}
#' \item{resolution}{ 0.5, 0.5  (x, y)}
#' \item{extent}{ -168.1, -54.1, 31.2, 71.2  (xmin, xmax, ymin, ymax)}
#' \item{coord. ref.}{ +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 }
#' \item{data source}{in memory}
#' \item{values}{0, 0.001093349  (min, max)}
#' }
#'
#' @source Ryan Harrigan
"wiwa_habitat_unclipped"
