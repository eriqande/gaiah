

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
#'   \item{Field_Num}{unique identifier for each bird}
#'   \item{Isotope.Value}{hydrogen isotope ratios measured in the bird's feather}
#'   \item{lat}{latitude of the bird's breeding/sampling location}
#'   \item{long}{latitude of the bird's breeding/sampling location}
#' }
#' @source Kristen Ruegg, Jeff Kelly, Thomas Smith
"breeding_wiwa_isotopes"




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
