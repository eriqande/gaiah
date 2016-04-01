

#' Isotope values, latitude, longitude and more data from 357 breeding Wilson's warblers
#'
#' A data frame containing hydrogen isotope values, lat, long, and IDs and some other
#' columns of data for birds sampled on the breeding grounds.
#'
#' @format A data frame with 357 rows and 14 variables. The relevant variables for
#' analyses here are:
#' \describe{
#'   \item{Field_Num}{unique identifier for each bird}
#'   \item{Isotope.Value}{hydrogen isotope ratios measured in the bird's feather}
#'   \item{Lat}{latitude of the bird's breeding/sampling location}
#'   \item{Long}{latitude of the bird's breeding/sampling location}
#' }
#' @source Kristen Ruegg, Jeff Kelly, Thomas Smith
"breeding_wiwa_isotopes"




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
