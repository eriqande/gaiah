
library(gaiah)


# this executes what Kristina does in the her script
# "Kristina WIWA isotopes.R" but it does it using the functions
# developed here in the gaiah package.  This is here now
# to remind me how all of these functions go together.  Later
# we will probably make a vignette out of it.

# "breeding_wiwa_isotopes" are internal data in the package for examples.
feather.dat <- breeding_wiwa_isotopes


# get the base map
wrld_simpl <- gaiah::get_wrld_simpl()


# get the isotopes (they are internal data too...)
iso.dat <- isomap_job54152_prediction


# get the rasters of the predictions and the SDs
riso <- gaiah::isomap2raster(isomap_job54152_prediction, "predkrig")
sdiso <- gaiah::isomap2raster(isomap_job54152_prediction, "stdkrig")


# Add the isomap prediction and sd to the feather data
# so, we have actually rolled up getting the rasters into this
# function:
feather.dat <- gaiah::extract_isopredictions(isoscape = isomap_job54152_prediction,
                                      birds = breeding_wiwa_isotopes,
                                      pred = "predkrig",
                                      sd = "stdkrig")


# Change name of location to match other locations with same lat and long - so not singleton
feather.dat$Location[feather.dat$Location=="Charlevoix"] <- "Camp Myrica"


# at this point she does a regression to visualize the relationship, but the results
# don't really get used later on.  I will come back and implement a simple function
# to look at that, or at least the one to plot it.


# now we have to group by location.  They used ddply, we will use dplyr and
# wrap it up in a function that will ensure all the columns are named appropriately.
fcalnew.dat <- gaiah::group_birds_by_location(D = feather.dat, feather_isotope_col = "Isotope.Value", location_col = "Location")


# here we run the rescale function as it was implemented by vander zanden and colleagues
gaiah:::rescale(as.data.frame(fcalnew.dat), 1, 2, 3, 4, 5, 6)
