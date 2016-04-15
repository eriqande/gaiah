
library(gaiah)
library(dplyr)
library(ggplot2)

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


#### Rescale Function Stuff ####
# here we run the rescale function as it was implemented by vander zanden and colleagues
# commented out because we don't want to run it again.  It takes forever: over 11 minutes!
# user  system elapsed
# 624.461   9.178 665.952
# system.time(old_rescale_output <- gaiah:::rescale(as.data.frame(fcalnew.dat), 1, 2, 3, 4, 5, 6))
# saveRDS(old_rescale_output, file = "development/outputs/old_rescale_output.rds")

# now we just get that saved result.
old_rescale_results <- readRDS("development/outputs/old_rescale_output.rds")

# now, for comparison, run the new, efficient version.  2.5 seconds.  So, it is only 266 times faster.
system.time(new_results <- vza_rescale(fcalnew.dat))


# lets compare the outputs by plotting their density estimates
oldtidy <- setNames(old_rescale_results, c("slope", "intercept")) %>%
  tidyr::gather(param, value, slope, intercept) %>%
  tbl_df
newtidy <- tidyr::gather(new_results, param, value, slopes, intercepts)

DF <- bind_rows(list(Original = oldtidy, Eric = newtidy), .id = "Method")

ggplot(DF, aes(x = value, fill = Method)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~ param, scales = "free")

# ggsave(file = "development/outputs/simulated-slope-intercept-compare.pdf")

# check the distribution of mean slopes
# fifty_trials <- sapply(1:50, function(x) mean(vza_rescale(fcalnew.dat)$slope))

#### Raster Conversion Function ####

# here is the old way it was done
system.time({
dir.create("scratch/")
old_rastcon <- gaiah:::raster.conversion(riso, as.data.frame(new_results), "scratch/")
unlink("scratch", recursive = T)
})
# user  system elapsed
# 38.081   4.808  40.520


# here is the new way (about 1/4 of the time)
system.time({
  new_rastcon <- gaiah::vza_mean_and_sd_rasters(riso, new_results)
})

# check that we get the same results:
all.equal(new_rastcon, old_rastcon)  # Yay!


