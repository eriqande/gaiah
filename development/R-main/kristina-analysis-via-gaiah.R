
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
#system.time({
#dir.create("scratch/")
#old_rastcon <- gaiah:::raster.conversion(riso, as.data.frame(new_results), "scratch/")
#unlink("scratch", recursive = T)
#})
# user  system elapsed
# 38.081   4.808  40.520


# here is the new way (about 1/4 of the time)
system.time({
  new_rastcon <- gaiah::vza_mean_and_var_rasters(riso, new_results)
})

# check that we get the same results:
#all.equal(new_rastcon, old_rastcon)  # Yay!



#### Now, compute the actual posteriors ####
# we will compare old and new for 10 random birds here
set.seed(10)
randobirds <- sort(sample(1:nrow(feather.dat), 10))
testy_birds <- feather.dat[randobirds,]
testy_birds$Field_Num <- make.names(testy_birds$Field_Num)
write.csv(testy_birds, file = "development/outputs/testy_birds.csv", row.names = FALSE)

# here is the old code way to do it:
dir.create("development/outputs/assignedWIWA/") #make a directory for saving assignment probabilty surfaces
gaiah:::assignment(rescaled_raster = new_rastcon$mean.raster,
           rescaled_SD_raster = sqrt(new_rastcon$var.raster), # 0.75 to 2.59
           precip_SD_raster = sdiso, # 8.53 to 21.64
           SD_indv=mean(fcalnew.dat$sdH), #12.497
           assign_table = "development/outputs/testy_birds.csv",
           d2Htissue = 2,
           ID = 1,
           save_dir="development/outputs/assignedWIWA/")

# here is our new function that we will use on the
mi_pajaros <- testy_birds$Isotope.Value
names(mi_pajaros) <- testy_birds$Field_Num

new_probs <- lapply(mi_pajaros, function(x) {
  gaiah::vza_assign(rescale_mean = new_rastcon$mean.raster,
                    rescale_var = new_rastcon$var.raster,
                    precip_sd = sdiso,
                    sd_indiv = mean(fcalnew.dat$sdH),
                    bird_iso = x)
})

pdf(file = "development/outputs/compare-assign.pdf", width = 15, height = 40)
par(mfrow = c(10, 3))
old_new_compare <- lapply(names(new_probs), function(x) {
  oldy <- raster(paste("development/outputs/assignedWIWA/", x, ".like.asc", sep = ""))
  plot(new_probs[[x]], main = paste(x, "New"))
  plot(oldy, main = paste(x, "Old"))
  hist(values(oldy) - values(new_probs[[x]]), breaks = 30)

  # return whether or not they are equal
  all.equal(oldy, new_probs[[x]])
})
dev.off()

# show that the old calcs and the new faster ones give the same results
old_new_compare

# Yay!

