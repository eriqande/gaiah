
library(dplyr)
library(gaiah)


####  Whether or not to re-compute things
COMPUTE_ISO_POSTERIORS <- FALSE




####  Get the isotope posterior probs via Leave-one-out for the reference birds ####

ref_birds <- breeding_wiwa_isotopes

if(COMPUTE_ISO_POSTERIORS == TRUE) {
  isotope_ref_bird_results <- isotope_posterior_probs(isoscape = isomap_job54152_prediction,
                                                    ref_birds = ref_birds[,],
                                                    isoscape_pred_column = "predkrig",
                                                    isoscape_sd_column = "stdkrig",
                                                    self_assign = TRUE)

  #save(isotope_ref_bird_results, file = "outputs/isotope_ref_bird_results.rda", compress = "xz")
} else {
  load("outputs/isotope_ref_bird_results.rda")
}

# clip the isotope posteriors to the known breeding range and normalize again
# I should rewrite the function to do this if we have the breeding range...
isotope_ref_bird_results$loo_results <- lapply(isotope_ref_bird_results$loo_results, function(y) {
  x <- y$posterior_probs
  y$posterior_probs <- x * wiwa_breed / cellStats(x * wiwa_breed, sum)
  y
})



#### Get the genetic posteriors and then make the genetic posterior rasters
breeding_wiwa_genetic_posteriors


