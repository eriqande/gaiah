
library(dplyr)
library(gaiah)


####  Whether or not to re-compute things
COMPUTE_ISO_POSTERIORS <- FALSE

####  Get the isotope posterior probs via Leave-one-out for the reference birds ####

ref_birds <- breeding_wiwa_isotopes %>%
  rename(ID = Field_Num)


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
