
library(raster)  # call this before dplyr so it doesn't mask select
library(dplyr)
library(stringr)
library(ggplot2)
library(gaiah)



####  Whether or not to re-compute things
COMPUTE_ISO_POSTERIORS <- FALSE




####  Get the isotope posterior probs via Leave-one-out for the reference birds ####

ref_birds <- breeding_wiwa_isotopes

if(COMPUTE_ISO_POSTERIORS == TRUE) {
  isotope_ref_bird_results <- isotope_posterior_probs(isoscape = isomap_job54152_prediction,
                                                    ref_birds = ref_birds,
                                                    isoscape_pred_column = "predkrig",
                                                    isoscape_sd_column = "stdkrig",
                                                    self_assign = TRUE)

  #save(isotope_ref_bird_results, file = "outputs/isotope_ref_bird_results.rda", compress = "xz")
} else {
  load("outputs/isotope_ref_bird_results.rda")
}

# clip the isotope posteriors to the known breeding range and normalize again
# I should rewrite the function to do this if we have the breeding range...maybe...
# also, at this point I am going to get a simple list of rasters
Miso <- lapply(isotope_ref_bird_results$loo_results, function(y) {
  x <- y$posterior_probs
  x * wiwa_breed / raster::cellStats(x * wiwa_breed, sum)
})




#### Get the genetic posteriors and then make the genetic posterior rasters  ####
# breeding_wiwa_genetic_posteriors  # here is a long format data frame of assignment posteriors to region

## A function to turn gpdf into a list of rasters ##
# G long format data frame like breeding_wiwa_genetic_posteriors.  Has to have columns of ID, region, and posterior
# R: a RasterStack like "genetic regions".  The sum of these should be the total known range.
# The names of the regions in R must be the same as in G.
genetic_posteriors2rasters <- function(G, R) {

  # get the number of non-zero cells in each of the genetic regions
  Ncell <- cellStats(R, sum)

  # split the genetic data frame into a list by individual
  gList <- base::split(G, G$ID)

  lapply(gList, function(df) {
    # make a named vector of posteriors to region
    gp <- df$posterior
    names(gp) <- as.character(df$region)
    gp <- gp[names(R)]  # make sure they are in the correct order (FUTURE: error check the names)

    # this smears posterior probability into each region so that the sum over cells
    # in each region is the posterior probability of being from that region, then the calc
    # function sums over all the layers.  Way to go Robert Hijman's lovely, compact sytax
    raster::calc(R * gp / Ncell, fun = sum)
  })

}

Mgen <- genetic_posteriors2rasters(breeding_wiwa_genetic_posteriors, genetic_regions)  # this takes about 25 seconds





#### Load up Mhab  ####
Mhab <- wiwa_habitat_unclipped * wiwa_breed  # make sure to clip it with the known breeding range.

Mhab_norm <- Mhab / raster::cellStats(Mhab, stat = sum)  # and for later analyses, treat these as posterior probs



#### Now, some bookkeeping.  ####
# I want to retain only the birds that appear both in Mgen and Miso, and I
# want to order them so that they are all together from different locations
keepers <- intersect(names(Miso), names(Mgen))

# here we input the genetic regions for those short pops.  I get these from the repunits
# file from the wiwa-popgen repo.  Ultimately I will want to have these with the data from the get-go
spr <- read.table(textConnection("ShortPop  region
                                 wAKDE AK.EastBC.AB
                                 wAKYA AK.EastBC.AB
                                 wAKUG AK.EastBC.AB
                                 wAKJU AK.EastBC.AB
                                 wABCA AK.EastBC.AB
                                 wBCMH AK.EastBC.AB
                                 wWADA Wa.To.NorCalCoast
                                 wORHA Wa.To.NorCalCoast
                                 wORMB Wa.To.NorCalCoast
                                 wCAEU Wa.To.NorCalCoast
                                 wCAHM CentCalCoast
                                 wCABS CentCalCoast
                                 wCASL CentCalCoast
                                 wCATE CalSierra
                                 wCACL CalSierra
                                 wCAHU CalSierra
                                 wMTHM Basin.Rockies
                                 wOREL Basin.Rockies
                                 wCOPP Basin.Rockies
                                 wCOGM Basin.Rockies
                                 eQCCM Eastern
                                 eONHI Eastern
                                 eNBFR Eastern
                                 "), header = TRUE, stringsAsFactors = FALSE)




kbirds <- ref_birds %>%
  filter(ID %in% keepers) %>%
  mutate(ShortPop = str_replace_all(Short_Name, "[0-9]", "")) %>%
  left_join(spr) %>%
  arrange(region, ShortPop, Short_Name) %>%
  dplyr::select(ID, region, ShortPop, Short_Name, lat, long, everything())




# so, kbirds is our master data frame of the birds that we will be using here


# Now, I think that it is just going to be better to use the Short_Names for all of these
# (at least it will be easier to think about and check everything).  So do the following:
Miso <- Miso[kbirds$ID]
names(Miso) <- kbirds$Short_Name

Mgen <- Mgen[kbirds$ID]
names(Mgen) <- kbirds$Short_Name

# and, in fact, it will be nice to make these RasterStacks
Miso <- raster::stack(Miso)
Mgen <- raster::stack(Mgen)




#### Here is a simple plotting tool and I will quickly make a bunch of plots ####
# bird is the Short_Name of the bird
quickie_plots <- function(bird, Miso, Mgen, Mhab, kbirds) {
  lat <- kbirds$lat[kbirds$Short_Name == bird]
  long <- kbirds$long[kbirds$Short_Name == bird]
  reg <- kbirds$region[kbirds$Short_Name == bird]

  plot(Mgen[[bird]],  main = paste("Genetics:", bird, reg))
  points(long, lat, pch = 18, col = 'red', cex = 1.5)

  plot(Miso[[bird]],  main = paste("Isotopes:", bird, reg))
  points(long, lat, pch = 18, col = 'red', cex = 1.5)

  plot(Mhab,  main = paste("Habitat:", bird, reg))
  points(long, lat, pch = 18, col = 'red', cex = 1.5)
}

bird_name_mat <- matrix(kbirds$Short_Name, nrow = 36)
for(i in 1:ncol(bird_name_mat)) {
  pdf(file = paste("outputs/known_origin_raster_", i, ".pdf", sep = ""), height = 162, width = 11)
  par(mfrow = c(36,3))
  lapply(bird_name_mat[, i], function(x) quickie_plots(x, Miso, Mgen, Mhab, kbirds))
  dev.off()
  message(paste("Done with", i))
}





#### Now go ahead and compute rasters of great circle distance for each bird in kbirds ####


GCD <- lapply(1:nrow(kbirds), function(i)
    great_circle_raster(wiwa_breed, kbirds$lat[i], kbirds$long[i])
  )
names(GCD) <- kbirds$Short_Name

GCD <- raster::stack(GCD)



#### And now we can easily compute the posterior mean great circle distance  ####

genpmgc <- raster::cellStats(Mgen * GCD, stat = sum)
isopmgc <- raster::cellStats(Miso * GCD, stat = sum)
habpmgc <- cellStats(GCD * Mhab_norm, stat = sum)
names(habpmgc) <- names(GCD)

# and just quickly, let us add that to our kbirds data frame
dist_df <- kbirds
dist_df$gc_iso <- isopmgc[dist_df$Short_Name]
dist_df$gc_gen <- genpmgc[dist_df$Short_Name]
dist_df$gc_hab <- habpmgc[dist_df$Short_Name]

# Now we are in a great place to ggplot those as histograms or densities
dist_tidy <- dist_df %>%
  tidyr::gather(., key = "data_type", value = "post_mean_gc_dist", gc_iso:gc_hab)

ggplot(dist_tidy %>% filter(data_type != "gc_hab"), aes(x = post_mean_gc_dist, fill = data_type)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ region)
ggsave(filename = "outputs/pm_great_circ_dens_gen_iso.pdf", width = 10, height = 10)


# this is ugly.
ggplot(dist_tidy, aes(x = post_mean_gc_dist, fill = data_type)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ region, scales = "free_y")


# we can add some noise to the gc_hab so that it is not so spiky
set.seed(5)
dist_df_nonspiky <- dist_df %>%
  group_by(region) %>%
  mutate(gc_hab_wig = gc_hab + rnorm(n(), mean = 0, sd = 0.05 * mean(gc_hab)))

dist_tidy_nspike <- dist_df_nonspiky %>%
  tidyr::gather(., key = "data_type", value = "post_mean_gc_dist", gc_iso, gc_gen, gc_hab_wig)

ggplot(dist_tidy_nspike, aes(x = post_mean_gc_dist, fill = data_type)) +
  geom_density(alpha = 0.34) +
  facet_wrap(~ region, scales = "free_y")
ggsave(filename = "outputs/pm_great_circ_dens.pdf", width = 10, height = 10)


# finally see how it looks as a scatter
ggplot(dist_df, aes(x = gc_gen, y = gc_iso, color = region)) +
  geom_point() +
  facet_wrap(~ region, ncol = 3) +
  geom_abline(slope = 1, intercept = 0)

# ultimately, I think that violin plots might make the most sense
ggplot(dist_tidy, aes(y = post_mean_gc_dist, x =  data_type, fill = data_type)) +
  geom_violin() +
  facet_wrap(~ region) +
  coord_flip()



#### Combining iso, hab, and genetics.
# I think this is going to be easiest if we have everything be a raster Stack, so make a rasterStack of Mhab
Mhab_stack <- raster::stack(lapply(1:nlayers(Miso), function(x) Mhab))

# holy crap! this is easy and quick
tmp  <- Miso * Mhab_stack * Mgen
Mcombo <- tmp / cellStats(tmp, sum)
