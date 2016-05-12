

#### Functions for summarizing the accuracy of the assignments given known location ####

# make a raster that holds the distances between a point and the center of every cell
# this is straightforward and fast.  Here is a synopsis:
#  bunk <- xyFromCell(riso, 1:ncell(riso))
#  risoDist <- riso
#  values(risoDist) <- distCosine(c(-100, 50), bunk)
#
# That is pretty much all there is to it!
# I can plot those as a density like so, roughly:
# ggplot(whoa, aes(x = dist, y = ..density.., weight = posterior_prob)) + geom_density()

