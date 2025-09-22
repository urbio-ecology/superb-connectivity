# calculate area of each habitat patch
patch_area <- function(patches) {
  patches$area <- sf::st_area(patches)
  areas <- data.frame(cbind(patches$cluster, patches$area))
  # rename columns
  names(areas)[1] <- "patchID"
  names(areas)[2] <- "area"
  areas
}
