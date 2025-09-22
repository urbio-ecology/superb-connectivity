# identify which of the remaining original habitat patches belong in which connected area
identify_patches <- function(remaining, fragID) {
  inter <- sf::st_intersects(remaining, fragID)
  # code to sanitise "inter"  (a list of vectors) to make sure any empty vectors are non-empty
  # and make sure there are no vectors longer that 1
  cleaned_inter <- lapply(inter, clean_inter)
  membership <- unlist(cleaned_inter)
  habID <- sf::st_sf(geometry = remaining, cluster = membership)
  habID
}
