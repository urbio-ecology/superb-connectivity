# the connectivity calculation
calc_connectivity <- function(groupedAreas) {
  effMesh <- sum(groupedAreas$areaSquared) / sum(groupedAreas$total_area)
  # convert to hectares
  effMesh <- effMesh * 0.0001
  # return
  effMesh
}

# calculate mean size of connected areas
calc_mean_size <- function(groupedAreas) {
  mean_size <- mean(groupedAreas$total_area)
  # return
  mean_size
}

# find number of connected areas
calc_num_areas <- function(groupedAreas) {
  numAreas <- length(groupedAreas$total_area)
  # return
  numAreas
}
