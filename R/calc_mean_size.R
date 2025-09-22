# calculate mean size of connected areas
calc_mean_size <- function(groupedAreas) {
  mean_size <- mean(groupedAreas$total_area)
  # return
  mean_size
}
