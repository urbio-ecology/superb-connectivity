# TODO consider slightly cleaner names - we don't necessarily need
# a calc_ prefix.
# potentially: connectivity()
calc_connectivity <- function(area_squared, area_total) {
  effective_mesh <- sum(area_squared) / sum(area_total)
  effective_mesh_hectares <- effective_mesh * 0.0001
  effective_mesh_hectares
}

# calculate mean size of connected areas
# potentially: area_connected_mean
calc_mean_size <- function(area_total) {
  mean_size <- mean(area_total)
  mean_size
}

# find number of connected areas
# potentially: area_connected_n
calc_num_areas <- function(area_total) {
  n_areas <- length(area_total)
  n_areas
}

# area_connected_total
calc_total <- function(area_total) {
  total <- sum(area_total)
  total_hectares <- total * 0.0001
  total_hectares
}

# probability_connectedness
# find probability of connectedness
calc_prob_connect <- function(area_squared, area_total) {
  total_habitat <- sum(area_total)
  connect_value <- calc_connectivity(area_squared, area_total)
  prob_connect <- connect_value / total_habitat
  prob_connect
}

# this could also be an S3 method for connectiveness?
summarise_connectivity <- function(area_squared, area_total) {
  results <- tibble(
    n_patches = calc_num_areas(area_total),
    prob_connectedness = calc_prob_connect(area_squared, area_total),
    effective_mesh_hectares = calc_connectivity(area_squared, area_total),
    patch_area_mean = calc_mean_size(area_total),
    patch_area_total = calc_total(area_total)
  )
  results
}
