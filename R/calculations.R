# the connectivity calculation
calc_connectivity <- function(grouped_areas) {
  effective_mesh <- sum(grouped_areas$area_squared) /
    sum(grouped_areas$area_total)
  effective_mesh_hectares <- effective_mesh * 0.0001
  effective_mesh_hectares
}

# calculate mean size of connected areas
calc_mean_size <- function(grouped_areas) {
  mean_size <- mean(grouped_areas$area_total)
  mean_size
}

# find number of connected areas
calc_num_areas <- function(grouped_areas) {
  n_areas <- length(grouped_areas$area_total)
  n_areas
}

calc_total <- function(grouped_areas) {
  total <- sum(grouped_areas$area_total)
  total_hectares <- total * 0.0001
  total_hectares
}

# find probability of connectedness
calc_prob_connect <- function(grouped_areas) {
  total_habitat <- sum(grouped_areas$area_total)
  connect_value <- calc_connectivity(grouped_areas)
  prob_connect <- connect_value / total_habitat
  prob_connect
}
