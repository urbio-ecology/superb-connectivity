effective_mesh_size <- function(area_squared, area_total) {
  effective_mesh <- sum(area_squared) / sum(area_total)
  effective_mesh_hectares <- effective_mesh * 0.0001
  effective_mesh_hectares
}

# calculate mean size of connected areas
# potentially: area_connected_mean
mean_patch_size <- function(area_total) {
  mean_size <- mean(area_total)
  mean_size
}

# find number of connected areas
# potentially: area_connected_n
n_patches <- function(area_total) {
  n_areas <- length(area_total)
  n_areas
}

# area_connected_total
total_habitat_area <- function(area_total) {
  total <- sum(area_total)
  total_hectares <- total * 0.0001
  total_hectares
}

# probability_connectedness
# find probability of connectedness
connectivity_probability <- function(area_squared, area_total) {
  total_habitat <- sum(area_total)
  connect_value <- effective_mesh_size(area_squared, area_total)
  prob_connect <- connect_value / total_habitat
  prob_connect
}

# this could also be an S3 method for connectiveness?
summarise_connectivity <- function(
  area_squared,
  area_total,
  buffer_distance,
  overlay_resolution,
  base_resolution,
  aggregation_factor,
  species_name
) {
  results <- tibble(
    species_name = species_name,
    buffer_distance = buffer_distance,
    n_patches = n_patches(area_total),
    prob_connectedness = connectivity_probability(area_squared, area_total),
    effective_mesh_ha = effective_mesh_size(area_squared, area_total),
    patch_area_mean = mean_patch_size(area_total),
    patch_area_total_ha = total_habitat_area(area_total),
    overlay_resolution = overlay_resolution,
    base_resolution = base_resolution,
    aggregation_factor = aggregation_factor
  ) |>
    mutate(
      prob_connectedness = round(prob_connectedness, 6)
    ) |>
    mutate(
      across(
        .cols = c(effective_mesh_ha, patch_area_mean, patch_area_total_ha),
        round
      )
    )
  results
}
