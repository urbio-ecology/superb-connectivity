library(urbioconnect)
library(terra)
# Load habitat raster
wren_habitat <- example_wren_habitat()
# Load barrier raster
wren_barrier <- example_wren_barrier()
# load barrier scenario
# In this demonstration we should call this a "barrier scenario", but it needs
# to be clear to the users that they can change the
# 1) habitat, 2) the barrier

# "The change in this scenario is a change in the barrier layer, but it represents
# a change in both habitat and barrier"
# there are three possible scenario ideas
# habitat gets removed
# habitat gets added
# barriers get added
wren_barrier_scenario <- example_wren_barrier_scenario()
plot(
  wren_barrier,
  col = c("grey", "white"),
  legend = FALSE,
  main = "Wren Habitat and Barrier"
)
plot(wren_habitat, col = "darkgreen", legend = FALSE, add = TRUE)

plot(
  wren_barrier_scenario,
  col = c("grey", "white"),
  legend = FALSE,
  main = "Wren Habitat and Barrier scenario"
)
plot(wren_habitat, col = "darkgreen", legend = FALSE, add = TRUE)

terra::res(wren_habitat)
terra::res(wren_barrier)
terra::res(wren_barrier_scenario)

# this should be the output of "summarise_connectivity"
wren_connectivity_baseline <- habitat_connectivity(
  habitat = wren_habitat,
  barrier = wren_barrier,
  species = "Superb Fairy Wren",
  interpatch_distance = 200
)

# this should be the output of "summarise_connectivity"
# habitat_connectivity_comparison, which will take the original baseline
# information from (say) - wren_connectivity_baseline <- habitat_connectivity()
wren_connectivity_scenario <- habitat_connectivity(
  habitat = wren_habitat,
  barrier = wren_barrier_scenario,
  species = "Superb Fairy Wren",
  interpatch_distance = 200
)
# patch_size object should be able to be retrieved from
# habitat_connectivity
# And also options for the workflow to retrieve/attach the spatial data
# so they can do the patch_id plot

wren_connectivity_baseline
wren_connectivity_scenario

output_from_habitat_connectivity <- summarise_connectivity(
  connectivity = wren_connectivity_baseline
)

output_from_habitat_connectivity

patch_sizes(output_from_habitat_connectivity)

# summarise_connectivity_scenario
summarise_connectivity(
  connectivity = wren_connectivity_scenario,
  connectivity_baseline = wren_connectivity_baseline
)

# they are the same - that's the difference here
# TODO:
# bug - n_patches, effective_mesh_ha, and pr_connectedness are not the same
# in the different scenarios
compare_connectivity(
  connectivity = wren_connectivity_baseline,
  connectivity_baseline = wren_connectivity_scenario
)
