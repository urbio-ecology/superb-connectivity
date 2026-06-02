# Plot connected habitat patches

Visualizes habitat patches colored by their connected fragment ID.

## Usage

``` r
plot_patches(patch_id, distance, species = "Species", n_cols = 7)
```

## Arguments

- patch_id:

  Terra SpatRaster. Raster with patch IDs.

- distance:

  Numeric. Buffer distance used (for subtitle).

- species:

  Character. Species name (default: "Species").

- n_cols:

  Integer. Number of colors to cycle through (default: 7).

## Value

A ggplot2 object showing patches with distinct colors.

## Examples

``` r
lizard_habitat <- example_habitat()
lizard_barrier <- example_barrier()
buffer_dist <- 5
buffered_habitat <- habitat_buffer(lizard_habitat, buffer_dist)
barrier_mask <- create_barrier_mask(lizard_barrier)
fragmented <- fragment_habitat(buffered_habitat, barrier_mask)
remaining_habitat <- drop_habitat_under_barrier(
  habitat = lizard_habitat,
  barrier = lizard_barrier
  )
fragment_patches <- assign_patches_to_fragments(
  remaining_habitat = remaining_habitat,
  fragment = fragmented
  ) |> add_patch_area()

plot_patches(fragment_patches, distance = buffer_dist)
#> <SpatRaster> resampled to 500554 cells.


#' add north arrow and scale bar with ggspatial
library(ggspatial)
library(tidyterra)
plot_patches(fragment_patches, distance = buffer_dist) +
 annotation_north_arrow(
   style = north_arrow_fancy_orienteering()
  ) +
  annotation_scale()
#> <SpatRaster> resampled to 500554 cells.
```
