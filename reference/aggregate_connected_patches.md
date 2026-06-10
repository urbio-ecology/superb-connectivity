# Aggregate connected patch areas

Aggregate a raster with connected patch areas into a data frame where
each row is a unique patch, and its area and area squared.

## Usage

``` r
aggregate_connected_patches(raster)
```

## Arguments

- raster:

  terra SpatRaster. Raster with patch_id and area layers.

## Value

Data frame with patch areas and areas squared.

## Examples

``` r
lizard_habitat <- example_habitat()
lizard_barrier <- example_barrier()
buffered_habitat <- habitat_buffer(lizard_habitat, 5)
#> Warning: Buffer radius doesn't align with the raster resolution.
#> ✖ 5 m isn't a multiple of 2 m.
#> ℹ It snaps to 4 m (interpatch distance 8 m).
#> ℹ Connectivity may shift for patches near the cut-off.
#> ℹ See `vignette(urbioconnect::interpatch-distance-and-resolution)`.
barrier_mask <- create_barrier_mask(lizard_barrier)
fragmented <- fragment_habitat(buffered_habitat, barrier_mask)
remaining_habitat <- drop_habitat_under_barrier(
  habitat = lizard_habitat,
  barrier = lizard_barrier
  )
fragment_patches <- assign_patches_to_fragments(
  remaining_habitat = remaining_habitat,
  fragment = fragmented
  )
library(terra)
patch_areas <- add_patch_area(fragment_patches)
aggregate_connected_patches(patch_areas)
```
