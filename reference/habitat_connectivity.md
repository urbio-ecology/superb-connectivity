# Calculate habitat connectivity using terra

This performs the entire connectivity workflow, returning a dataframe
output. The steps are:

- [`create_barrier_mask()`](https://urbio-ecology.github.io/urbioconnect/reference/create_barrier_mask.md):
  Creating barrier mask.

- [`drop_habitat_under_barrier()`](https://urbio-ecology.github.io/urbioconnect/reference/drop_habitat_under_barrier.md):
  Removes Habitat underneath barrier.

- [`habitat_buffer()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_buffer.md):
  Buffers the habitat layer by the interpatch distance (m).

- [`fragment_habitat()`](https://urbio-ecology.github.io/urbioconnect/reference/fragment_habitat.md):
  Fragments habitat layer along barrier intersection.

- [`assign_patches_to_fragments()`](https://urbio-ecology.github.io/urbioconnect/reference/assign_patches_to_fragments.md):
  Assign patch ID to fragments.

- [`aggregate_connected_patches()`](https://urbio-ecology.github.io/urbioconnect/reference/aggregate_connected_patches.md):
  Summarise area in each patch.

## Usage

``` r
habitat_connectivity(
  habitat,
  barrier,
  interpatch_distance = NULL,
  buffer_radius = NULL,
  verbose = TRUE
)
```

## Arguments

- habitat:

  Terra SpatRaster. Habitat raster.

- barrier:

  Terra SpatRaster. Barrier raster.

- interpatch_distance:

  Numeric. The distance (in meters) where habitat patches are considered
  connected. E.g., if set to 500, patches 498m apart are connected,
  those 501m apart are not connected. This is passed internally to a
  spatial operation known as "buffering", where this distance is used as
  a radius from the edge of the habitat zone. This means the specified
  `interpatch_distance` is halved exactly. So an interpatch distance of
  500 will be converted to 250. For the buffer to be representable on
  the raster, keep `resolution <= interpatch_distance / 2`; below that
  the buffer is a no-op and a warning is raised. See
  [`vignette("interpatch-distance-and-resolution")`](https://urbio-ecology.github.io/urbioconnect/articles/interpatch-distance-and-resolution.md).

- buffer_radius:

  Numeric. The radius in metres around the habitat. Since patches of
  habitat will be connected when their edge-to-edge gap is \<= 2 \*
  `buffer radius`, we recommend you specify `buffer_radius` to be half
  the "interpatch distance". This is the distance past which habitat
  patches are no longer considered connected. For example, if your
  interpatch distance is 500m, set `buffer_radius = 250`. The buffer can
  only be represented if it is at least one raster cell, i.e. keep
  `resolution <= interpatch_distance / 2`. Below that the buffer is a
  no-op:
  [`habitat_buffer()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_buffer.md)
  warns and returns the habitat unchanged. See
  [`vignette("interpatch-distance-and-resolution")`](https://urbio-ecology.github.io/urbioconnect/articles/interpatch-distance-and-resolution.md).

- verbose:

  Logical. Display progress messages (default: TRUE).

## Value

Data frame with connectivity metrics per patch.

## See also

[`vignette("interpatch-distance-and-resolution")`](https://urbio-ecology.github.io/urbioconnect/articles/interpatch-distance-and-resolution.md)
for the relationship between interpatch distance, buffer radius, and
resolution.

## Examples

``` r
lizard_habitat <- example_habitat()
lizard_barrier <- example_barrier()
connectivity <- habitat_connectivity(
    habitat = lizard_habitat,
    barrier = lizard_barrier,
    interpatch_distance = 10
  )
#> ℹ Creating barrier mask
#> ✔ Creating barrier mask [31ms]
#> 
#> ℹ Removing habitat underneath barrier
#> ✔ Removing habitat underneath barrier [23ms]
#> 
#> ℹ Adding 5m buffer (interpatch distance 10m)
#> Warning: Buffer radius doesn't align with the raster resolution.
#> ✖ 5 m isn't a multiple of 2 m.
#> ℹ It snaps to 4 m (interpatch distance 8 m).
#> ℹ Connectivity may shift for patches near the cut-off.
#> ℹ See `vignette(urbioconnect::interpatch-distance-and-resolution)`.
#> ✔ Adding 5m buffer (interpatch distance 10m) [162ms]
#> 
#> ℹ Fragmenting habitat layer along barrier intersection
#> ✔ Fragmenting habitat layer along barrier intersection [20ms]
#> 
#> ℹ Assigning patches ID to fragments
#> ✔ Assigning patches ID to fragments [2.5s]
#> 
#> ℹ Summarising area in each patch
#> ✔ Summarising area in each patch [37ms]
#> 
connectivity
#> # A tibble: 703 × 3
#>    patch_id    area area_squared
#>       <dbl>   <dbl>        <dbl>
#>  1        1    60.0        3600.
#>  2        2  1648.      2716264.
#>  3        7    12.0         144.
#>  4        8  1200.      1440195.
#>  5       10 92034.   8470191653.
#>  6       11    40.0        1600.
#>  7       12   288.        82955.
#>  8       28  1156.      1336497.
#>  9       30    64.0        4096.
#> 10       31   940.       883705.
#> # ℹ 693 more rows
```
