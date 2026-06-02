# Calculate habitat connectivity using terra

This performs the entire connectivity workflow, returning a dataframe
output. The steps are:

- [`create_barrier_mask()`](https://urbio-ecology.github.io/urbioconnect/reference/create_barrier_mask.md):
  Creating barrier mask.

- [`drop_habitat_under_barrier()`](https://urbio-ecology.github.io/urbioconnect/reference/drop_habitat_under_barrier.md):
  Removes Habitat underneath barrier.

- [`habitat_buffer()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_buffer.md):
  Adds buffer of distance (m) to habitat layer.

- [`fragment_habitat()`](https://urbio-ecology.github.io/urbioconnect/reference/fragment_habitat.md):
  Fragments habitat layer along barrier intersection.

- [`assign_patches_to_fragments()`](https://urbio-ecology.github.io/urbioconnect/reference/assign_patches_to_fragments.md):
  Assign patch ID to fragments.

- [`aggregate_connected_patches()`](https://urbio-ecology.github.io/urbioconnect/reference/aggregate_connected_patches.md):
  Summarise area in each patch.

## Usage

``` r
habitat_connectivity(habitat, barrier, distance, verbose = TRUE)
```

## Arguments

- habitat:

  Terra SpatRaster. Habitat raster.

- barrier:

  Terra SpatRaster. Barrier raster.

- distance:

  Numeric. Buffer distance in meters.

- verbose:

  Logical. Display progress messages (default: TRUE).

## Value

Data frame with connectivity metrics per patch.

## Examples

``` r
lizard_habitat <- example_habitat()
lizard_barrier <- example_barrier()
connectivity <- habitat_connectivity(
    habitat = lizard_habitat,
    barrier = lizard_barrier,
    distance = 10
  )
#> ℹ Creating barrier mask
#> ✔ Creating barrier mask [32ms]
#> 
#> ℹ Removing habitat underneath barrier
#> ✔ Removing habitat underneath barrier [23ms]
#> 
#> ℹ Adding buffer of 10m to habitat layer
#> ✔ Adding buffer of 10m to habitat layer [430ms]
#> 
#> ℹ Fragmenting habitat layer along barrier intersection
#> ✔ Fragmenting habitat layer along barrier intersection [21ms]
#> 
#> ℹ Assigning patches ID to fragments
#> ✔ Assigning patches ID to fragments [1.1s]
#> 
#> ℹ Summarising area in each patch
#> ✔ Summarising area in each patch [46ms]
#> 
connectivity
#> # A tibble: 163 × 3
#>    patch_id    area area_squared
#>       <dbl>   <dbl>        <dbl>
#>  1        1 97878.  9580104085. 
#>  2       15  2416.     5837832. 
#>  3       18  1304.     1700646. 
#>  4       32  1592.     2534763. 
#>  5       37     4           16.0
#>  6       39  3332.    11103470. 
#>  7       40   132.       17426. 
#>  8       44   108.       11665. 
#>  9       47    36.0       1296. 
#> 10       57  1112.     1236681. 
#> # ℹ 153 more rows
```
