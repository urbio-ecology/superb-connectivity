# Calculate habitat connectivity with visualization data

Like
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md),
but also returns the intermediate rasters (buffered habitat, patch ID
raster, barrier mask, remaining habitat) useful for mapping and
reporting.

## Usage

``` r
habitat_connectivity_full(habitat, barrier, distance, verbose = TRUE)
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

Named list with elements: `buffered_habitat`, `patch_id_raster`,
`areas_connected`, `barrier_mask`, `remaining_habitat`.

## Examples

``` r
lizard_habitat <- example_habitat()
lizard_barrier <- example_barrier()
result <- habitat_connectivity_full(
  lizard_habitat,
  lizard_barrier,
  distance = 10,
  verbose = FALSE
)
names(result)
#> [1] "buffered_habitat"  "patch_id_raster"   "areas_connected"  
#> [4] "barrier_mask"      "remaining_habitat"
```
