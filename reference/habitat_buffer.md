# Buffer habitat raster

Buffer around the habitat a given distance in metres using
`terra::focalMat(d = buffer_radius, type = "circle")`. This operation is
used to identify connected patches of habitat. Two patches will connect
when their edge-to-edge gap is \<= 2 \* `buffer radius`. So, we
recommend you specify the `buffer_radius` value to be half the
interpatch distance, which is the distance past which habitat patches
are no longer considered connected. For example, if your interpatch
distance is 500m, set `buffer_radius = 250`.

## Usage

``` r
habitat_buffer(habitat, buffer_radius)
```

## Arguments

- habitat:

  Terra SpatRaster. Habitat raster.

- buffer_radius:

  Numeric. The radius in metres around the habitat. Since patches of
  habitat will be connected when their edge-to-edge gap is \<= 2 \*
  `buffer radius`, we recommend you specify `buffer_radius` to be half
  the "interpatch distance". This is the distance past which habitat
  patches are no longer considered connected. For example, if your
  interpatch distance is 500m, set `buffer_radius = 250`. The buffer can
  only be represented if it is at least one raster cell, i.e. keep
  `resolution <= interpatch_distance / 2`. Below that the buffer is a
  no-op: `habitat_buffer()` warns and returns the habitat unchanged. See
  [`vignette("interpatch-distance-and-resolution")`](https://urbio-ecology.github.io/urbioconnect/articles/interpatch-distance-and-resolution.md).

## Value

Terra SpatRaster with buffered habitat.

## See also

[`vignette("interpatch-distance-and-resolution")`](https://urbio-ecology.github.io/urbioconnect/articles/interpatch-distance-and-resolution.md)
for the relationship between interpatch distance, buffer radius, and
resolution.

## Examples

``` r
lizard_habitat <- example_habitat()
library(terra)
plot(lizard_habitat, col = "darkgreen", legend = FALSE)

# run with a small buffer radius
lizard_buff <- habitat_buffer(lizard_habitat, buffer_radius = 10)
plot(lizard_buff, col = "lightgreen", legend = FALSE)
plot(lizard_habitat, col = "darkgreen", legend = FALSE, add = TRUE)
```
