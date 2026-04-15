# Buffer habitat raster

Buffer around the habitat a given distance in metres using
[`terra::focal()`](https://rspatial.github.io/terra/reference/focal.html).
We recommend you buffer the habitat by half the threshold distance (the
distance past which habitat patches are no longer considered connected).

## Usage

``` r
habitat_buffer(habitat, distance)
```

## Arguments

- habitat:

  Terra SpatRaster. Habitat raster.

- distance:

  Numeric. Buffer distance in meters.

## Value

Terra SpatRaster with buffered habitat.

## Examples

``` r
lizard_habitat <- example_habitat()
library(terra)
plot(lizard_habitat, col = "darkgreen", legend = FALSE)

# run with a small buffer distance
lizard_buff <- habitat_buffer(lizard_habitat, 10)
plot(lizard_buff, col = "lightgreen", legend = FALSE)
plot(lizard_habitat, col = "darkgreen", legend = FALSE, add = TRUE)
```
