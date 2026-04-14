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
# run with a small buffer distance
habitat_buffer(lizard_habitat, 5)
#> class       : SpatRaster 
#> size        : 763, 766, 1  (nrow, ncol, nlyr)
#> resolution  : 2, 2  (x, y)
#> extent      : 326109.6, 327641.6, 5820362, 5821888  (xmin, xmax, ymin, ymax)
#> coord. ref. : GDA94 / MGA zone 55 (EPSG:28355) 
#> source(s)   : memory
#> varname     : lizard_habitat_raster 
#> name        : focal_max 
#> min value   :         1 
#> max value   :         1 
```
