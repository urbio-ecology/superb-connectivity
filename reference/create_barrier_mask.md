# Create barrier mask

Converts a barrier layer into a multiplier mask for connectivity
analysis. Takes a raster where barriers are coded as 1 (and non-barriers
as NA), and inverts it to produce a mask with NA values where barriers
exist and 1 elsewhere. This format allows barriers to be applied by
multiplying the mask with connectivity surfaces, effectively blocking
movement through barrier cells.

## Usage

``` r
create_barrier_mask(barrier)
```

## Arguments

- barrier:

  Terra SpatRaster. Barrier layer with 1 = barrier, NA = no barrier.

## Value

Terra SpatRaster. Mask with 1 where movement is allowed, NA where
barriers exist.

## Examples

``` r
lizard_barrier <- example_barrier()
create_barrier_mask(lizard_barrier)
#> class       : SpatRaster
#> size        : 763, 766, 1  (nrow, ncol, nlyr)
#> resolution  : 2, 2  (x, y)
#> extent      : 326109.6, 327641.6, 5820362, 5821888  (xmin, xmax, ymin, ymax)
#> coord. ref. : GDA94 / MGA zone 55 (EPSG:28355)
#> source(s)   : memory
#> varname     : lizard_barrier_raster
#> name        : layer
#> min value   :     1
#> max value   :     1
```
