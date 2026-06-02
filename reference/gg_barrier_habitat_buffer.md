# Plot barrier, habitat, and buffer layers

Creates a visualisation of habitat, buffer zone, and barriers using
terra rasters.

## Usage

``` r
gg_barrier_habitat_buffer(
  barrier,
  buffered,
  habitat,
  distance,
  species,
  col_barrier,
  col_buffer,
  col_habitat,
  col_paper = NA
)
```

## Arguments

- barrier:

  Terra SpatRaster. Barrier layer (e.g., roads).

- buffered:

  Terra SpatRaster. Buffered habitat layer.

- habitat:

  Terra SpatRaster. Original habitat layer.

- distance:

  Numeric. Buffer distance in meters.

- species:

  Character. Species name for plot title.

- col_barrier:

  Character. Color for barrier layer.

- col_buffer:

  Character. Color for buffer zone.

- col_habitat:

  Character. Color for habitat patches.

- col_paper:

  Character. Background color (default: "white").

## Value

A ggplot2 object.

## Examples

``` r
lizard_habitat <- example_habitat()
lizard_barrier <- example_barrier()
lizard_buffered <- habitat_buffer(lizard_habitat, 10)
gg_bar_hab_buf <- gg_barrier_habitat_buffer(
  barrier = lizard_barrier,
  buffered = lizard_buffered,
  habitat = lizard_habitat,
  distance = 10,
  species = "Blue Tongue Lizard",
  col_barrier = "black",
  col_buffer = "lightgreen",
  col_habitat = "seagreen"
)
#> <SpatRaster> resampled to 500554 cells.
#> <SpatRaster> resampled to 500554 cells.
#> <SpatRaster> resampled to 500554 cells.
gg_bar_hab_buf


# add north arrow and scale bar with ggspatial
library(ggspatial)
library(tidyterra)
#> 
#> Attaching package: ‘tidyterra’
#> The following object is masked from ‘package:stats’:
#> 
#>     filter
gg_bar_hab_buf +
 annotation_north_arrow(
   style = north_arrow_fancy_orienteering()
  ) +
  annotation_scale()
```
