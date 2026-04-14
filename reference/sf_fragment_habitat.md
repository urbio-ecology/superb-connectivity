# Fragment habitat along barriers

Removes barrier areas from buffered habitat and splits the result into
individual polygon fragments.

## Usage

``` r
sf_fragment_habitat(habitat_buffered, barrier)
```

## Arguments

- habitat_buffered:

  SF object. Buffered habitat geometry.

- barrier:

  SF object. Barrier geometry (e.g., roads).

## Value

SF object with individual habitat fragments, each with a unique ID.

## Examples

``` r
lizard_habitat_sf <- terra::as.polygons(example_habitat(), dissolve = TRUE) |>
  sf::st_as_sf()
lizard_barrier_shp <- example_barrier_shp()
#> Reading layer `lizard_barrier' from data source 
#>   `/home/runner/work/_temp/Library/urbioconnect/ex/lizard_barrier.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 326089.6 ymin: 5820342 xmax: 327662.5 ymax: 5821909
#> Projected CRS: GDA94 / MGA zone 55
if (FALSE) { # \dontrun{
buffered <- sf_habitat_buffer(lizard_habitat_sf, distance = 10)
sf_fragment_habitat(buffered, lizard_barrier_shp)
} # }
```
