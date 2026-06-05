# Calculate habitat connectivity

Performs complete habitat connectivity analysis using vector-based
spatial operations. Buffers habitat, fragments it along barriers, and
calculates areas of connected patches.

## Usage

``` r
sf_habitat_connectivity(
  habitat,
  barrier,
  interpatch_distance = NULL,
  buffer_radius = NULL
)
```

## Arguments

- habitat:

  SF object. Original habitat spatial data.

- barrier:

  SF object. Barrier spatial data (e.g., roads, waterways).

- interpatch_distance:

  Numeric. The distance (in meters) where habitat patches are considered
  connected. E.g., if set to 500, patches 498m apart are connected,
  those 501m apart are not connected. This is passed internally to a
  spatial operation known as "buffering", where this distance is used as
  a radius from the edge of the habitat zone. This means the specified
  `interpatch_distance` is halved exactly. So an interpatch distance of
  500 will be converted to 250. Note that `interpatch_distance` is
  mutually exclusive to `habitat_buffer`, so you can only specify either
  `interpatch_distance` or `habitat_buffer`, and never both.

- buffer_radius:

  Numeric. The radius in metres around the habitat. Since patches of
  habitat will be connected when their edge-to-edge gap is \<= 2 \*
  `buffer radius`, we recommend you specify `buffer_radius` to be half
  the "interpatch distance". This is the distance past which habitat
  patches are no longer considered connected. For example, if your
  interpatch distance is 500m, set `buffer_radius = 250`. Note that
  `interpatch_distance` is mutually exclusive to `habitat_buffer`, so
  you can only specify either `interpatch_distance` or `habitat_buffer`,
  and never both.

## Value

Data frame with connectivity metrics for each connected patch, including
`patch_id`, `area`, and `area_squared`.

## Examples

``` r
lizard_barrier_shp <- example_barrier_shp()
#> Reading layer `lizard_barrier' from data source 
#>   `/home/runner/work/_temp/Library/urbioconnect/ex/lizard_barrier.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 326089.6 ymin: 5820342 xmax: 327662.5 ymax: 5821909
#> Projected CRS: GDA94 / MGA zone 55
lizard_habitat_sf <- terra::as.polygons(example_habitat(), dissolve = TRUE) |>
  sf::st_as_sf()
result <- sf_habitat_connectivity(lizard_habitat_sf, lizard_barrier_shp, interpatch_distance = 10)
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Warning: repeating attributes for all sub-geometries for which they may not be constant
result
#> # A tibble: 479 × 3
#>    patch_id   area area_squared
#>       <dbl>  <dbl>        <dbl>
#>  1        1 1536.      2360688.
#>  2        2  287.        82321.
#>  3        3  292.        85089.
#>  4        4  851.       723456.
#>  5        5    8            64 
#>  6        6 3056.      9340981.
#>  7        7   83.2        6921.
#>  8        8   47.9        2294.
#>  9        9  307.        94169.
#> 10       11   24           576 
#> # ℹ 469 more rows
```
