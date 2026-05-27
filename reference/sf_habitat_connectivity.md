# Calculate habitat connectivity

Performs complete habitat connectivity analysis using vector-based
spatial operations. Buffers habitat, fragments it along barriers, and
calculates areas of connected patches.

## Usage

``` r
sf_habitat_connectivity(habitat, barrier, distance)
```

## Arguments

- habitat:

  SF object. Original habitat spatial data.

- barrier:

  SF object. Barrier spatial data (e.g., roads, waterways).

- distance:

  Numeric. Threshold distance in meters for connectivity. Habitat
  patches within this distance are considered connected.

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
result <- sf_habitat_connectivity(lizard_habitat_sf, lizard_barrier_shp, distance = 10)
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Warning: repeating attributes for all sub-geometries for which they may not be constant
result
#> # A tibble: 136 × 3
#>    patch_id   area area_squared
#>       <dbl>  <dbl>        <dbl>
#>  1        1  1102.     1213405.
#>  2        3  3325.    11054714.
#>  3        4   173.       29758.
#>  4        6     4           16 
#>  5        7  1582.     2501863.
#>  6        8 97842.  9573070108.
#>  7        9  2412.     5819302.
#>  8       10  1299.     1688592.
#>  9       11   810.      656219.
#> 10       12   974.      947954.
#> # ℹ 126 more rows
```
