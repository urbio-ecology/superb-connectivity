# Aggregate connected patch areas

Groups habitat patches by their connected fragment ID and calculates
total and squared areas for connectivity metrics.

## Usage

``` r
sf_aggregate_connected_patches(patch_areas)
```

## Arguments

- patch_areas:

  SF object. Habitat patches with area column.

## Value

Data frame with `patch_id`, `area`, and `area_squared` columns.

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
buffered <- sf_habitat_buffer(lizard_habitat_sf, buffer_radius = 10)
fragments <- sf_fragment_habitat(buffered, lizard_barrier_shp)
remaining <- sf_drop_habitat_under_barrier(lizard_habitat_sf, lizard_barrier_shp)
patches <- sf_assign_patches_to_fragments(remaining, fragments) |>
  sf_add_patch_area()
sf_aggregate_connected_patches(patches)
} # }
```
