# Assign habitat patches to fragment IDs

Determines which connected fragment each remaining habitat patch belongs
to based on spatial intersection.

## Usage

``` r
sf_assign_patches_to_fragments(remaining, fragment_id)
```

## Arguments

- remaining:

  SF object. Remaining habitat patches after barrier removal.

- fragment_id:

  SF object. Fragment geometries with IDs.

## Value

SF object with habitat patches labeled by their fragment ID.

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
sf_assign_patches_to_fragments(remaining, fragments)
} # }
```
