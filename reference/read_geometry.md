# Read shapefile geometry

Reads a shapefile and extracts only the spatial geometry, discarding
attribute data.

## Usage

``` r
read_geometry(shapefile)
```

## Arguments

- shapefile:

  Character. File path to a shapefile or an SF object.

## Value

An `sfc` object containing only the spatial geometry.

## Examples

``` r
# Read geometry from a shapefile path
barrier_path <- system.file("ex/lizard_barrier.shp", package = "urbioconnect")
barrier_geom <- read_geometry(barrier_path)
#> Reading layer `lizard_barrier' from data source 
#>   `/home/runner/work/_temp/Library/urbioconnect/ex/lizard_barrier.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 326089.6 ymin: 5820342 xmax: 327662.5 ymax: 5821909
#> Projected CRS: GDA94 / MGA zone 55
```
