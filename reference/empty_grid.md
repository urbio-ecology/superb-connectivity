# Create Empty terra raster grid

Create Empty terra raster grid

## Usage

``` r
empty_grid(habitat, resolution = 10)
```

## Arguments

- habitat:

  SF object or terra SpatRaster.

- resolution:

  Numeric. Cell size in meters (default: 10).

## Value

Terra SpatRaster. Empty raster grid.

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
empty_grid(lizard_barrier_shp, resolution = 10)
#> class       : SpatRaster
#> size        : 157, 157, 1  (nrow, ncol, nlyr)
#> resolution  : 10.0184, 9.979108  (x, y)
#> extent      : 326089.6, 327662.5, 5820342, 5821909  (xmin, xmax, ymin, ymax)
#> coord. ref. : GDA94 / MGA zone 55 (EPSG:28355)
```
