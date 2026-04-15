
<!-- README.md is generated from README.Rmd. Please edit that file -->

# urbioconnect

<!-- badges: start -->

[![R-CMD-check](https://github.com/urbio-ecology/urbioconnect/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/urbio-ecology/urbioconnect/actions/workflows/R-CMD-check.yaml)
[![R-universe
version](https://urbio-ecology.r-universe.dev/urbioconnect/badges/version)](https://urbio-ecology.r-universe.dev/urbioconnect)
[![Codecov test
coverage](https://codecov.io/gh/urbio-ecology/urbioconnect/graph/badge.svg)](https://app.codecov.io/gh/urbio-ecology/urbioconnect)
<!-- badges: end -->

`urbioconnect` implements methods that help quantify the ecological
connectivity for different urban wildlife species.

This is done by providing wildlife **habitat** and urban **barrier**
information, the roaming range (in metres) of the wildlife, and some
other parameters.

For example, raster grid data on blue tongued-lizard **habitat**, how
far their roaming range is (say, 100 metres) and raster grid data on
road and buildings (**barriers**). It then computes metrics that help
assess how connected these habitat patches are:

- effective mesh size
- probability of connectedness
- number of patches,
- total patch area and
- mean patch area

Note that you can use **vector** or **raster** data for habitat and
barrier formats.

This method is described in:

> Kirk, H., Soanes, K., Amati, M., Bekessy, S., Harrison, L., Parris,
> K., Ramalho, C., van de Ree, R., & Threlfall, C. (2023). Ecological
> connectivity as a planning tool for the conservation of wildlife in
> cities. *MethodsX*, 10, 101989.
> <https://doi.org/10.1016/j.mex.2022.101989>

See the [getting
started](https://urbio-ecology.github.io/urbioconnect/articles/getting-started.html)
vignette for more details.

## Shiny app

We include a Shiny app for interactive analysis and report generation.
You can launch the interactive Shiny app with:

``` r
run_connectivity_app()
```

A hosted version is available at:
<https://njtierney.shinyapps.io/urbioconnect/>

However, it is worth noting you do not need the shiny app to perform
analyses, all functions have been designed to work together in a
pipeline - see example usage below.

## Installation

Install from R-universe:

``` r
install.packages(
  "urbioconnect",
  repos = c("https://urbio-ecology.r-universe.dev", "https://cloud.r-project.org")
)
```

Or install from GitHub:

``` r
# install.packages("pak")
pak::pak("urbio-ecology/urbioconnect")
```

## Get started

``` r
library(urbioconnect)

# load example habitat and barrier rasters
habitat <- example_habitat()
barrier <- example_barrier()

# run the full raster pipeline at a 10m buffer distance
areas <- habitat_connectivity(
  habitat  = habitat,
  barrier  = barrier,
  distance = 10
)
#> ℹ Creating barrier mask
#> ✔ Creating barrier mask [44ms]
#> 
#> ℹ Removing habitat underneath barrier
#> ✔ Removing habitat underneath barrier [17ms]
#> 
#> ℹ Adding buffer of 10m to habitat layer
#> ✔ Adding buffer of 10m to habitat layer [261ms]
#> 
#> ℹ Fragmenting habitat layer along barrier intersection
#> ✔ Fragmenting habitat layer along barrier intersection [14ms]
#> 
#> ℹ Assigning patches ID to fragments
#> ✔ Assigning patches ID to fragments [701ms]
#> 
#> ℹ Summarising area in each patch
#> ✔ Summarising area in each patch [1.8s]
#> 

areas
#> # A tibble: 163 × 3
#>    patch_id    area area_squared
#>       <dbl>   <dbl>        <dbl>
#>  1        1 97878.  9580104085. 
#>  2       15  2416.     5837832. 
#>  3       18  1304.     1700646. 
#>  4       32  1592.     2534763. 
#>  5       37     4           16.0
#>  6       39  3332.    11103470. 
#>  7       40   132.       17426. 
#>  8       44   108.       11665. 
#>  9       47    36.0       1296. 
#> 10       57  1112.     1236681. 
#> # ℹ 153 more rows
```

Summarise the connectivity metrics:

``` r
summarise_connectivity(
  area_squared     = areas$area_squared,
  area_total       = areas$area,
  buffer_distance  = 100,
  target_resolution = 500,
  data_resolution  = 10,
  aggregation_factor = 50,
  species_name     = "Blue-tongued Lizard"
)
#> # A tibble: 1 × 10
#>   species_name    buffer_distance n_patches prob_connectedness effective_mesh_ha
#>   <chr>                     <dbl>     <int>              <dbl>             <dbl>
#> 1 Blue-tongued L…             100       163           0.000017                 4
#> # ℹ 5 more variables: patch_area_mean <dbl>, patch_area_total_ha <dbl>,
#> #   target_resolution <dbl>, data_resolution <dbl>, aggregation_factor <dbl>
```

## Use raster and vector

You can use raster and vector data with `urbioconnect`:

|  | Raster pipeline | Vector pipeline |
|----|----|----|
| Input format | `terra` SpatRaster | `sf` polygons |
| Main function | `habitat_connectivity()` | `sf_habitat_connectivity()` |
| Best for | Large study areas, GeoTIFF output | Small precise study areas, exact polygon boundaries |

You can see the vignette,
[“raster-vs-vector”](https://urbio-ecology.github.io/urbioconnect/articles/raster-vs-vector.html)
for a side-by-side comparison and guidance on which to choose.

# Example usage

We have an example pipeline using `targets` and `geotarget`, which
includes generating reports as output at
<https://github.com/urbio-ecology/urbio-eco-targets>. See the vignette
[“Using urbioconnect in a targets
pipeline”](https://urbio-ecology.github.io/urbioconnect/articles/targets-pipeline.html)
for more detail.

# Acknowledgements

We would like to thank Kylie Soanes, Marco Amati, Sarah Bekessy, Lee
Harrison, Kirsten Parris, Cristina Ramalho, Rodney van de Ree, and
Caragh Threlfall for their work on the original paper this work is based
upon. We would also like to thank Hugh Stanford, Nadine Gaskell, Kerryn
Kneebone, and Nicholas Golding for their comments and insight during the
development of the methods and software.
