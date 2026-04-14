
<!-- README.md is generated from README.Rmd. Please edit that file -->

# urbioconnect

<!-- badges: start -->

[![R-CMD-check](https://github.com/urbio-ecology/urbioconnect/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/urbio-ecology/urbioconnect/actions/workflows/R-CMD-check.yaml)
[![R-universe
version](https://urbio-ecology.r-universe.dev/urbioconnect/badges/version)](https://urbio-ecology.r-universe.dev/urbioconnect)
[![Codecov test
coverage](https://codecov.io/gh/urbio-ecology/urbioconnect/graph/badge.svg)](https://app.codecov.io/gh/urbio-ecology/urbioconnect)
<!-- badges: end -->

`urbioconnect` quantifies how connected habitats are in urban
landscapes. This is done by providing **habitat** and **barrier**
information. For example, raster grid data on blue tongued-lizard
**habitat**, how far their roaming range is (say, 100 metres) and raster
grid data on road and buildings (**barriers**). It then computes
connected habitat patches and a set of landscape-level metrics:

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

We include a Shiny app for interactive analysis and report generation.
These are powered by a set of R functions, so you can implement your own
methods or approaches to pipelines.

## Installation

Install from R-universe:

``` r
install.packages(
  "urbioconnect",
  repos = c("https://urbio-ecology.r-universe.dev", "https://cloud.r-project.org")
)
```

Or install the development version from GitHub:

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

# run the full raster pipeline at a 100 m buffer distance
areas <- habitat_connectivity(
  habitat  = habitat,
  barrier  = barrier,
  distance = 10,
  verbose  = FALSE
)

head(areas)
#> # A tibble: 6 × 3
#>   patch_id   area area_squared
#>      <dbl>  <dbl>        <dbl>
#> 1        1 97878. 9580104085. 
#> 2       15  2416.    5837832. 
#> 3       18  1304.    1700646. 
#> 4       32  1592.    2534763. 
#> 5       37     4          16.0
#> 6       39  3332.   11103470.
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

## Shiny app

Launch the interactive Shiny app:

``` r
run_connectivity_app()
```

A hosted version is available at:
<https://njtierney.shinyapps.io/urbioconnect/>

## Two pipelines: raster and vector

`urbioconnect` provides two complete analysis pipelines:

|  | Raster pipeline | Vector pipeline |
|----|----|----|
| Input format | `terra` SpatRaster | `sf` polygons |
| Main function | `habitat_connectivity()` | `sf_habitat_connectivity()` |
| Best for | Large study areas, GeoTIFF output | Small precise study areas, exact polygon boundaries |

You can see the vignette, “raster-vs-vector” for a side-by-side
comparison and guidance on which to choose with

``` r
vignette("raster-vs-vector")
```

Other vignettes include:

- `vignette("getting-started")` — step-by-step raster workflow with
  example data
- `vignette("targets-pipeline")` — using `urbioconnect` in a `targets`
  reproducible pipeline

## Example targets workflow

For a complete, production-ready `targets` pipeline with real species
data and report generation, see:

**<https://github.com/urbio-ecology/urbio-eco-targets>**
