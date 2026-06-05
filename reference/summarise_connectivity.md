# Summarise connectivity metrics

Calculates a comprehensive set of habitat connectivity metrics including
effective mesh size, probability of connectedness, and patch statistics.
Intended for usage from objects created by
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md).
See examples below.

## Usage

``` r
summarise_connectivity(
  area,
  area_baseline = NULL,
  interpatch_distance,
  target_resolution,
  data_resolution,
  aggregation_factor,
  species
)
```

## Arguments

- area:

  Numeric vector. Areas of connected patches.

- area_baseline:

  Numeric vector. Areas of connected patch baseline.

- interpatch_distance:

  Numeric. The distance (in meters) where habitat patches are considered
  connected. E.g., if set to 500, patches 498m apart are connected,
  those 501m apart are not connected. This is passed internally to a
  spatial operation known as "buffering", where this distance is used as
  a radius from the edge of the habitat zone. This means the specified
  `interpatch_distance` is halved exactly. So an interpatch distance of
  500 will be converted to 250.

- target_resolution:

  Numeric. Target resolution in meters.

- data_resolution:

  Numeric. Data resolution in meters.

- aggregation_factor:

  Numeric. Factor by which Data resolution was aggregated.

- species:

  Character. Name of species analysed.

## Value

A tibble with connectivity metrics including number of patches,
probability of connectedness, effective mesh size, mean and total patch
areas.

## Examples

``` r
summarise_connectivity(
  area = lizard_areas_connected$area,
  interpatch_distance = 10,
  target_resolution = 500,
  data_resolution = 10,
  aggregation_factor = 50,
  species = "Blue-tongued Lizard"
)
#> # A tibble: 1 × 10
#>   species     interpatch_distance n_patches effective_mesh_ha prob_connectedness
#>   <chr>                     <dbl>     <int>             <dbl>              <dbl>
#> 1 Blue-tongu…                  10        59                 4           0.000017
#> # ℹ 5 more variables: patch_area_mean <dbl>, patch_area_total_ha <dbl>,
#> #   target_resolution <dbl>, data_resolution <dbl>, aggregation_factor <dbl>
```
