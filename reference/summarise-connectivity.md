# Summarise connectivity metrics

Calculates a comprehensive set of habitat connectivity metrics including
effective mesh size, probability of connectedness, and patch statistics.
Intended for usage from objects created by
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md),
but raw area vectors can be passed. See examples below.

## Usage

``` r
summarise_connectivity(connectivity, connectivity_baseline = NULL, ...)

# Default S3 method
summarise_connectivity(
  connectivity,
  connectivity_baseline = NULL,
  interpatch_distance,
  data_resolution,
  species,
  ...
)
```

## Arguments

- connectivity:

  data.frame of class "patch_size", obtained from
  [`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md).
  Contains area measurements of connected patches.

- connectivity_baseline:

  Optional. data.frame of class "patch_size", obtained from
  [`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md).
  Contains baseline area measurements of connected patches. Default is
  NULL.

- ...:

  extra arguments to pass through for default method.

- interpatch_distance:

  Numeric. The distance (in meters) where habitat patches are considered
  connected. E.g., if set to 500, patches 498m apart are connected,
  those 501m apart are not connected. This is passed internally to a
  spatial operation known as "buffering", where this distance is used as
  a radius from the edge of the habitat zone. This means the specified
  `interpatch_distance` is halved exactly. So an interpatch distance of
  500 will be converted to 250.

- data_resolution:

  Numeric. Data resolution in meters.

- species:

  Character. Name of species analysed.

## Value

A tibble with connectivity metrics including number of patches,
probability of connectedness, effective mesh size, mean and total patch
areas.

## Examples

``` r
summarise_connectivity(
  connectivity = lizard_areas_connected$area,
  interpatch_distance = 10,
  data_resolution = 10,
  species = "Blue-tongued Lizard"
)
#> # A tibble: 1 × 8
#>   species     interpatch_distance n_patches effective_mesh_ha prob_connectedness
#>   <chr>                     <dbl>     <int>             <dbl>              <dbl>
#> 1 Blue-tongu…                  10        73                 4           0.000017
#> # ℹ 3 more variables: patch_area_mean <dbl>, patch_area_total_ha <dbl>,
#> #   data_resolution <dbl>
```
