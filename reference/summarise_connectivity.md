# Summarise connectivity metrics

Calculates a comprehensive set of habitat connectivity metrics including
effective mesh size, probability of connectedness, and patch statistics.
Intended for usage from objects created by
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md).
See examples below.

## Usage

``` r
summarise_connectivity(
  area_squared,
  area,
  buffer_distance,
  target_resolution,
  data_resolution,
  aggregation_factor,
  species_name
)
```

## Arguments

- area_squared:

  Numeric vector. Squared areas of connected patches.

- area:

  Numeric vector. Area of a connected patch.

- buffer_distance:

  Numeric. Buffer distance used in analysis (meters).

- target_resolution:

  Numeric. Target resolution in meters.

- data_resolution:

  Numeric. Data resolution in meters.

- aggregation_factor:

  Numeric. Factor by which Data resolution was aggregated.

- species_name:

  Character. Name of species analysed.

## Value

A tibble with connectivity metrics including number of patches,
probability of connectedness, effective mesh size, mean and total patch
areas.

## Examples

``` r
summarise_connectivity(
  area_squared = lizard_areas_connected$area_squared,
  area = lizard_areas_connected$area,
  buffer_distance = 10,
  target_resolution = 500,
  data_resolution = 10,
  aggregation_factor = 50,
  species_name = "Blue-tongued Lizard"
)
#> # A tibble: 1 × 10
#>   species_name    buffer_distance n_patches effective_mesh_ha prob_connectedness
#>   <chr>                     <dbl>     <int>             <dbl>              <dbl>
#> 1 Blue-tongued L…              10        59                 4           0.000017
#> # ℹ 5 more variables: patch_area_mean <dbl>, patch_area_total_ha <dbl>,
#> #   target_resolution <dbl>, data_resolution <dbl>, aggregation_factor <dbl>
```
