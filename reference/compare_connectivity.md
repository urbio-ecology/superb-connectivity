# Compare measurements the connectivity of different scenarios

We can measure the connectivity of a given habitat and barrier with
[`habitat_connectivity()`](https://urbio-ecology.github.io/urbioconnect/reference/habitat_connectivity.md).
We can also compare the connectivity, say for example if you have the
same area habitat and barrier, but you want to understand what the
change in connectedness is when you remove, or add some habitat, or some
barrier(s), or both. This function help you do that.

## Usage

``` r
compare_connectivity(area_new, area_baseline, interpatch_distance, species)
```

## Arguments

- area_new:

  Numeric vector. Area of a connected patch.

- area_baseline:

  Numeric vector. Baseline area of a connected patch.

- interpatch_distance:

  Numeric. The distance (in meters) where habitat patches are considered
  connected. E.g., if set to 500, patches 498m apart are connected,
  those 501m apart are not connected. This is passed internally to a
  spatial operation known as "buffering", where this distance is used as
  a radius from the edge of the habitat zone. This means the specified
  `interpatch_distance` is halved exactly. So an interpatch distance of
  500 will be converted to 250.

- species:

  name of species

## Value

tibble with "scenario", "interpatch_distance", "species", "n_patches",
"effective_mesh_ha", and "prob_connectedness".

## Examples

``` r
# for demonstration purposes - let's imagine the area decreases by 20%
baseline_areas <- round(lizard_areas_connected$area)
new_areas <- baseline_areas[-1] * 0.8
compare_connectivity(
  area_new = new_areas,
  area_baseline = baseline_areas,
  interpatch_distance = 10,
  species = "blue-tongued lizard"
)
#> # A tibble: 3 × 6
#>   scenario   interpatch_distance species             n_patches effective_mesh_ha
#>   <chr>                    <dbl> <chr>                   <int>             <dbl>
#> 1 baseline                    10 blue-tongued lizard        59              4.49
#> 2 new                         10 blue-tongued lizard        58              2.87
#> 3 difference                  10 blue-tongued lizard        -1             -1.62
#> # ℹ 1 more variable: prob_connectedness <dbl>
```
